# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggforce)
library(ggfx)
library(ggbump)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 46)
diwali <- dat$diwali_sales_data |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

pal <-colorRampPalette(c("#4a166a", "#ce5068", "#f0d35b", "#61aa6f", "#2f3c68"))(18)
txt <- "white"
bg <- "grey10"
accent <- "white"

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Karla", "karla")
font_add_google("Pacifico", "pac")
ft1 <- "pac"
ft <- "karla"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

id <- gs4_find("Shapes")

df_base <- diwali |>
  group_by(zone, product_category) |>
  summarise(
    n = n(),
    n_cust = n_distinct(user_id),
    n_orders = sum(orders, na.rm = TRUE),
    amount = sum(amount, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    y0 = as.numeric(factor(zone)),
    x0 = as.numeric(factor(product_category)),
    globe = paste(zone, product_category),
    y_amount = as.numeric(cut(amount, 3)),
    join = 1
    )

df_cord <- df_base |>
  mutate(
    x = x0 + a*0.5,
    y_amount = as.numeric(cut(amount, 3)),
    y = 2*y0 - y_amount/3
  )

df_gender <- diwali |>
  group_by(zone, product_category, gender) |>
  summarise(
    n_gender = n_distinct(user_id),
    .groups = "drop") |>
  left_join(
    df_base |>
      distinct(zone, product_category, join, y_amount),
    by = c("zone", "product_category")
  ) |>
  ungroup() |>
  mutate(
    y0 = as.numeric(factor(zone)),
    x0 = as.numeric(factor(product_category)),
    x = a*ifelse(gender == "M", 0.45, 0.55) + x0,
    y = a*0.18 + 2*y0 - y_amount/3,
    theta = ifelse(gender == "F", 10, 350),
    just = ifelse(gender == "F", 0, 1)
  )

df_globe <- read_shapes(id, "globe") |>
  mutate(
    x = min_max(x, 0.16, 0.84),
    y = min_max(y, 0, 0.9),
    join = 1
  ) |>
  left_join(df_base, by = "join", relationship = "many-to-many") |>
  group_by(globe) |>
  mutate(
    theta = runif(1, 0, 10),
    x = x*a + x0,
    y = y*a + 2*y0 - y_amount/3
  )

df_globe_top <- read_shapes(id, "globe top") |>
  mutate(
    y = min_max(y, 0.75, 1),
    x = min_max(x, 0.28, 0.72),
    join = 1
  ) |>
  left_join(df_base, by = "join", relationship = "many-to-many") |>
  group_by(globe) |>
  mutate(
    theta = runif(1, 0, 10),
    x = x*a + x0,
    y = y*a + 2*y0 - y_amount/3
  )

df_filament <- tribble(
  ~x, ~xend, ~y, ~yend, ~group,
  0.45, 0.35, 0.75, 0.25, 1,
  0.55, 0.65, 0.75, 0.25, 2,
  0.5, 0.5, 0.75, 0.22, 3
) |>
  mutate(join = 1) |>
  left_join(df_base, by = "join", relationship = "many-to-many") |>
  group_by(globe) |>
  mutate(
    theta = runif(1, 0, 10),
    x = x*a + x0,
    y = y*a + 2*y0 - y_amount/3,
    xend = xend*a + x0,
    yend = yend*a + 2*y0 - y_amount/3
  )



a <- 0.8

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(c("#ce5068", "#f0d35b", "#61aa6f", "#2f3c68"))

title <- "Diwali\nSales"

df_lab <- df_base |>
  filter(y0 == 1) |>
  distinct(product_category, x0)

df_zone <- df_base |>
  filter(x0 == 1) |>
  distinct(zone, y0)

# 📊 plot --------------------------------------------------------------------

df_globe |>
  ggplot() +
  geom_bump(aes(x, y+0.8, group = zone), df_cord, linewidth = 2) +
  with_outer_glow(geom_bspline_closed0(aes(x, y, group = globe, fill = product_category)), colour = "grey80", sigma = 10) +
  # geom_bspline_closed0(aes(x, y, group = globe, fill = product_category)) +
  geom_polygon(aes(x, y, group = globe), df_globe_top, fill = "black") +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend, group = paste(globe, group)), df_filament, alpha = 0.5) +
  geom_text(aes(x, y, label = n_gender), df_gender, family = ft, size = 6, alpha = 0.75, angle = df_gender$theta, hjust = df_gender$just) +
  geom_text(aes(x0+0.45, 11, label = str_wrap(product_category, 10)), df_lab,  family = ft, size = 10, colour = txt, lineheight = 0.3, vjust = 0) +
  geom_text(aes(19, 2*y0, label = zone), df_zone, family = ft, size = 10, colour = txt, lineheight = 0.3, hjust = 0) +
  annotate("text", x = -2, y = 9, label = title, lineheight = 0.3, family = ft1, size = 80, angle = 20, colour = txt) +
  annotate("from_path", x = -2, y = 3, path = "scripts/2023/week-46-diwali/legend1.png", width = 0.25) +
  coord_fixed(clip = "off") +
  scale_fill_manual(values = pal) +
  xlim(-2, 19) +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 150),
    legend.position = "none"
  )

ggsave("scripts/2023/week-46-diwali/diwali.png", height = 12, width = 21)

