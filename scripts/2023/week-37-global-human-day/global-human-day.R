# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(colorspace)

# 💾 load data ---------------------------------------------------------------

all_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/all_countries.csv') |>
  clean_names()
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/country_regions.csv') |>
  clean_names()
global_human_day <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_human_day.csv') |>
  clean_names()
global_economic_activity <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-12/global_economic_activity.csv') |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
txt1 <- "white"
bg <- "white"
accent <- "grey20"

pal <- c('#4a166a', '#ce5068', '#f0d35b', '#61aa6f', "#2f3c68")
pal <- colorRampPalette(pal)(8)
show_pal(pal)

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
ft <- "bar"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

lighten_amount <- seq(0, 1, length = 7)[2:6]

df_cats <- all_countries |>
  distinct(category, subcategory)

df_global <- global_human_day |>
  left_join(df_cats, by = "subcategory") |>
  arrange(category, desc(hours_per_day)) |>
  mutate(
    hrs = cumsum(hours_per_day),
    x = ifelse(row_number() == 1, 0, lag(hrs)) + hours_per_day/2,
    xmin = ifelse(row_number() == 1, 0, lag(hrs)),
    xmax = hrs,
    ymin = 0,
    ymax = 0.2,
    cat_num = as.numeric(factor(category)),
    col = pal[cat_num],
    y_lab = breathing_space_on_x(x, 1.3, -0.1, 0.22),
    sub_lab = str_wrap(subcategory, 15),
    hrs_lab = paste0(str_wrap(subcategory, 10), "\n", to_hm(hours_per_day*3600)),
    hrs_lab = to_hm(hours_per_day*3600),
    y_hrs = breathing_space_on_x(x, 0.5, 0.126, 0.06)
  ) |>
  group_by(category) |>
  mutate(
    subcat_num = 1:n(),
    sub_col = lighten(col, lighten_amount[subcat_num])
    ) |>
  ungroup()

df_global_cat <- global_human_day |>
  left_join(df_cats, by = "subcategory") |>
  group_by(category) |>
  summarise(
    hours_per_day = sum(hours_per_day)
  ) |>
  mutate(
    hrs = cumsum(hours_per_day),
    x = ifelse(row_number() == 1, 0, lag(hrs)) + hours_per_day/2,
    xmin = ifelse(row_number() == 1, 0, lag(hrs)),
    xmax = hrs,
    ymin = 0.21,
    ymax = 0.41,
    cat_num = as.numeric(factor(category)),
    col = pal[cat_num],
    y_lab = breathing_space_on_x(x, 1.2, 0.55, -0.22),
    cat_lab = str_wrap(category, 15),
    hrs_lab = to_hm(hours_per_day*3600)
  )

# australia
a <- 0.2
df_aus <- all_countries |>
  filter(country_iso3 == "AUS") |>
  rename(hours_per_day = hours_per_day_combined) |>
  left_join(
    global_human_day |>
      select(subcategory, mean = hours_per_day),
      by = "subcategory"
  ) |>
  arrange(category, desc(hours_per_day)) |>
  mutate(
    change = hours_per_day - mean,
    hrs = cumsum(hours_per_day),
    x = ifelse(row_number() == 1, 0, lag(hrs)) + hours_per_day/2,
    xmin = ifelse(row_number() == 1, 0, lag(hrs)),
    xmax = hrs,
    ymin = -1.8-a,
    ymax = -1.6-a,
    cat_num = as.numeric(factor(category)),
    col = pal[cat_num],
    hrs_lab = to_hm(hours_per_day*3600),
    change_lab = to_hm(abs(change)*3600),
    change_lab = ifelse(change < 0, paste0("-", change_lab), paste0("+", change_lab)),
    y_lab = breathing_space_on_x(x, 0.6, -1.9-a, 0.22),
  ) |>
  group_by(category) |>
  mutate(
    subcat_num = 1:n(),
    sub_col = lighten(col, lighten_amount[subcat_num])
  ) |>
  ungroup()

df_aus_cat <- all_countries |>
  filter(country_iso3 == "AUS") |>
  rename(hours_per_day = hours_per_day_combined) |>
  left_join(
    global_human_day |>
      select(subcategory, mean = hours_per_day),
    by = "subcategory"
  ) |>
  group_by(category) |>
  summarise(
    hours_per_day = sum(hours_per_day),
    mean = sum(mean)
    ) |>
  mutate(
    change = hours_per_day - mean,
    hrs = cumsum(hours_per_day),
    x = ifelse(row_number() == 1, 0, lag(hrs)) + hours_per_day/2,
    xmin = ifelse(row_number() == 1, 0, lag(hrs)),
    xmax = hrs,
    ymin = -1.59-a,
    ymax = -1.39-a,
    cat_num = as.numeric(factor(category)),
    col = pal[cat_num],
    y_lab = breathing_space_on_x(x, 0.5, -1.24-a, -0.22),
    hrs_lab = to_hm(hours_per_day*3600),
    change_lab = to_hm(abs(change)*3600),
    change_lab = ifelse(change < 0, paste0("-", change_lab), paste0("+", change_lab)),
  )

df_global_cat <- df_global_cat |>
  bind_rows(df_aus_cat)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "Data")

title_bar <- tribble(
  ~x, ~y, ~title,
  0, -1, "Global average",
  0, min(df_aus$y_lab), "Australia"
)

title <- "GLOBAL HUMAN DAY: HOW WE SPEND OUR TIME"
subtitle <-
"           Global Human Day estimates how all humans spend their time on any given day using a generalized, physical outcome–based categorisation that facilitates the integration of data from hundreds of diverse datasets.
Compared with the global average, Australians get less sleep, spend less time on personal hygiene, less time provisioning food, and more time commuting."

# 📊 plot --------------------------------------------------------------------

pt_size <- 2
pt_dist <- 0.04
txt_dist <- 0.07

df_global |>
  bind_rows(df_aus) |>
  ggplot() +

  # shading rect
  annotate("rect", xmin = -0.5, xmax = 24.5, ymin = -1.15, ymax = 1, fill = "grey95") +

  # title text
  geom_text(aes(x = x, y = y, label = toupper(title)), title_bar, family = ft, colour = txt, size = 32, vjust = 0, hjust = 0, fontface = "bold") +

  # little lines down
  geom_segment(aes(x = x, xend = x, y = ymin, yend = y_lab+pt_dist, colour = sub_col)) +
  geom_point(aes(x = x, y = ymin, colour = sub_col), size = pt_size) +
  geom_point(aes(x = x, y = y_lab+pt_dist, colour = sub_col), size = pt_size) +

  # little lines up
  geom_segment(aes(x = x, xend = x, y = ymax, yend = y_lab-pt_dist, colour = col), df_global_cat) +
  geom_point(aes(x = x, y = ymax, colour = col), df_global_cat, size = pt_size) +
  geom_point(aes(x = x, y = y_lab-pt_dist, colour = col), df_global_cat, size = pt_size) +

  # text
  geom_text(aes(x = x, y = y_lab-txt_dist, label = sub_lab), family = ft, size = 10, colour = txt, vjust = 1, lineheight = 0.3) +
  geom_text(aes(x = x, y = y_lab, label = hrs_lab), family = ft, size = 12, colour = txt, vjust = 1, lineheight = 0.3, fontface = "bold") +

  # cat text
  geom_text(aes(x = x, y = y_lab+txt_dist, label = cat_lab), df_global_cat, family = ft, size = 10, colour = txt, hjust = 0.5, lineheight = 0.3, vjust = 0) +
  geom_text(aes(x = x, y = y_lab, label = hrs_lab), filter(df_global_cat, !is.na(cat_lab)), family = ft, size = 12, colour = txt, hjust = 0.5, lineheight = 0.3, vjust = 0, fontface = "bold") +
  geom_text(aes(x = x, y = y_lab+txt_dist, label = hrs_lab), df_aus_cat, family = ft, size = 12, colour = txt, hjust = 0.5, lineheight = 0.3, vjust = 0, fontface = "bold") +

  # change text
  geom_text(aes(x = x, y = y_lab-txt_dist, label = change_lab), family = ft, size = 10, colour = txt, vjust = 1, lineheight = 0.3) +
  geom_text(aes(x = x, y = y_lab, label = change_lab), df_aus_cat, family = ft, size = 10, colour = txt, vjust = 0, lineheight = 0.3) +

  # stacked bars
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = sub_col)) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = col), df_global_cat) +

  scale_fill_identity() +
  scale_colour_identity() +

  ylim(min(df_aus$y_lab-txt_dist), 1) +
  coord_cartesian(clip = "off") +
  labs(
    caption = caption,
    title = title,
    subtitle = subtitle
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 150, hjust = 0.14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.1),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 0, l = 0)
  )

ggsave("scripts/2023/week-37-global-human-day/global-human-day.png", height = 12, width = 24)

