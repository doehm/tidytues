# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggchicklet)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 16)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey90"
bg <- "grey10"
accent <- "grey90"

df_pal <- tribble(
  ~category, ~edibility, ~fill,
  "Wild plants", "Edible",     "#65dbff",
  "Wild plants", "Not edible", '#386876',
  "Pulses",     "Edible",     "#a9a9ff",
  "Pulses",     "Not edible", '#545476',
  "Grasses",      "Edible",     "#a0ffc7",
  "Grasses",      "Not edible", '#507660',
  "Fruits/nuts", "Edible",     "#ff76ae",
  "Fruits/nuts", "Not edible", "#9D0159"
)

pal <- df_pal |>
  filter(edibility == "Edible") |>
  pull(fill)
names(pal) <- c("Wild plants", "Pulses", "Grasses", "Fruits/nuts")


font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
showtext_auto()

ft <- "bar"

# 🤼 wrangle -----------------------------------------------------------------

df_cat <- dat$founder_crops |>
  group_by(category) |>
  summarise(n = sum(prop)) |>
  ungroup() |>
  drop_na() |>
  mutate(p = n/sum(n)-0.001) |>
  arrange(desc(category)) |>
  mutate(
    y = cumsum(p)-p/2,
    xend = c(8, 6, 4, 2)-1,
    category = as.factor(category),
    pct = paste0(round(100*p, 0), "%")
  )

df_edible <- dat$founder_crops |>
  mutate(edibility = ifelse(is.na(edibility), "Not edible", "Edible")) |>
  group_by(category, edibility) |>
  summarise(n = sum(prop)) |>
  group_by(category) |>
  drop_na() |>
  left_join(df_pal, by = c("category", "edibility")) |>
  mutate(
    p = n/sum(n),
    edibility = factor(edibility, levels = c("Not edible", "Edible"))
  ) |>
  left_join(
    df_cat |>
      select(category, y),
    by = "category"
  ) |>
  mutate(
    category = as.factor(category)
    )

df_perc <- df_edible |>
  filter(edibility == "Edible") |>
  mutate(
    p_mid = p/2,
    pct = paste0(round(p*100, 0), "%")
    )

dat$founder_crops |>
  mutate(edibility = ifelse(is.na(edibility), "Not edible", "Edible")) |>
  group_by(edibility) |>
  summarise(n = sum(prop)) |>
  ungroup() |>
  mutate(p = n/sum(n))

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}Neolithic Founder Crops")

subtitle <- glue("Breakdown of crops from the first farmers into 4 broad categories.<br>
<span style='color:{pal[3]}'>57%</span>are <span style='color:{pal[3]}'>grasses</span> and only <span style='color:{pal[4]}'>8%</span> were <span style='color:{pal[4]}'>fruit & nuts</span>. Overall 84% of the<br>
crops were edible primarily from <span style='color:{pal[2]}'>pulses</span>, <span style='color:{pal[3]}'>grasses</span> and <span style='color:{pal[4]}'>fruit & nuts</span>")

# 📊 plot --------------------------------------------------------------------

g_cat <- ggplot() +
  geom_col(aes(10, p, fill = category), df_cat, colour = NA, width = 1.4) +
  geom_segment(aes(x = 10, xend = xend, y = y, yend = y, colour = category), df_cat, linetype = 3) +
  geom_text(aes(11, y = y, label = str_replace(category, "/", " & "), colour = category), df_cat, family = ft, size = 20) +
  geom_point(aes(10.7, y = y, colour = category), df_cat, size = 5) +
  geom_text(aes(10, y = y, label = pct), df_cat, family = ft,  colour = bg, size = 32) +
  annotate("segment", x = 0.2, xend = 7.5, y = -0.1, yend = -0.1, colour = txt) +
  annotate("richtext", x = 3.9, y = -0.1, label = "Edibility", family = ft, size = 20, colour = txt, angle = 90, label.colour = NA, fill = bg) +
  xlim(0, 11.5) +
  ylim(-0.20, 1.2) +
  coord_flip() +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    caption = caption,
    title = "What did the first farmers farm\nand how much was edible?",
    subtitle = subtitle
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.4, colour = txt),
    plot.title = element_text(size = 128, hjust = 0.5, lineheight = 0.3),
    plot.subtitle = element_markdown(halign = 0, hjust = 0.5, size = 56, margin = margin(t=25, b=-25)),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "none"
  )

g_edible <- ggplot() +
  geom_col(aes(category, p), df_edible, width = 0.4, fill = df_edible$fill) +
  geom_point(aes(x = as.numeric(category)+0.2, y = y, colour = category), df_edible, size = 5) +

  # text
  geom_text(aes(category, p_mid, label = pct), df_perc, family = ft, colour = bg, size = 32) +

  ylim(-0.2, 1.2) +
  coord_flip() +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    legend.position = "none"
  )

g_cat +
  inset_element(g_edible, left = 0, right = 1, bottom = 0, top = 0.7) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave("scripts/2023/week-16-crops/crops.png", height = 20, width = 12)

