# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

palmtrees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

pal1 <- c('#4484e5', '#4ec7ea', '#ddebe7', '#faf6ea', '#d9e1f3', '#a8aa42')
pal2 <- c('#2c3510', '#334001', '#2d3a15', '#9f962d', '#f3f0e4', '#fcf9ee', '#77d4ed', '#3f83d7')

txt <- "grey20"
bg <- pal2[5]
accent <- pal2[3]

bg1 <- colorspace::darken(bg, 0.1)
bg2 <- pal2[5]

font_add_google("Raleway", "raleway", regular.wt = 200)
ft <- "raleway"
showtext_auto()

# 🚙 functions ---------------------------------------------------------------


# 🤼 wrangle -----------------------------------------------------------------

df <- palmtrees |>
  filter(max_stem_height_m > 0, max_stem_height_m <= 70) |>
  select(conspicuousness, max_leaf_number, max_stem_height_m) |>
  mutate(conspicuousness = str_to_title(conspicuousness)) |>
  drop_na()

df_consp <- df |>
  distinct(conspicuousness) |>
  mutate(
    x = 60,
    y = 75
  )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, pal2[4])

# 📊 plot --------------------------------------------------------------------

df |>
  ggplot(aes(max_stem_height_m, max_leaf_number)) +
  geom_point(alpha = 0.3, colour = pal2[3], pch = "/", size = 15) +
  geom_smooth(method = "lm", colour = pal2[4], fill = pal2[4]) +
  scale_x_continuous(breaks = seq(0, 60, 10), labels = paste0(seq(0, 60, 10), "m")) +
  scale_y_continuous(breaks = seq(0, 60, 10), labels = seq(0, 60, 10)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "PALM TREES",
    subtitle = "On average, the taller the palm, the more leaves it has",
    caption = caption,
    x = "Max height",
    y = "Max leaves"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5, margin = margin(b = 20)),
    plot.subtitle = element_text(margin = margin(b = 40), hjust = 0.5),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 60)),
    plot.margin = margin(b = 20, t = 50, r = 100, l = 50),
    panel.background = element_rect(fill = grid::radialGradient(colours = c(bg2, bg1)), colour = NA),
    strip.text = element_blank(),
    axis.text = element_text(margin = margin(r = 20, t = 20)),
    axis.title.x = element_text(margin = margin(t = 20)),
    axis.title.y = element_text(angle = 90, margin = margin(r = 20))
  )

ggsave("scripts/2025/11-palm trees/palm-trees.png", height = 12, width = 12)
