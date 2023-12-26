# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggfx)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 51)
episodes <- dat$holiday_episodes

# ✍️ fonts and palettes ------------------------------------------------------

the_simpsons <- c('#fccb00', '#1769b6', '#e44423', '#cddf8d', '#c6a168', '#54676a')
txt <- "black"
bg <- the_simpsons[1]
accent <- txt

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add("simpsons", regular = "assets/fonts/the-simpsons/Simpsonfont DEMO.otf")
font_add_google("Barlow", "bar")
ft <- "simpsons"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_base <- episodes |>
  filter(str_detect(parent_primary_title, "Simpsons")) |>
  mutate(lab = paste0(str_wrap(primary_title, 15), "\n", average_rating))

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  with_shadow(geom_point(aes(season_number, average_rating, size = num_votes), colour = the_simpsons[2]), colour = "black", x_offset = -10) +
  geom_text(aes(season_number, average_rating, label = lab), family = ft, colour = "grey10", size = 14, hjust = 0, lineheight = 0.3) +
  scale_size_binned(range = c(10, 30)) +
  coord_cartesian(clip = "off") +
  labs(
    title = "The Simpsons",
    subtitle = "holiday episodes",
    x = "Season",
    y = "IMDb\nrating",
    caption = caption
    ) +
  ylim(6, 9) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 256, hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_text(margin = margin(b = 15), hjust = 0.5, size = 96, face = "italic"),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20), family = "bar"),
    axis.line = element_line(),
    axis.title = element_text(margin = margin(r = 15, t = 15)),
    axis.text = element_text(),
    axis.ticks = element_line(),
    axis.ticks.length = unit(0.2, "cm"),
    plot.margin = margin(b = 50, t = 50, r = 125, l = 50),
    legend.position = "none"
  )

ggsave("scripts/2023/week-51-holiday-episodes/holiday-episodes.png", height = 12, width = 12)
