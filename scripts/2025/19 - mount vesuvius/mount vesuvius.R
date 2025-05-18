# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

vesuvius <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-13/vesuvius.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "grey20"
bg <- "white"
accent <- txt

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
autumn <- c('#1c2426', '#455852', '#9d8e71', '#d4935d', '#cc612b', '#a5371e', '#ab443b')

font_add_google("Raleway", "raleway", regular.wt = 200)
font_add_google("Josefin Sans", "jose", regular.wt = 200, bold.wt = 700)
ft <- "raleway"
ft <- "jose"
showtext_auto()

# 🚙 functions ---------------------------------------------------------------



# 🤼 wrangle -----------------------------------------------------------------

df <- vesuvius |>
  drop_na()

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, bg)

x <- c(0.1, 0.25, 0.5, 1)

# 📊 plot --------------------------------------------------------------------

df |>
  ggplot(aes(log(depth_km), duration_magnitude_md)) +
  stat_density_2d(aes(fill = after_stat(level)), geom = "polygon", alpha = 0.5, linewidth = 0.25, colour = NA, n = 200) +
  scale_fill_gradientn(colours = pencil_case$spec$div) +
  scale_x_continuous(breaks = log(x), labels = glue("{x*1000}m"), ) +
  labs(
    title = "Seismic Events at\nMount Vesuvius",
    subtitle = "On average events are recorded at a depth of 240m (median)",
    x = "Depth",
    y = "Duration\nmagnitude",
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    panel.background = element_rect(fill = "grey95", colour = NA),
    plot.title = element_text(size = 128, hjust = 0.5, margin = margin(b = 30), face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 20)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 55)),
    plot.margin = margin(b = 20, t = 75, r = 150, l = 75),
    axis.text = element_text(margin = margin(t = 10, r = 10)),
    axis.ticks = element_line(linewidth = 0.25),
    axis.ticks.length = unit(0.25, "cm"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10), angle = 90),
    legend.position = "none"
  )

ggsave("scripts/2025/19 - mount vesuvius/mount vesuvius.png", height = 12, width = 12)
