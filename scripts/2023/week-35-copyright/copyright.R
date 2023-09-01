# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 35)
cases <- dat$fair_use_cases
findings <- dat$fair_use_findings

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- "grey20"

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
ft <- "bar"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

# For fuck sake
# Rstudio replaced my code with an old version
# I'm not re-writing it

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "Data")

# 📊 plot --------------------------------------------------------------------

ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = y, ymax = y+0.5), df_base_1, fill = NA, colour = "black") +
  geom_segment(aes(x = z, xend = z, y = y, yend = y+0.5, group = id), df, alpha = 0.1) +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
  )

ggsave("scripts/2023/week-xx-yy/yy.png", height = 12, width = 12)
