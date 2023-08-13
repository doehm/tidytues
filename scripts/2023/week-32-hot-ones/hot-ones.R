# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 32)

attach(dat)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "#cad2c5"
accent <- '#230827'

hot_sauce <- c('#ff813c', '#fe5454', '#7b265d', '#4a1647', '#230827')

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Advent Pro", "advent")
font_add_google("Passion One", "passion")
font_add_google("Bangers", "bang")
ft <- "advent"
ft1 <- "bang"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_base <- sauces |>
  mutate(l_scov = log(scoville))

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "Hot Ones")

chilli <- "<span style='font-family:fa-solid'>&#xf816;</span>"

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_richtext(aes(sauce_number, -season, label = chilli, colour = l_scov), label.colour = NA, size = 32, fill = NA) +
  geom_text(aes(0, -season, label = season), family = ft, colour = txt, size = 16) +
  geom_text(aes(sauce_number, 0, label = sauce_number), family = ft, colour = txt, size = 16) +
  annotate("text", x = 0, y = 0, label = "Season", family = ft, colour = txt, size = 16, hjust = 1) +
  annotate("text", x = 1, y = 1, label = "Sauce number", family = ft, colour = txt, size = 16, hjust = 0) +
  scale_colour_gradientn(
    colours = hot_sauce,
    breaks = log(c(2000, 20000, 200000, 2000000)),
    labels = c("2k", "20k", "200k", "2M")
    ) +
  labs(
    title = "HOT ONES!",
    caption = caption,
    colour = "Scoville units"
    ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(family = ft1, size = 220, hjust = 0.5, face = "italic"),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20), face = "italic"),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "bottom"
  )

ggsave("scripts/2023/week-32-hot-ones/hot-ones.png", height = 12, width = 8)
