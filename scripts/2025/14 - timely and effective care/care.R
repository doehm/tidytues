# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggforce)
library(geofacet)

# 💾 load data ---------------------------------------------------------------

care_state <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-08/care_state.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "grey20"
bg <- "white"
accent <- txt

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
autumn <- c('#1c2426', '#455852', '#9d8e71', '#d4935d', '#cc612b', '#a5371e', '#ab443b')
pal <- rev(c('#001219', '#005f73', '#0a9396', '#94d2bd', '#e9d8a6', '#ee9b00', '#ca6702', '#bb3e03', '#ae2012', '#9b2226')[-1])
pal <- rev(c('#587DB3', '#42BFDD', '#BBE6E4', '#F0F6F6', '#FF66B3'))

font_add_google("Raleway", "raleway", regular.wt = 200)
ft <- "raleway"
showtext_auto()

# 🚙 functions ---------------------------------------------------------------



# 🤼 wrangle -----------------------------------------------------------------

measures <- c(
  "Percentage of healthcare personnel who are up to date with COVID-19 vaccinations",
  "Healthcare workers given influenza vaccination Higher percentages are better"
)

df <- care_state |>
  filter(measure_name %in% measures) |>
  mutate(
    x0 = 0,
    y0 = 0,
    r = 1,
    start = 0,
    end = 2*pi*score/100,
    score = round(score)
  )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, bg)

# 📊 plot --------------------------------------------------------------------

plt_base <- ggplot() +
  xlim(0, 1) +
  ylim(0, 1) +
  labs(
    title = str_wrap("Most healthcare workers are given influenza vaccinations but very few are up to date with COVID-19 vaccinations", 50),
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5, margin = margin(b = 20), face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 70, r = 100, l = 100),
    legend.position = "none",
    strip.text = element_blank()
  )

plt_flu <- df |>
  filter(measure_name == measures[2]) |>
  ggplot() +
  geom_arc(aes(x0 = x0, y0 = y0, r = r, start = start, end = end, size = after_stat(index), colour = score), lineend = "round") +
  geom_text(aes(r*cos(end-pi/2), r*sin(end+pi/2), label = paste0(score, "%")), family = ft, colour = txt, size = 7, fontface = "bold") +
  geom_text(aes(x0, y0, label = state), family = ft, colour = txt, size = 16) +
  scale_size(range = c(0, 9)) +
  scale_colour_gradientn(colours = pal) +
  facet_geo(~state) +
  xlim(-1.25, 1.25) +
  ylim(-1.25, 1.25) +
  coord_equal(clip = "off") +
  labs(title = "Influenza") +
  theme_void() +
  theme(
    # plot.background = element_rect(fill = "grey90", colour = NA),
    plot.title = element_text(size = 84, hjust = 0, margin = margin(b = 20), family = ft),
    legend.position = "none",
    strip.text = element_blank()
  )

plt_covid <- df |>
  filter(measure_name == measures[1]) |>
  ggplot() +
  geom_arc(aes(x0 = x0, y0 = y0, r = r, start = start, end = end, size = after_stat(index), colour = score), lineend = "round") +
  geom_text(aes(r*cos(end-pi/2), r*sin(end+pi/2), label = paste0(score, "%")), family = ft, colour = txt, size = 7, fontface = "bold") +
  geom_text(aes(x0, y0, label = state), family = ft, colour = txt, size = 16) +
  scale_size(range = c(0, 9)) +
  scale_colour_gradientn(colours = pal) +
  facet_geo(~state) +
  xlim(-1.25, 1.25) +
  ylim(-1.25, 1.25) +
  coord_equal(clip = "off") +
  labs(title = "COVID-19") +
  theme_void() +
  theme(
    # plot.background = element_rect(fill = "grey95", colour = NA),
    plot.title = element_text(size = 84, hjust = 0, margin = margin(b = 20), family = ft),
    legend.position = "none",
    strip.text = element_blank()
  )

plt_base +
  inset_element(plt_flu, left = 0, right = 1, top = 1, bottom = 0.5) +
  inset_element(plt_covid, left = 0, right = 1, top = 0.5, bottom = 0) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave("scripts/2025/14 - timely and effective care/care.png", height = 24, width = 16)
