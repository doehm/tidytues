# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(maps)
library(sf)
library(rgeoboundaries)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 34)
df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-08-22/population.csv')

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- "grey20"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
ft <- "bar"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

world_center <- map_data("world") |>
  group_by(region) |>
  summarise(
    long = mean(long),
    lat = mean(lat)
  )

df |>
  filter(coa_name == "Australia") |>
  ggplot() +
  geom_line(aes(year, refugees, group = coo_name))

world_1 <- st_as_sf(world_1, coords = c('long', 'lat'))

ggplot() +
  geom_sf(data = world_1) +
  coord_sf()

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "Data")

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
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
