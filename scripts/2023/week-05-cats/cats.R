# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(lubridate)
library(geosphere)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 5)

cats <- dat$cats_uk |>
  rename(
    lat = location_lat,
    lon = location_long
  )

# ✍️ fonts and palettes ------------------------------------------------------

bg <- "grey10"
txt <- "white"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "barlow")
showtext_auto()

ft <- "barlow"

pal <- c(
  # "Female - Neutered" = '#f3f3f3',
  "Female" = '#e6a3d0',
  "Male" = '#8be2af'
  # "Male" = '#7b8eaf'
    )

# 🤼 wrangle -----------------------------------------------------------------

calc_dist <- function(lon, lat, lon1, lat1) {
  map_dbl(1:length(lon), function(k) {
    drop(distm(c(lon[k], lat[k]), c(lon1[k], lat1[k]), fun = distGeo))
  })
}

df_base <- cats |>
  left_join(df_home, by = "tag_id") |>
  group_by(tag_id) |>
  mutate(
    hour = hour(timestamp),
    min = minute(timestamp),
    day_night = ifelse(hour > 6 & hour <= 18, "Day", "Night")
  ) |>
  arrange(tag_id, timestamp) |>
  mutate(
    lon1 = lag(lon),
    lat1 = lag(lat)
  ) |>
  drop_na() |>
  mutate(dx = calc_dist(lon, lat, lon1, lat1)/1000) |>
  group_by(tag_id, day_night) |>
  summarise(total_dist = sum(dx, na.rm = TRUE)) |>
  ungroup() |>
  select(tag_id, day_night, total_dist) |>
  pivot_wider(names_from = day_night, values_from = total_dist) |>
  left_join(
    dat$cats_uk_reference |>
      select(tag_id, animal_id, animal_sex, prey_p_month, animal_reproductive_condition, hrs_indoors),
    by = "tag_id"
  ) |>
  filter(animal_reproductive_condition != "Not fixed") |>
  mutate(sex = ifelse(animal_sex == "f", "Female", "Male")) |>
  filter(Day < 10, Night < 30)

df_top_6 <- df_base |>
  slice_max(prey_p_month, n = 10)

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
caption <- glue("{mastodon} @danoehm@{space}fosstodon.org • {twitter} @danoehm • {github} doehm/tidytues • {floppy} Movebank")

subtitle <- "Little bag of bones been out all night
Kitty you're scratchin' at the screen door
Kitty you're scratchin' at the screen door
Little bag of bones been out all night
He needs some pettin' and lovin' on his head
He needs some pettin' and lovin' on his rain-soaked hide
He's circlin' around my ankle
He's circlin' around my ankle
He needs some pettin' and lovin' on his hide
Kitty, won't you come inside?"

subtitle <- "Cats covered more distance during the night than during the day. On the hunt.
Point size indicates kills per month."

y_title <- "Night (6pm-6am)"

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_abline(slope = 1, colour = txt, linetype = 2) +
  geom_point(aes(Day, Night, size = prey_p_month, colour = sex), alpha = 0.5) +
  geom_text(aes(Day+0.2, Night+0.3, label = toupper(animal_id)), df_top_6, hjust = 0, family = ft, colour = txt, size = 12, lineheight = 0.35, fontface = "bold") +
  geom_text(aes(Day+0.2, Night-0.2, label = paste("Kill count:", round(prey_p_month))), df_top_6, hjust = 0, family = ft, colour = txt, size = 12, lineheight = 0.25) +
  annotate("text", x = 0, y = 25, label = y_title, hjust = 0, vjust = 1, family = ft, colour = txt, size = 16, lineheight = 0.35) +
  scale_size_identity() +
  scale_colour_manual(values = pal) +
  labs(
    title = "Little bag o' bones been out all night...",
    subtitle = subtitle,
    x = "Day (6am-6pm)",
    caption = caption,
    colour = "Sex"
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.title = element_text(size = 108, face = "bold", margin = margin(b=10), hjust = 0),
    plot.subtitle = element_text(margin = margin(b=0), hjust = 0),
    plot.background = element_rect(fill = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t=20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text = element_text(),
    axis.title.x = element_text(margin = margin(30, 10, 10, 10)),
    # axis.title.y = element_text(angle = 0, margin = margin(10, 30, 10, 10)),
    legend.position = "bottom"
  )

ggsave("scripts/2023/week 5 cats/cats.png", height = 12, width = 18)

