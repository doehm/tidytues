# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggchicklet)

# 💾 load data ---------------------------------------------------------------

conf2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2023.csv')
conf2024 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-14/conf2024.csv')

# ✍️ fonts and palettes ------------------------------------------------------


font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "grey20"
bg <- "white"
sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
font_add_google("Poppins", "pop", regular.wt = 200)
ft <- "pop"
showtext_auto()

# 🚙 functions ---------------------------------------------------------------

first_name <- function(x) {
  str_sub(x, 1, 1)
}

last_name <- function(x) {
  map_chr(x, ~{
    str_split_1(.x, " ") |>
      tail(1) |>
      str_sub(1,1)
  })
}

# 🤼 wrangle -----------------------------------------------------------------

df <- conf2023 |>
  mutate(year = 2023) |>
  bind_rows(
    conf2024 |>
      mutate(year = 2024)
  ) |>
  select(year, speaker_name) |>
  mutate(
    `First name` = first_name(speaker_name),
    `Last name` = last_name(speaker_name)
  ) |>
  select(-speaker_name) |>
  pivot_longer(-year, names_to = "name", values_to = "val") |>
  group_by(year, name, val) |>
  mutate(n = 1:n())

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(txt)
title <- "posit::conf()"
subtitle <- str_wrap("Distribution of the first letter of the speaker's first and last name.
2023 was the year of the J's and M's. 2024 was the year of the A's. There has yet
to be a speaker with a name starting with a Q, X, or Y.", 110)

# 📊 plot --------------------------------------------------------------------

expand_grid(
  year = c(2023, 2024),
  name = c("First name", "Last name"),
  val = LETTERS
  ) |>
  left_join(df, join_by(year, name, val)) |>
  ggplot() +
  geom_tile(aes(val, n, fill = n), width = 0.9, height = 0.9) +
  scale_fill_gradientn(colours = sunset) +
  facet_grid(year ~ name) +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = NA),
    plot.title = element_text(size = 128, hjust = 0.5, face = "bold", margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0, margin = margin(b = 20), size = 48),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "none",
    panel.spacing = unit(1, "cm"),
    axis.text.x = element_text(margin = margin(t = 5)),
    strip.text = element_text(size = 48)
  )

ggsave("scripts/2025/02-posit-conf/posit-conf.png", height = 10, width = 12)

