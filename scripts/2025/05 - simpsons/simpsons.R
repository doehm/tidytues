# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(shadowtext)

# 💾 load data ---------------------------------------------------------------

simpsons_characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_characters.csv')
simpsons_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_episodes.csv')
simpsons_locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_locations.csv') |>
  rename(location = normalized_name)
simpsons_script_lines <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-04/simpsons_script_lines.csv') |>
  rename(words = normalized_text)


# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add("simpsons", regular = "assets/fonts/the-simpsons/Simpsonfont DEMO.otf")

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
autumn <- c('#1c2426', '#455852', '#9d8e71', '#d4935d', '#cc612b', '#a5371e', '#ab443b')
simpsons <- c('#fad225', '#0388f4', '#fdfdfb', '#392d12')

txt <- simpsons[1]
bg <- simpsons[2]
accent <- txt

font_add_google("Poppins", "pop", regular.wt = 200)
ft <- "pop"
ft1 <- "simpsons"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

.n <- 12

df_words <- simpsons_script_lines |>
  count(words, sort = TRUE) |>
  drop_na() |>
  head(.n) |>

df_location <- simpsons_script_lines |>
  left_join(simpsons_locations, join_by(location_id == id)) |>
  count(location, sort = TRUE) |>
  drop_na() |>
  head(.n)

df <- simpsons_script_lines |>
  left_join(simpsons_locations, join_by(location_id == id)) |>
  semi_join(df_words, join_by(words)) |>
  semi_join(df_location, join_by(location)) |>
  count(location, words, sort = TRUE)


# 🔡 text --------------------------------------------------------------------

caption <- make_caption(simpsons[4], simpsons[3])
title <- "THE SIMPSONS"
subtitle <- glue("The top {.n} most spoken phrases in the most referrenced locations")

# 📊 plot --------------------------------------------------------------------

df |>
  mutate(location = str_wrap(location, 10)) |>
  ggplot() +
  geom_shadowtext(aes(location, words, label = str_to_sentence(words), size = 4*n+12), family = ft1, colour = txt, angle = rnorm(nrow(df), 360, 5)) +
  scale_x_discrete(position = "top") +
  scale_size_identity() +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = simpsons[3]),
    plot.background = element_rect(fill = grid::linearGradient(
      colours = simpsons[2:3],
      x1 = unit(0, "npc"),
      y1 = unit(1, "npc"),
      x2 = unit(0, "npc"),
      y2 = unit(0, "npc"))),
    plot.title = element_shadowtext(size = 220, hjust = 0.5, family = ft1, margin = margin(b = 20)),
    plot.subtitle = element_text(margin = margin(b = 50, t = 40), hjust = 0.5),
    plot.caption = element_markdown(colour = simpsons[4], hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),

    axis.text.x = element_shadowtext(colour = simpsons[3], family = ft1, margin = margin(b = 20)),
    legend.position = "bottom"
  )

ggsave("scripts/2025/05 - simpsons/simpsons.png", height = 12, width = 16)
