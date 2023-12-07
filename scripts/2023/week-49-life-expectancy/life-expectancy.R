# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggbump)
library(RColorBrewer)
library(ggnewscale)

# 💾 load data ---------------------------------------------------------------

life_expectancy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-12-05/life_expectancy.csv') |>
  clean_names()

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

df_bump <- life_expectancy |>
  filter(
    year %in% c(1970, 2020),
    !is.na(code)
  ) |>
  group_by(year) |>
  mutate(rank = rank(life_expectancy)) |>
  group_by(entity) |>
  mutate(
    diff = rank[year == 2020] - rank[year == 1970],
    min_le = life_expectancy[year == 1970],
    x_text = ifelse(year == 1970, 1968, 2022),
    lab = ifelse(year == 1970, paste0(entity, " (", round(life_expectancy), ")"), paste0("(", round(life_expectancy), ") ", entity)),
    lab1 = ifelse(year == 1970, paste0(entity, " (", 237 - rank + 1, ")"), paste0("(", 237 - rank + 1, ") ", entity)),
    just = ifelse(year == 1970, 1, 0)
  )

df_tile <- life_expectancy |>
  filter(
    year >= 1970,
    !is.na(code)
  ) |>
  group_by(entity) |>
  mutate(min_le = life_expectancy[year == 1970]) |>
  ungroup() |>
  mutate(entity = fct_reorder(entity, min_le))

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)
title <- "L\nI\nF\nE\n\nE\nX\nP\nE\nC\nT\nA\nN\nC\nY"
years <- tibble(
  x = c(1970, 2020),
  y = 239
)

# 📊 plot --------------------------------------------------------------------

g_bump <- df_bump |>
  ggplot() +
  geom_bump(aes(year, rank, group = entity, colour = diff), linewidth = 0.8) +
  geom_point(aes(year, rank, group = entity, colour = diff), size = 1) +
  geom_text(aes(x_text, rank, label = lab1), hjust = df_bump$just, size = 6, colour = txt) +
  geom_text(aes(x, y, label = x), years, family = ft, size = 12, colour = txt) +
  annotate("text", x = 1995, y = 239, label = "Rank change from 1970-2020", family = ft, size = 16,
           colour = txt, lineheight = 0.3, vjust = 0) +
  scale_colour_gradient2(low = "#e63946", mid = "#f1faee", high = "#457b9d", midpoint = 0) +
  coord_cartesian(clip = "off") +
  labs(colour = "Rank change") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.title = element_text(),
    plot.background = element_rect(fill = bg, colour = bg),
    legend.box.margin = margin(t = -55),
    legend.position = "bottom"
  )


g_tile <- df_tile |>
  ggplot() +
  geom_tile(aes(year, entity, fill = life_expectancy)) +
  geom_text(aes(x_text, entity, label = lab), df_bump, hjust = df_bump$just, size = 6, colour = txt) +
  geom_text(aes(x, y, label = x), years, family = ft, size = 12, colour = txt) +
  scale_fill_viridis_c(option = "A", direction = 1) +
  annotate("text", x = 1995, y = 239, label = "Life expectancy has increased by 13.7 years\nglobally from 1970-2020",
           family = ft, size = 16, colour = txt, vjust = 0, lineheight = 0.3) +
  coord_cartesian(clip = "off") +
  labs(fill = "Life expectancy") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    legend.position = "bottom"
  )

g_title <- ggplot() +
  annotate("text", x = 0.5, y = 1, label = title, vjust = 1, lineheight = 0.3, family = ft, fontface = "bold", size = 64) +
  xlim(0, 1) +
  ylim(0, 1) +
  coord_cartesian(clip = "off") +
  theme_void()

g_title +
  inset_element(g_tile, left = 0.05, right = 0.4, top = 1, bottom = 0) +
  inset_element(g_bump, left = 0.6, right = 0.95, top = 1.045, bottom = 0) +
  plot_annotation(
    caption = caption,
    theme = theme(
      text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
      plot.background = element_rect(fill = bg, colour = bg),
      plot.title = element_text(size = 128, hjust = 0.5),
      plot.subtitle = element_text(),
      plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
      plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
    )
  )

ggsave("scripts/2023/week-49-life-expectancy/life-expectancy.png", height = 18, width = 18)



# 📊 Australia ------------------------------------------------------------

g_bump <- df_bump |>
  ggplot() +
  geom_bump(aes(year, rank, group = entity, colour = diff), alpha = 0.5) +
  geom_point(aes(year, rank, group = entity, colour = diff), size = 1, alpha = 0.5) +

  # Aus
  geom_bump(aes(year, rank, group = entity), filter(df_bump, code == "AUS"), colour = "#457b9d", linewidth = 1) +
  geom_point(aes(year, rank, group = entity), filter(df_bump, code == "AUS"), size = 2, colour = "#457b9d") +

  geom_text(aes(x_text, rank, label = lab1), hjust = df_bump$just, size = 6, colour = txt) +
  geom_text(aes(x, y, label = x), years, family = ft, size = 12, colour = txt) +
  annotate("text", x = 1995, y = 239, label = "Australia ranked 33rd in the world in 1970 and\nincreased to 5th in 2020.", family = ft, size = 16,
           colour = txt, lineheight = 0.3, vjust = 0) +
  scale_colour_gradient2(low = "grey30", mid = "grey95", high = "grey30", midpoint = 0) +
  coord_cartesian(clip = "off") +
  labs(colour = "Rank change") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.title = element_text(),
    plot.background = element_rect(fill = bg, colour = bg),
    legend.box.margin = margin(t = -55),
    legend.position = "bottom"
  )


g_tile <- df_tile |>
  ggplot() +
  geom_tile(aes(year, entity, fill = life_expectancy)) +
  geom_text(aes(x_text, entity, label = lab), df_bump, hjust = df_bump$just, size = 6, colour = txt) +
  geom_text(aes(x, y, label = x), years, family = ft, size = 12, colour = txt) +
  annotate("text", x = 1995, y = 239, label = "Australia's life expectancy was 70.7 years in 1970\nwhich increased to 84.7 in 2020",
           family = ft, size = 16, colour = txt, vjust = 0, lineheight = 0.3) +
  scale_fill_viridis_c(option = "A", direction = 1) +
  labs(fill = "Life expectancy") +

  # Aus
  new_scale_fill() +
  geom_tile(aes(year, entity, fill = life_expectancy), filter(df_tile, code != "AUS")) +
  scale_fill_gradient(low = "grey40", high = "grey95") +

  coord_cartesian(clip = "off") +
  labs(fill = "Life expectancy") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    legend.position = "bottom"
  )

g_title <- ggplot() +
  annotate("text", x = 0.5, y = 1, label = title, vjust = 1, lineheight = 0.3, family = ft, fontface = "bold", size = 64) +
  xlim(0, 1) +
  ylim(0, 1) +
  coord_cartesian(clip = "off") +
  theme_void()

g_title +
  inset_element(g_tile, left = 0.05, right = 0.4, top = 1, bottom = 0) +
  inset_element(g_bump, left = 0.6, right = 0.95, top = 1.045, bottom = 0) +
  plot_annotation(
    caption = caption,
    theme = theme(
      text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
      plot.background = element_rect(fill = bg, colour = bg),
      plot.title = element_text(size = 128, hjust = 0.5),
      plot.subtitle = element_text(),
      plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
      plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
    )
  )

ggsave("scripts/2023/week-49-life-expectancy/life-expectancy-aus.png", height = 18, width = 18)
