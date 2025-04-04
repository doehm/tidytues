# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggpath)
library(ggforce)
library(ggrepel)

# 💾 load data ---------------------------------------------------------------

pokemon_df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv') |>
  mutate(
    pokemon = str_to_title(pokemon)
  )

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add("pokemon", regular = "assets/fonts/pokemon/Pokemon solid.ttf")

txt <- "grey20"
bg <- "white"
accent <- txt

pal <- c('#d34d60', '#e9e44c', '#68ae29', '#7abfb4', '#556eaf', '#847a9e', '#f6c8d5')

font_add_google("Raleway", "raleway", regular.wt = 200)
ft <- "raleway"
showtext_auto()

# 💃 model -------------------------------------------------------------------

mod <- glm(hp ~ height + weight + base_experience + defense + attack + speed, data = pokemon_df)
summary(mod)

# 🤼 wrangle -----------------------------------------------------------------

df_mod <- tibble(
  id = pokemon_df$id,
  y = pokemon_df$hp,
  y_hat = fitted.values(mod),
  res = residuals(mod)
) |>
  filter(!id %in% c(10120, 10057, 10026, 10056)) |>
  mutate(
    rank = n() - rank(res) + 1,
    top_n = rank <= 16
  )

df <- pokemon_df  |>
  select(id, pokemon, height, weight, base_experience, hp, attack, defense, speed) |>
  pivot_longer(-c(id, pokemon), names_to = "var", values_to = "val") |>
  group_by(var) |>
  mutate(val_scaled = min_max(val, 0, 1)) |>
  inner_join(df_mod, join_by(id)) |>
  filter(top_n)

df_url <- pokemon_df |>
  inner_join(df_mod, join_by(id)) |>
  filter(top_n)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, bg)
title_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/9/98/International_Pok%C3%A9mon_logo.svg/1200px-International_Pok%C3%A9mon_logo.svg.png"
subtitle <- "A model was fit to estimate the Pokemon's HP based on height, weight, attack, defense, speed, base experience.
The 16 Pokemon below are those that have the largest positive difference between their actual and expected HP levels (i.e. the largest positive residual)."

# 📊 plot --------------------------------------------------------------------

plt_point <- df_mod |>
  ggplot(aes(y_hat, y)) +
  geom_mark_hull(aes(fill = top_n), filter(df_mod, top_n), fill = pal[2], colour = pal[5], linewidth = 0.5) +
  geom_point(alpha = 0.2, size = 3, colour = pal[5]) +
  geom_smooth(method = "lm", se = FALSE, colour = pal[5]) +
  geom_text_repel(aes(label = pokemon), filter(df_url, top_n), family = ft, colour = pal[5], size = 12) +
  coord_cartesian(clip = "off") +
  labs(
    x = "Predicted HP",
    y = "HP"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 30),
    panel.spacing.y = unit(3, "cm"),
    panel.spacing.x = unit(2, "cm"),
    legend.position = "bottom",
    axis.text = element_text(margin = margin(t = 10)),
    axis.line = element_line(linewidth = 0.2),
    axis.ticks = element_line(linewidth = 0.2),
    axis.ticks.length = unit(0.25, "cm"),
    axis.title = element_text()
  )

plt_title <- ggplot() +
  annotate("from_path", x = 1, y = 0.75, path = title_url, width = 0.8) +
  annotate("text", x = 1, y = 0.5, label = str_wrap(subtitle, 50), family = ft, colour = txt, size = 16, vjust = 1, lineheight = 0.3) +
  coord_cartesian(clip = "off") +
  theme_void() +
  xlim(0, 2) +
  ylim(-1, 1)

plt_title +
  inset_element(plt_point, left = 0, top = 0.6, bottom = 0.1, right = 1)

ggsave("scripts/2025/13 - pokemon/pokemon-title.png", height = 12, width = 6)

# 📊 plot --------------------------------------------------------------------

df |>
  mutate(var = snakecase::to_title_case(var)) |>
  ggplot() +
  geom_col(aes(var, 1.5), fill = "grey95") +
  geom_col(aes(var, val_scaled+0.5, fill = var)) +
  geom_text(aes(var, val_scaled+0.52, label = val), family = ft, colour = txt, size = 12, hjust = 0) +
  geom_point(aes("Height", 0.01), df_url,  size = 56, colour = txt) +
  geom_point(aes("Height", 0), df_url,  size = 56, colour = "white") +
  geom_from_path(aes("Height", 0, path = url_image), df_url, width = 0.7) +
  facet_wrap(~pokemon) +
  scale_fill_manual(values = rev(pal)) +
  labs(
    caption = caption,
    fill = "Attribute"
  ) +
  coord_flip(clip = "off") +
  guides(fill = guide_legend(nrow = 1)) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 100),
    panel.spacing.y = unit(3, "cm"),
    panel.spacing.x = unit(2, "cm"),
    legend.position = "bottom",
    legend.margin = margin(t = 20),
    strip.text = element_text(family = "pokemon", margin = margin(b = 10), size = 64)
  )

ggsave("scripts/2025/13 - pokemon/pokemon.png", height = 12, width = 20)

title <- image_read("scripts/2025/13 - pokemon/pokemon-title.png")
body <- image_read("scripts/2025/13 - pokemon/pokemon-stats.png")

image_append(c(title, body)) |>
  image_write("scripts/2025/13 - pokemon/pokemon.png")

