# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 39)

rich <- dat$richmondway |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- pencil_case$pal1[2]

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Bangers", "bang")
font_add_google("Barlow", "bar")
ft <- "bar"
ft1 <- "bang"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df <- rich |>
  rename(n_fucks = f_count_total) |>
  mutate(angle = runif(n(), 350, 360))

df_fck <- df |>
  group_by(season) |>
  summarise(episode = max(episode) + 1) |>
  mutate(fck = "CK!") |>
  bind_rows(
    tibble(
      season = 1:3,
      episode = 0,
      fck = "F"
    )
  ) |>
  mutate(
    n_fucks = 30
    )

df_total <- df |>
  group_by(season) |>
  summarise(n_fucks = sum(n_fucks))

df_season <- tibble(
  x = 0,
  y = 1:3 + 0.3,
  lab = paste("Season", 1:3, " - ", df_total$n_fucks)
)

# 🔡 text --------------------------------------------------------------------

subtitle <- "The number of f-bombs unleashed by Roy Kent in Ted Lasso by season. Each 'U' is an episode and the number of fucks."
caption <- make_caption(accent)

caption <- paste0(subtitle, "<br>", caption)

# 📊 plot --------------------------------------------------------------------

df |>
  ggplot() +
  geom_text(aes(x = episode, y = -season, label = "U"), family = ft1, colour = accent, fontface = "bold", size = df$n_fucks*3, hjust = 0.5, angle = df$angle) +
  geom_text(aes(x = episode, y = -season, label = fck, size = n_fucks), df_fck, family = ft1, colour = txt, fontface = "bold", size = 90, hjust = 0.25) +
  geom_text(aes(x = x, y = -y, label = lab), df_season, family = ft, size = 12, colour = txt, hjust = 0, fontface = "italic") +
  # annotate("text", x = 8, y = -3.6, label = subtitle, family = ft, size = 12, colour = txt, fontface = "italic") +
  coord_cartesian(clip = "off") +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 42, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 150), face = "italic", lineheight = 0.6),
    plot.margin = margin(b = 50, t = 200, r = 200, l = 150)
  )

ggsave("scripts/2023/week-39-roy-kent-f-count/roy-kent-f-count.png", height = 12, width = 12)

