# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(glue)
library(ggtext)
library(alone)
library(cropcircles)
library(magick)
library(ggpath)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"

pal <- c('#0C1315', '#1C2D34', '#263D46', '#304B55', '#3E5D66', '#56767C', '#868E91', '#FBFCFC')

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
font_add_google("Bebas Neue", "bn")
showtext_auto()

ft <- "mukta"
ft1 <- "bn"

# 🤼 wrangle -----------------------------------------------------------------

starvation <- c(
  "BMI too low",
  "Systolic pressure too low",
  "Lost too much weight",
  "Low BMI, lost too much weight",
  "Frostbite, malnutrition",
  "Low BMI (lost too much weight), parastic infection"
)

df_base <- survivalists |>
  group_by(season) |>
  mutate(p = days_lasted/max(days_lasted)) |>
  left_join(
    episodes |>
      group_by(season) |>
      summarise(max_eps = max(episode)),
    by = "season"
  ) |>
  mutate(ep_tap = ceiling(max_eps*p)) |>
  left_join(
    episodes |>
      select(season, episode, quote, author),
    by = c("season", "ep_tap" = "episode")
  ) |>
  filter(medically_evacuated, reason_tapped_out %in% starvation) |>
  filter(!name %in% c("Jesse Bosdell", "Brad Richardson")) |>
  ungroup() |>
  arrange(desc(days_lasted)) |>
  mutate(
    y = n():1,
    path = as.character(glue("scripts/2023/week 4 alone/images/S{season} {name}.jpg")),
    images = map_chr(path, ~circle_crop(.x, border_size = 16, border_colour = pal[2])),
    demogs = glue("Season {season} | Age {age}, {profession}"),
    location = glue("{state}, {country}"),
    quote = paste0(str_wrap(quote, 40), "\n                          - ", author)
    )

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
caption <- glue("{mastodon} @danoehm@{space}fosstodon.org • {twitter} @danoehm • {github} doehm/tidytuesday • {floppy} alone R pacakge")
subtitle <- "     A tribute to the Alone survivalists that were medically evacuated from the game due to
starvation or starvation related health concerns"

# 📊 plot --------------------------------------------------------------------

a <- 8
df_base |>
  ggplot(aes(y, days_lasted, fill = gender)) +
  geom_col(width = 0.95) +
  geom_from_path(aes(path = images), width = 0.088) +
  geom_from_path(aes(1.8, 100, path = "scripts/2023/week 4 alone/images/trees.jpg"), width = 0.4) +
  geom_text(aes(y+0.3, days_lasted+a-1, label = name), family = ft, size = 20, fontface = "bold", colour = txt, hjust = 0) +
  geom_text(aes(y+0.1, days_lasted+a, label = demogs), family = ft, size = 16, colour = txt, hjust = 0) +
  geom_text(aes(y-0.1, days_lasted+a, label = location), family = ft, size = 16, colour = txt, hjust = 0) +
  geom_text(aes(y-0.3, days_lasted+a-1, label = reason_tapped_out), family = ft, size = 16, colour = txt, hjust = 0) +
  geom_text(aes(y, 4, label = quote), family = ft, size = 18, fontface = "bold", colour = "white", hjust = 0, lineheight = 0.4) +
  scale_y_continuous(breaks = seq(0, 90, 10), labels = seq(0, 90, 10), limits = c(0, 115)) +
  scale_fill_manual(values = pal[c(2, 4)]) +
  labs(
    caption = caption,
    title = "WHEN THE MIND IS STRONGER THAN THE BODY...",
    subtitle = subtitle,
    y = "Days lasted"
    ) +
  coord_flip(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 64, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 220, family = ft1, colour = txt, hjust = 0.2),
    plot.subtitle = element_text(hjust = 0.06),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t=20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text.x = element_text(),
    axis.title.x = element_text(margin = margin(t = 10, b = 10)),
    legend.position = "none"
  )

ggsave("scripts/2023/week 4 alone/alone.png", height = 18, width = 19)
