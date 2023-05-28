# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 21)

df <- dat$squirrel_data |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "black"
bg <- "white"
accent <- "black"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
showtext_auto()

ft <- "bar"

# 🤼 wrangle -----------------------------------------------------------------

df_actions <- df |>
  summarise(
    running = sum(running),
    chasing = sum(chasing),
    climbing = sum(climbing),
    eating = sum(eating),
    foraging = sum(foraging)
  ) |>
  pivot_longer(everything(), names_to = "action", values_to = "n") |>
  mutate(
    p = n/sum(n),
    pct = glue("{round(p*100)}%"),
    action = str_to_title(action)
    )

df_coords <- tribble(
   ~action,  ~x0,  ~y0,  ~r,  ~x1,  ~y1,  ~grp,
   "Running", 0.1, 0.4, 0.05, 0.1, 0.16, 1,
   "Chasing", 0.3, 0.5, 0.05, 0.3, 0.25, 0,
   "Chasing", 0.3, 0.25, 0.05, 0.35, 0.15, 2,
   "Eating", 0.8, 0.4, 0.05, 0.8, 0.17, 1,
   "Foraging", 0.95, 0.4, 0.05, 0.95, 0.13, 1,
   "Climbing", 0.48, 0.6, 0.05, 0.48, 0.39, 1
) |>
  left_join(df_actions, by = "action")

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}2018 Central Park Squirrel Census")

# 📊 plot --------------------------------------------------------------------

a <- 0.02
b <- 0.01
df_coords |>
  ggplot() +
  geom_from_path(aes(0.5, 0.5, path = "scripts/2023/week-21-squirrels/base.png")) +
  geom_segment(aes(x = x0, xend = x1, y = y0, yend = y1), colour = "black") +
  geom_point(aes(x1, y1), filter(df_coords, grp >= 1), size = 3) +
  geom_circle(aes(x0 = x0, y0 = y0, r = r), filter(df_coords, grp <= 1), fill = bg, colour = "black", size = 0.5) +
  geom_text(aes(x = x0, y = y0-a, label = action), filter(df_coords, grp <= 1), family = ft, size = 14, colour = txt, fontface = "bold") +
  geom_text(aes(x = x0, y = y0+b, label = pct), filter(df_coords, grp <= 1), family = ft, size = 28, colour = txt, fontface = "bold") +
  annotate("text", x = 1, y = 1, hjust = 1, vjust = 1, family = ft, size = 36, fontface = "bold", colour = "white",
           label = "A day in the life of a\nsquirrel in Central Park", lineheight = 0.3) +
  xlim(0, 1) +
  ylim(0, 1) +
  coord_fixed(clip = "off") +
  labs(
    caption = caption
  ) +
  theme_void() +
  theme(
    plot.margin = margin(t = 10, b = 40, l = 10, r = 10),
    plot.background = element_rect(fill = bg, colour = "white"),
    plot.caption = element_markdown(margin = margin(b = 20, t = 10), size = 36, family = ft, colour = txt, hjust = 0.5)
  )

ggsave("scripts/2023/week-21-squirrels/squirrels.png", height = 12, width = 12)
