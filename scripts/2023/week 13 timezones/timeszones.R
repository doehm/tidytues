# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggforce)
library(ggfx)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 13)

trans <- dat$transitions

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "white"
bg <- "grey20"
accent <- '#ecbf3d'

pal <- c('#ecbf3d', '#f0cc46', '#f5e355', '#fdf56d', '#fdffbe', '#ffffff')

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
font_add_google("Rubik Mono One", "rubik")
showtext_auto()

ft <- "mukta"
ft1 <- "rubik"

# 🤼 wrangle -----------------------------------------------------------------

cities <- c("Paris", "Berlin", "London", "Sydney", "New York")

df_time <- trans |>
  group_by(zone) |>
  slice_max(end) |>
  mutate(city = str_replace(str_extract(zone, "(?<=/)[^/]*$"), "_", " ")) |>
  filter(city %in% cities) |>
  mutate(
    city = factor(city, levels = rev(cities)),
    lab = paste0(city, "."),
    offset = offset/3600,
    offset_lab = ifelse(offset < 0, as.character(offset), paste0("+", offset))
    ) |>
  ungroup()

df_circle <- map_dfr(cities, ~{
  tibble(
    x = 0,
    y = 0,
    r = seq(1, 0.75, length = 10),
    col = colorRampPalette(pal)(10),
    city = .x
  )
  }) |>
  mutate(city = factor(city, levels = rev(cities))) |>
  left_join(
    df_time |>
      select(city, offset),
    by = "city"
    ) |>
  mutate(y = as.numeric(city))

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}IANA tz database")

# 📊 plot --------------------------------------------------------------------

df_time |>
  ggplot() +
  geom_text(aes(0, city, label = lab), family = ft1, size = 48, colour = ifelse(df_time$dst, accent, txt), hjust = 0, fontface = "bold") +
  geom_text(aes(3.8, city, label = offset_lab), family = ft, colour = txt, size = 24, fontface = "bold") +
  geom_segment(aes(x=4, xend = 8, y = city, yend = city), colour = txt) +
  with_inner_glow(
    geom_circle(aes(x0 = offset/6+6, y0 = y, r = r/2, fill = col, group = city), df_circle, colour = NA),
    colour = bg, expand = 2, sigma = 5
  ) +
  scale_fill_identity() +
  coord_fixed() +
  labs(
    title = "TIME ZONES",
    subtitle = glue("Time zone offsets and <span style='color:{accent};'>daylight savings</span>"),
    caption = caption
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_markdown(colour = txt, hjust = 0.05, margin = margin(t = 20), size = 128, face = "bold", family = ft1),
    plot.subtitle = element_markdown(colour = txt, hjust = 0.05, margin = margin(t = 20)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
  )

ggsave("scripts/2023/week 13 timezones/timezones.png", height = 12, width = 16)
