# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(geofacet)
library(ggchicklet)
library(ggfx)
library(colorspace)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 45)

house <- dat$house

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- "#00A95C"

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
font_add_google("Archivo Black", "ab")
ft <- "bar"
ft1 <- "ab"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_base <- house |>
  group_by(year, state) |>
  summarise(
    green_votes = sum(totalvotes[party == "GREEN"], na.rm = TRUE),
    total = sum(totalvotes, na.rm = TRUE),
    runoff = sum(runoff),
    special = sum(special),
    writein = sum(writein),
    .groups = "drop"
  ) |>
  mutate(state = str_to_title(state)) |>
  group_by(year) |>
  mutate(
    p = green_votes/total,
    pct = to_pct(p, 3),
    votes_lab = scales::comma(green_votes)
  )

df_2020 <- df_base |>
  filter(year == 2020)

df_line <- df_base |>
  group_by(year) |>
  summarise(
    green_votes = sum(green_votes),
    total = sum(total),
    .groups = "drop"
  ) |>
  mutate(p = green_votes/total)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

subtitle <- "Total votes cast for the Green Party in 2020
The Green Party received 3-4% of the votes between 2000-2016"

# 📊 plot --------------------------------------------------------------------

g_base <- df_2020 |>
  ggplot() +
  with_shadow(geom_rrect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = p), radius = grid::unit(9, "pt"))) +
  geom_text(aes(0.1, 0.9, label = str_wrap(state, 10)), family = ft, size = 10, colour = txt, hjust = 0, lineheight = 0.3, vjust = 1) +
  geom_text(aes(0.5, 0.35, label = pct), family = ft, size = 16, colour = txt) +
  geom_text(aes(0.5, 0.15, label = votes_lab), family = ft, size = 8, colour = txt) +
  facet_geo(~state) +
  scale_fill_gradient(low = "grey95", high = accent) +
  coord_fixed() +
  labs(
    title = "THE GREEN PARTY",
    subtitle = subtitle,
    caption = caption,
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 160, hjust = 0, colour = accent, face = "bold", family = ft1),
    plot.subtitle = element_text(hjust = 0),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 80)),
    plot.margin = margin(b = 20, t = 50, r = 150, l = 50),
    strip.text = element_blank(),
    legend.position = "none"
  )

g_line <- df_line |>
  ggplot() +
  with_shadow(geom_area(aes(year, p), fill = accent), x_offset = -5, y_offset = -5) +
  scale_y_continuous(breaks = seq(0, 0.04, 0.01), labels = to_pct(seq(0, 0.04, 0.01), 2), position = "right") +
  labs(
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    legend.position = "none",
    axis.title = element_blank()
  )

g_base +
  inset_element(g_line, left = 0.83, right = 1.1, top = 0.3, bottom = 0)

ggsave("scripts/2023/week-45-house/house.png", height = 10, width = 14)

