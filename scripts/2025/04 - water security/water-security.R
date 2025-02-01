# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

water_insecurity_2022 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2022.csv')
water_insecurity_2023 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-28/water_insecurity_2023.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "white"
accent <- txt
bg <- "grey20"
bg1 <- "grey20"
bg2 <- "grey10"
line <- "grey30"
sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
pal <- c("#ee4266", "#3bceac")

font_add_google("Poppins", "pop", regular.wt = 200)
ft <- "pop"
showtext_auto()

# 🚙 functions ---------------------------------------------------------------



# 🤼 wrangle -----------------------------------------------------------------

df <- water_insecurity_2022 |>
  bind_rows(water_insecurity_2023) |>
  mutate(state = str_extract(name, "(?<=,[:space:]).*")) |>
  filter(!state %in% c("Puerto Rico", "District of Columbia")) |>
  group_by(year, state) |>
  summarise(
    total_pop = sum(total_pop, na.rm = TRUE),
    plumbing = sum(plumbing, na.rm = TRUE)
  ) |>
  group_by(state) |>
  mutate(
    increasing = plumbing[year == 2022] < plumbing[year == 2023],
    order = plumbing[year == 2022] + increasing*1e6
  ) |>
  ungroup() |>
  mutate(state = fct_reorder(state, order, min))

df_labs <- df |>
  filter(year == 2022) |>
  distinct(state, plumbing)

df_increasing <- df |>
  distinct(state, increasing) |>
  mutate(col = pal[increasing+1])

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(c(txt, sunset[c(2, 5)]), bg)
scale_x <- seq(0, 25000, 2500)
subtitle <- "Water insecurity can be influenced by number of social vulnerability
indicators from demographic characteristics to living conditions and socioeconomic
status that vary spatially across the U.S. Pumbing facilities are improving in 22
states and it is getting worse in 28 states."
title <- "US Water Insecurity"

df_text <- tibble(
  x = 10000,
  y = factor(c("Connecticut", "Alaska")),
  increasing = c(FALSE, TRUE),
  text = str_wrap(c("Plumbing facilities are improving", "Plumbing facilities are getting worse"), 10)
)

# 📊 plot --------------------------------------------------------------------

df |>
  ggplot() +
  geom_line(aes(x = plumbing, y = state), colour = txt) +
  geom_point(aes(x = plumbing, y = state, colour = as.factor(year)), size = 6) +
  geom_text(aes(x, y, label = text), df_text, family = ft, size = 48, hjust = 0, vjust = 1, lineheight = 0.25, fontface = "bold", colour = rev(pal)) +
  scale_x_continuous(breaks = scale_x, labels = paste0(scale_x/1000, "k")) +
  scale_colour_manual(values = sunset[c(2, 5)]) +
  facet_wrap(~increasing, nrow = 1, scales = "free_y") +
  labs(
    caption = caption,
    colour = "",
    x = "US households lacking plumbing facilities",
    title = title,
    subtitle = str_wrap(subtitle, 150)
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = grid::radialGradient(c(bg1, bg2)), colour = bg),
    plot.title = element_text(size = 128, hjust = 0, face = "bold"),
    plot.subtitle = element_text(hjust = 0, margin = margin(t = 10, b = 10)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "top",
    axis.text = element_text(hjust = 1, margin = margin(r = 5)),
    axis.text.x = element_text(hjust = 1, margin = margin(t = 10, b = 10)),
    axis.title.x = element_text(face = "bold"),
    panel.grid.major.y = element_line(linewidth = 0.5, linetype = 3, colour = line),
    panel.spacing = unit(1, "inches"),
    strip.text = element_blank()
  )

ggsave("scripts/2025/04 - water security/water security.png", height = 12, width = 20)

