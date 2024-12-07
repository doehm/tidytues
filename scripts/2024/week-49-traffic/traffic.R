# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

traffic <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-03/A64_traffic.csv') |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
txt <- "white"
accent <- txt
bg <- "black"

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Poppins", "pop", regular.wt = 200)
ft <- "pop"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_base <- traffic |>
  select(site_id, time_interval, 10:23) |>
  pivot_longer(cols = starts_with("x"), names_to = "speed", values_to = "volume") |>
  group_by(site_id, time_interval, speed) |>
  summarise(volume = sum(volume, na.rm = TRUE)) |>
  mutate(
    speed = case_when(
      speed == "x0_10_mph" ~ "<10 mph",
      speed == "x80_mph" ~ ">80 mph",
      TRUE ~ glue("{str_sub(speed, 2, 3)}-{str_sub(speed, 5, 6)} mph")
    ),
    speed = factor(speed, levels = rev(unique(speed)))
  ) |>
  ungroup()

df_base |>
  group_by(site_id) |>
  summarise(volume = sum(volume)/31)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

x_labs <- c("12am", "6am", "12pm", "6pm")

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_col(aes(time_interval, volume, fill = volume)) +
  facet_grid(speed ~ site_id) +
  scale_x_continuous(breaks = seq(0, 72, 24), labels = x_labs) +
  scale_fill_gradientn(colours = paletteer_d("rcartocolor::ag_GrnYl")) +
  labs(fill = "Volume") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 64, lineheight = 0.3, colour = txt),
    # plot.background = element_rect(fill = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    # plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text.x = element_text(margin = margin(t = 10), size = 48),
    axis.line.x = element_line(colour = txt),
    axis.ticks = element_line(colour = txt),
    axis.ticks.length.x = unit(0.25, "cm"),
    legend.position = "left",
    legend.title = element_text(margin = margin(b = 10), face = "bold")
  )

ggsave("scripts/2024/week-49-traffic/traffic.png", height = 12, width = 18)


