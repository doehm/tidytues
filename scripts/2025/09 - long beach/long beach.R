# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

longbeach <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "grey20"
bg <- colorspace::lighten("#f4f1de", 0.9)
accent <- txt

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
autumn <- c('#1c2426', '#455852', '#9d8e71', '#d4935d', '#cc612b', '#a5371e', '#ab443b')
pal <- sunset

font_add_google("Poppins", "pop", regular.wt = 200)
ft <- "pop"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df <- longbeach |>
  mutate(
    animal_type = case_when(
      animal_type %in% c("other", "wild", "guinea pig", "livestock", "amphibian", "rabbit", "reptile") ~ "other",
      TRUE ~ animal_type
    ),
    animal_type = factor(animal_type, levels = c("dog", "cat", "bird", "other"))
  ) |>
  count(animal_type, outcome_is_dead) |>
  group_by(animal_type) |>
  mutate(
    p = n/sum(n),
    xmin = lag(p, default = 0),
    xmax = cumsum(p),
    ymin = rev(as.numeric(animal_type)),
    ymax = rev(as.numeric(animal_type)+0.5),
    pct = paste0(round(100*p), "%")
  )

b <- 6
wd <- 1
lab_d <- 0.5

df_total <- df |>
  group_by(animal_type) |>
  summarise(
    n = sum(n),
    .groups = "drop"
  ) |>
  mutate(
    p = n/sum(n),
    xmin = cumsum(lag(p, default = 0)),
    xmax = cumsum(p),
    ymin = b,
    ymax = b+wd,
    x_lab = xmin + (xmax - xmin)/2,
    y_lab = ifelse(as.numeric(animal_type) %% 2 == 0, b-lab_d, b+wd+lab_d),
    vjust = ifelse(as.numeric(animal_type) %% 2 == 0, 1, 0),
    pct = paste0(round(100*p), "%"),
    lab = paste0(str_to_title(animal_type), ": ", pct),
    yl = ifelse(as.numeric(animal_type) %% 2 == 0, b, b+wd),
    yl_end = ifelse(as.numeric(animal_type) %% 2 == 0, b-lab_d+0.1, b+wd+lab_d-0.1)
  )

a <- -0.1
df_line <- tribble(
  ~x, ~xend, ~y, ~yend,
  0, a, b+0.5, b+0.5,
  a, a, b+0.5, 1.25,
) |>
  bind_rows(
    tibble(
      x = a,
      xend = 0,
      y = 1:4 + 0.25,
      yend = y,
      animal_type = df_total$animal_type
    )
  )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, bg)
title <- "Dogs have the highest\nsurvival rate at the\nLong Beach Animal Shelter"

# 📊 plot --------------------------------------------------------------------

df |>
  ggplot() +
  geom_segment(aes(x = x_lab, xend = x_lab, y = b+wd/2, yend = yl_end), df_total) +
  geom_point(aes(x_lab, yl, colour = animal_type), df_total, size = 6) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = animal_type), df_total) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = animal_type, alpha = outcome_is_dead)) +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), df_line) +
  geom_text(aes(x_lab, y_lab, label = lab), df_total, vjust = df_total$vjust, family = ft, size = 24, colour = txt) +
  geom_text(aes(1.1, ymin + 0.25, label = pct), filter(df, !outcome_is_dead), family = ft, size = 24, colour = txt) +
  geom_point(aes(x = xend, y = y, colour = animal_type), df_line[3:6,], size = 6) +
  annotate("text", x = 0.5, y = 4.75, label = "% of animals alive at outcome", family = ft, size = 20, colour = txt) +
  scale_alpha_manual(values = c(`TRUE` = 0.2, `FALSE` = 1)) +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  coord_cartesian(clip = "off") +
  ylim(1, b+wd+lab_d) +
  labs(
    caption = caption,
    title = title
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "none"
  )

ggsave("scripts/2025/09 - long beach/long beach.png", height = 16, width = 10)
