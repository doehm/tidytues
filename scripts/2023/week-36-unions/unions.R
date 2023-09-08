# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "white"
bg <- '#1E212B'
accent <- "orange"
pal <- c("#fb8500", "#ffb703")

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
font_add_google("Josefin Sans", "jose")
font_add_google("Arimo", "arimo")
ft <- "bar"
ft <- "jose"
ft <- "arimo"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_base <- demographics |>
  filter(facet %in% c("private sector: all", "public sector: all")) |>
  select(year, p_members, p_covered,  facet) |>
  filter(year %in% c(1990, 2020)) |>
  mutate(
    pct = to_pct(p_members),
    x1 = ifelse(year == 1990, 1985, 2025)
    ) |>
  left_join(
    wages |>
      select(year, wage, facet),
    by = c("year", "facet")
  ) |>
  group_by(year) |>
  mutate(
    ratio = wage/wage[facet == "private sector: all"],
    r_pct = paste0("+", to_pct(ratio-1)),
    ratio_members = p_members/p_members[facet == "private sector: all"],
    r_pct_members = paste0("+", to_pct(ratio_members))
    )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "Data")
title <- "US Union Membership"
text <- glue(
"The {ct('public sector', pal[2])} has historical had much higher rates of union membership than the {ct('private sector', pal[1])}.<br>
From 1990 to 2020 Union membership in the US has dropped {ct('1%', pal[2])} point in the {ct('public sector', pal[2])} and {ct('6%', pal[1])}<br>
points in the {ct('private sector', pal[1])} in terms of the percentage of total employment, effectively halving the<br>
rate from 1990. Wages for the {ct('public sector', pal[2])} were, on average, 18% higher than the {ct('private sector', pal[1])}<br>
in 1990 and only 4% in 2020")

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  with_shadow(
    geom_line(aes(year, p_members, group = facet, colour = facet), linewidth = 10, lineend = "round"),
    x_offset = 30,
    y_offset = 30
  ) +
  geom_text(aes(x = x1, y = p_members, colour = facet, label = pct), family = ft, size = 48) +
  geom_text(aes(x = year, y = 0, label = year), family = ft, size = 24, colour = txt) +
  annotate("text", x = 2005, y = 0.355+0.05, label = "Public", family = ft, colour = pal[2], size = 32) +
  annotate("text", x = 2005, y = 0.09+0.05, label = "Private", family = ft, colour = pal[1], size = 32, angle = 356) +
  annotate("text", x = 2005, y = 1, label = title, family = ft, colour = txt, size = 64, fontface = "bold", vjust = 1) +
  annotate("richtext", x = 2005, y = 0.88, label = text, family = ft, colour = txt, size = 16, vjust = 1, lineheight = 0.35,
           label.color = NA, fill = NA) +
  scale_colour_manual(values = pal) +
  ylim(0, 1) +
  labs(caption = caption) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 30)),
    plot.margin = margin(b = 150, t = 120, r = 150, l = 150),
    legend.position = "none"
  )

ggsave("scripts/2023/week-36-unions/unions.png", height = 12, width = 12)

