# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(mapdata)
library(biscale)
library(ggforce)

# 💾 load data ---------------------------------------------------------------

agencies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-18/agencies.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "white"
bg <- "black"
accent <- txt

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
autumn <- c('#1c2426', '#455852', '#9d8e71', '#d4935d', '#cc612b', '#a5371e', '#ab443b')
pal <- rev(eyedroppeR::pencil_case$bright$cat)
pal <- rev(eyedroppeR::pencil_case$secret_of_mana$cat)
pal <- sunset

font_add_google("Poppins", "pop", regular.wt = 200)
ft <- "pop"
showtext_auto()

# 🚙 functions ---------------------------------------------------------------



# 🤼 wrangle -----------------------------------------------------------------

df_state <- map_data("county")

df_base <- agencies |>
  filter(
    longitude < -10,
    longitude > -125
  ) |>
  mutate(
    county = tolower(county),
    county_split = str_split(county, ", ")
  ) |>
  unnest(county_split) |>
  group_by(county = county_split) |>
  summarise(
    p = mean(is_nibrs),
    n = sum(is_nibrs)
  )

df_base <- df_state |>
  left_join(df_base, join_by(subregion == county))

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, bg)
title <- "Agencies on the FBI’s National Incident-Based Reporting System (NIBRS)"
subtitle <- "Crime data is dynamic. Offenses occur, arrests are made, and property
is recovered every day. The FBI’s Crime Data Explorer, the digital front door for
UCR data, is an attempt to reflect that fluidity in crime. The data presented there
is updated regularly in a way that UCR publications previously could not be.
Launched in 2017, the CDE’s content and features are updated and expanded continuously.
CDE enables law enforcement agencies, researchers, journalists, and the public to
more easily use and understand the massive amounts of UCR data using charts and graphs."

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = n), colour = bg, linewidth = 0.1) +
  annotate("text", x = -124.6813, y = 25.13, label = "79%", family = ft, size = 128, colour = txt, hjust = 0, vjust = -0.2) +
  annotate("text", x = -124.6813, y = 24.9, label = "of agencies are on FBI’s National Incident-Based Reporting System (NIBRS).", family = ft, size = 16, colour = txt, hjust = 0) +
  scale_fill_gradientn(colours = pal) +
  coord_fixed() +
  labs(
    title = title,
    subtitle = str_wrap(subtitle, 200),
    caption = caption,
    fill = "Agency\ncount"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = grid::radialGradient(c(bg, "grey25"), r1 = unit(0.5, "npc"))),
    plot.title = element_text(size = 128, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "bottom"
  )

ggsave("scripts/2025/07 - agencies/agencies.png", height = 13.5, width = 24)
