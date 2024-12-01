# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(glue)
library(ggtext)
library(lubridate)

# 💾 load data ---------------------------------------------------------------

cbp_resp <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_resp.csv')
cbp_state <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-26/cbp_state.csv')

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
txt <- "white"
accent <- txt

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Poppins", "pop", regular.wt = 200)
ft <- "pop"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df <- cbp_resp |>
  mutate(date = ym(paste(fiscal_year, month_abbv))) |>
  group_by(date, land_border_region, encounter_type) |>
  summarise(n = sum(encounter_count)) |>
  mutate(land_border_region = factor(land_border_region, levels = c("Southwest Land Border", "Northern Land Border", "Other"))) |>
  ungroup()

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

# 📊 plot --------------------------------------------------------------------

df |>
  ggplot() +
  geom_area(aes(date, n, fill = encounter_type)) +
  facet_wrap(~land_border_region, nrow = 1) +
  scale_y_continuous(breaks = seq(0, 3e5, 1e5), label = scales::comma(seq(0, 3e5, 1e5))) +
  scale_fill_manual(values = sunset[c(1, 3, 5)]) +
  labs(
    caption = caption,
    fill = ""
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text = element_text(margin = margin(t = 10, r = 10, l = 10)),
    axis.line.x = element_line(colour = txt),
    axis.ticks.x = element_line(colour = txt),
    axis.ticks.length.x = unit(0.25, "cm"),
    legend.box.margin = margin(b = 20),
    legend.position = "top"
  )

ggsave("scripts/2024/week-48-border-control/border-control.png", height = 7, width = 12)
