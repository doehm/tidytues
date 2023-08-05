# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(geofacet)
library(rcartocolor)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 31)
states <- dat$states |>
  rename(abb = postal_abbreviation)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "white"
bg <- '#1E212B'
accent <- "white"

pal_low <- c("#009B9E", "#42B7B9", "#A7D3D4")
pal_high <- c("#E4C1D9", "#D691C1", "#C75DAB")

choose_col <- function(x) {
  if(any(x < 0)) {
    cols <- colorRampPalette(pal_low)(101)
  } else {
    cols <- colorRampPalette(pal_high)(101)
  }
  p <- round(min_max(x, 0, 1), 2)*100+1
  cols[p]
}

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
ft <- "bar"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_base <- states |>
  mutate(
    state = glue("{state} ({n_representatives})"),
    n_reps_per_1m = n_representatives/(population_2020/1e6),
    index = n_reps_per_1m/n_reps_per_1m[which(abb == "CA")],
    index_lab = paste0(round(index-1, 3)*100, "%"),
    index_lab1 = paste0(round(n_reps_per_1m, 1), " / ", round(index-1, 3)*100, "%"),
    state = fct_reorder(state, index, min),
    y_text = ifelse(index < 1, index-1.01, index - 0.99),
    y_state = ifelse(index < 1, 0.01, -0.01),
    hjust = ifelse(index < 1, 1, 0),
    hjust_state = ifelse(index < 1, 0, 1),
    grp = ifelse(index < 1, "low", "high")
  ) |>
  group_by(grp) |>
  mutate(
    col = choose_col(index-1),
    col = ifelse(abb == "CA", "white", col)
  )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "US states")
title <-
"Representation
of US states"
subtitle <-
"US States are either over or under
represented given their population
to number of representatives.

The number of representatives per
1 million population then compared
to California which has the largest
number of representatives.

An index of 10% means that state has
10% higher representation per 1 million
population than California.

Montana, Rhode Island and Wyoming all
have over 30% more representation than
California.

Delaware has the least representation
at 23% less than California.

Number in brackets indicates the number
of representatives for the state."

# 📊 plot --------------------------------------------------------------------

g_bar <- df_base |>
  ggplot() +
  geom_col(aes(state, index-1, fill = col)) +
  geom_text(aes(state, y_text, label = index_lab), family = ft, size = 12, colour = txt, hjust = df_base$hjust) +
  geom_text(aes(state, y_state, label = state), family = ft, size = 12, colour = txt, hjust = df_base$hjust_state) +
  annotate("text", x = 42, y = -0.23, label = subtitle, family = ft, colour = txt, size = 16, lineheight = 0.35, hjust = 0, vjust = 1) +
  annotate("text", x = 50, y = -0.23, label = title, family = ft, colour = txt, size = 48, lineheight = 0.35, hjust = 0, vjust = 1, fontface = "bold") +
  scale_fill_identity() +
  coord_flip(clip = "off") +
  labs(
    caption = caption,
    fill = "Index"
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0, face = "bold"),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "none"
  )

g_facet <- df_base |>
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = col)) +
  geom_text(aes(0.5, 0.66, label = abb), family = ft, size = 20, fontface = "bold", colour = bg) +
  geom_text(aes(0.5, 0.3, label = index_lab1), family = ft, size = 9, colour = bg) +
  facet_geo(~abb) +
  scale_fill_identity() +
  theme_void() +
  theme(
    strip.text = element_blank()
  )

g_bar +
  inset_element(g_facet, top = 0.6, bottom = 0.1, left = 0.55, right = 1) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave("scripts/2023/week-31-us-states/us-states.png", height = 12, width = 20)

