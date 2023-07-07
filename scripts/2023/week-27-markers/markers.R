# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 27)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- "grey20"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
font_add_google("Graduate", "grad")
showtext_auto()

ft <- "mukta"
ft1 <- "grad"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- dat$historical_markers |>
  count(state = state_or_prov) |>
  arrange(desc(n)) |>
  mutate(state = fct_reorder(state, n, max)) |>
  slice_head(n = 20) |>
  mutate(
    fill = letters[1:20],
    n_lab = paste0(round(n/1000, 1), "k"),
    state_lab = ifelse(as.character(state) == "District of Columbia", "D.C.", as.character(state)),
    state_lab = str_wrap(state_lab, 10)
    )

make_fat_arrow <- function(xmin, xmax, ymin, ymax, edge_length = 0.2, body_length = 0.66) {

  dx <- xmax-xmin
  dy <- ymax-ymin

  tibble(
    x = c(xmin, xmin, xmin+dx*body_length, xmin+dx*body_length, xmax, xmin+dx*body_length, xmin+dx*body_length, xmin),
    y = c(ymin+dy*edge_length, ymax-dy*edge_length, ymax-dy*edge_length, ymax, ymin+0.5*dy, ymin, ymin+dy*edge_length, ymin+dy*edge_length)
  )

}

df_fat_arrow <- make_fat_arrow(12, 19, 4500, 11000)

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}Historical Maraker Database")

title <- "EVERYTHING'S\nBIGGER IN"
subtitle <-
"The top 20 states with the most
markers. Texas has 3 times as many
as second place Georgia"

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_brick(aes(state, n, fill = n), colour = NA, size = 0.2) +
  geom_text(aes(state, y = n+700, label = state_lab), family = ft, colour = txt, lineheight = 0.25, size = 16, fontface = "bold", vjust = 0) +
  geom_text(aes(state, y = n+350, label = n_lab), family = ft, colour = txt, lineheight = 0.25, size = 16) +
  geom_polygon(aes(x, y), df_fat_arrow, fill = "grey90", colour = txt) +
  annotate("shadowtext", x = 2, y = 11000, label = title, size = 84, family = ft1, lineheight = 0.26, colour = "white", hjust = 0, vjust = 1, fontface = "bold") +
  annotate("shadowtext", x = 2, y = 7800, label = "TEXAS", size = 200, family = ft1, lineheight = 0.3, colour = "white", hjust = 0, vjust = 1, fontface = "bold") +
  annotate("text", x = 13, y = 8500, label = "...even the arrow annotations!", family = ft, colour = txt, lineheight = 0.25, size = 16, hjust = 0, vjust = 1) +
  annotate("text", x = 13, y = 7500, label = subtitle, family = ft, colour = txt, lineheight = 0.25, size = 16, hjust = 0, vjust = 1) +
  scale_fill_gradientn(colours = c("darkblue", mid = "white", high = "firebrick4")) +
  labs(
    caption = caption,
    fill = "Number of\nHistorical\nMarkers"
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.title = element_text(size = 40)
  )

ggsave("scripts/2023/week-27-markers/markers.png", height = 10, width = 20)
