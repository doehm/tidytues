# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(mapdata)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 26)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "#D4D4D4"
bg <- "#f7e4c6"
accent <- "#875346"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Noticia Text", "bar")
font_add_google("Monoton", "mon")
showtext_auto()

ft <- "bar"
ft1 <- "mon"

# 🤼 wrangle -----------------------------------------------------------------

df_cali <- dat$us_place_names |>
  filter(state_name == "California") |>
  filter(str_detect(tolower(feature_name), "mountain")) |>
  select(x = prim_long_dec, y = prim_lat_dec, everything()) |>
  drop_na() |>
  left_join(distinct(dat$us_place_history, feature_id, description, history), by = "feature_id") |>
  arrange(desc(y)) |>
  mutate(
    id = 1:n(),
    label = glue("{feature_name}"),
    point = glue("{id} <span style='font-family:fa-solid'>&#xf6fc;</span>"),
    y_text = seq(42, min(y), length = n()),
    text = coalesce(history, description)
    )

cali <- map_data("state") |>
  filter(region == "california")

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}Historical Marker Database")

subtitle <- "Mountain related\nstuff in California"

# 📊 plot --------------------------------------------------------------------

df_cali |>
  ggplot(aes(x, y)) +
  geom_polygon(aes(long, lat), cali, fill = "#cddbc2", colour = txt) +
  geom_text(aes(-128, y_text, label = label), family = ft, colour = txt, size = 12, hjust = 0) +
  geom_text(aes(-128.2, y_text, label = id), family = ft, colour = txt, size = 12, vjust = 1, fontface = "bold") +
  geom_richtext(aes(x, powerglove::breathing_space(y, diff = 0.1), label = point), family = ft, colour = txt, size = 12, label.color = NA, fill = NA) +
  geom_text(aes(-128, y_text-0.1, label = str_wrap(text, 70)), family = ft, colour = txt, size = 6, hjust = 0, lineheight = 0.25, vjust = 1) +
  annotate("text", x = -118, y = 41, label = subtitle, family = ft, size = 24, colour = txt, hjust = 0, vjust = 1, lineheight = 0.35) +
  coord_map() +
  labs(
    title = "CALIFORNIA",
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 48),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 260, family = ft1, hjust = 0.5, colour = "#875346"),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
  )

ggsave("scripts/2023/week-26-us-places/us-places.png", height = 12, width = 12)
