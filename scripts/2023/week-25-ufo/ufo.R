# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(geofacet)
library(cropcircles)
library(ggpath)
library(magick)
library(readr)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 25)

# ✍️ fonts and palettes ------------------------------------------------------

alien <- c('#47fcea', '#28ee85', '#17bd52', '#679d76', '#3e6f50', '#27593d')
txt <- alien[2]
bg <- '#101319' # '#010101'
accent <- txt

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Orbitron", "orb")
font_add_google("Barlow", "bar")
showtext_auto()
ft <- "orb"
ft1 <- "bar"

# 🤼 wrangle -----------------------------------------------------------------

df_pop <- dat$places |>
  filter(country_code == "US") |>
  group_by(state) |>
  summarise(pop = sum(population)) |>
  ungroup()

df_us <- dat$ufo_sightings |>
  filter(country_code == "US") |>
  count(state) |>
  left_join(df_pop, by = "state") |>
  mutate(
    r = n/pop*10000,
    x = min_max(r, 0.2, 1),
    xmin = (1-x)/2,
    xmax = x + (1-x)/2,
    alpha = r/max(r)
    )

df_shape <- dat$ufo_sightings |>
  filter(!shape %in% c("unknown", "other")) |>
  count(shape) |>
  arrange(desc(n)) |>
  slice_head(n=10) |>
  mutate(
    shape = fct_reorder(shape, n, min),
    alpha = min_max(n, 0.3, 1)
    )

df_day_hour <- dat$ufo_sightings |>
  mutate(
    day = wday(reported_date_time),
    hour = hour(reported_date_time),
    wday = wday(reported_date_time, label = TRUE)
  ) |>
  count(day, wday, hour) |>
  mutate(
    alpha = n/max(n),
    hour_lab = case_when(
      hour == 0 ~ "12am",
      hour <= 12 ~ paste0(hour, "am"),
      hour == 12 ~ "12pm",
      TRUE ~ paste0(hour-12, "pm"))
    )

# image made with openai::create_image() API call to DALL-E
df_alien <- tibble(
  x = 0,
  y = -5,
  image = image_read("scripts/2023/week-25-ufo/alien.png") |>
    image_fill(color = "none", fuzz = 10) |>
    circle_crop()
)

# print a few random summaries
sample_summary <- function(n) {
  x <- dat$ufo_sightings |>
    mutate(id = 1:n()) |>
    filter(str_length(summary) > 120) |>
    sample_n(n)
  text <- x |>
    pull(summary) |>
    str_to_sentence()
  list(
    text = paste0('"...', text, '..."'),
    id = x$id
  )
}

quotes <- paste0('"...', str_to_sentence(dat$ufo_sightings$summary[c(47816, 6795, 93833)]), '..."')

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}UFO sightings")

# 📊 plot --------------------------------------------------------------------

g_us <- df_us |>
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, alpha = alpha), fill = accent) +
  geom_text(aes(0.5, 0.7, label = state), lineheight = 0.25, family = ft, size = 10, colour = bg, fontface = "bold") +
  geom_text(aes(0.5, 0.3, label = round(r, 1)), lineheight = 0.25, family = ft, size = 8, colour = bg, fontface = "bold") +
  facet_geo(~state) +
  scale_alpha_identity() +
  coord_fixed() +
  labs(subtitle = "Sightings per 10k population") +
  theme_void() +
  theme(
    strip.text = element_blank(),
    plot.subtitle = element_text(family = ft1, size = 40, hjust = 1, colour = txt, margin = margin(b = 10)),
    legend.position = "none"
  )

g_day <- df_day_hour |>
  ggplot() +
  geom_tile(aes(hour, day, alpha = alpha), fill = accent, height = 0.9, width = 0.9) +
  geom_text(aes(hour, 9, label = hour_lab), colour = accent, family = ft, size = 10) +
  geom_text(aes(0, day, label = str_sub(wday, 1, 1)), colour = bg, family = ft, size = 8, fontface = "bold") +
  geom_from_path(aes(0, -5, path = image), df_alien, width = 0.25) +
  scale_alpha_identity() +
  coord_polar() +
  ylim(-5, 9) +
  xlim(NA, 23.55) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = bg, colour = bg)
  )

g_shape <- df_shape |>
  ggplot() +
  geom_col(aes(shape, n, alpha = alpha), fill = accent) +
  geom_text(aes(10, 200, label = "10 most common reported shapes"), family = ft1, colour = txt, size = 16, nudge_x = 1, hjust = 0) +
  geom_text(aes(shape, 200, label = str_to_title(shape)), family = ft, colour = bg, size = 14, hjust = 0, fontface = "bold", nudge_x = 0.2) +
  geom_text(aes(shape, n-200, label = scales::comma(n)), family = ft, colour = bg, size = 10, hjust = 1, fontface = "bold", nudge_x = -0.2) +
  coord_flip(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none"
  )

g_base <- ggplot() +
  labs(
    title = "UFO Sightings",
    subtitle = "Summary of over 88k reported sightings across the US",
    caption = caption
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, face = "bold", hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(family = ft1, hjust = 0.5, margin = margin(b = 20)),
    plot.caption = element_markdown(family = ft1, colour = colorspace::darken(txt, 0.5), hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text.x = element_text()
  )

quote1 <- ggplot() +
  annotate("text", x = 0, y = 1, label = str_wrap(quotes[1], 40), family = ft1, colour = txt, size = 16, hjust = 0, fontface = "italic", lineheight = 0.4) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  coord_cartesian(clip = "off")

quote2 <- ggplot() +
  annotate("text", x = 0, y = 1, label = str_wrap(quotes[2], 25), family = ft1, colour = txt, size = 16, hjust = 0, fontface = "italic", lineheight = 0.4) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  coord_cartesian(clip = "off")

quote3 <- ggplot() +
  annotate("text", x = 0, y = 1, label = str_wrap(quotes[3], 25), family = ft1, colour = txt, size = 16, hjust = 0, fontface = "italic", lineheight = 0.4) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_void() +
  coord_cartesian(clip = "off")

g_final <- g_base +
  inset_element(g_shape, left = 0, right = 1, top = 1, bottom = 0.66) +
  inset_element(g_us, left = 0.42, right = 1, top = 0.74, bottom = 0.33) +
  inset_element(g_day, left = 0, right = 0.66, top = 0.4, bottom = 0) +
  inset_element(quote1, left = 0.5, right = 1, top = 0.8, bottom = 0.72) +
  inset_element(quote2, left = 0, right = 1, top = 0.52, bottom = 0.4) +
  inset_element(quote3, left = 0.7, right = 1, top = 0.2, bottom = 0) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave(plot = g_final, filename = "scripts/2023/week-25-ufo/ufo.png", height = 16, width = 10)
