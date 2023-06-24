# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 24)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- colorspace::lighten("#FDB813", 0.9)
accent <- "grey20"
pal <- c(Muddaub = '#70543d', Burntbricks =  '#a24d39', Sunbricks = '#EAD1A8', Cement = '#D2D1CD')

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Josefin Sans", "jose")
showtext_auto()

ft <- "jose"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- dat$safi_data |>
  mutate(respondent_wall_type = str_to_title(str_trim(respondent_wall_type))) |>
  count(village, respondent_wall_type) |>
  group_by(village) |>
  mutate(
    p = n/sum(n),
    respondent_wall_type = factor(respondent_wall_type, c("Cement", "Sunbricks", "Burntbricks", "Muddaub"))
    ) |>
  arrange(respondent_wall_type)

df_village <- dat$safi_data |>
  count(village)

brick_row <- function(layer, width, brick_height = 1, brick_width = 2.5, gap = 0.125) {
  tibble(
    xmin = seq(1, width*brick_width, brick_width),
    xmax = xmin + brick_width - gap,
    ymin = 0 + layer,
    ymax = brick_height + layer - gap
  )
}

half_brick_row <- function(layer, width, brick_height = 1, brick_width = 2.5, gap = 0.125) {
  tibble(
    xmin = c(1, seq(1, width*brick_width, brick_width) + brick_width/2),
    xmax = c(1+brick_width/2, seq(1, (width-1)*brick_width, brick_width) + brick_width*3/2, brick_width*width+1)-gap,
    ymin = 0 + layer,
    ymax = brick_height + layer - gap
  )
}

build_wall <- function(height, width, brick_height = 1, brick_width = 2.5, gap = 0.125) {
  map_dfr(seq(0, (height-1)*brick_height, brick_height), ~{
    if(.x %% 2 == 0) {
      brick_row(.x, width, brick_height, brick_width, gap)
    } else {
      half_brick_row(.x, width, brick_height, brick_width, gap)
    }
  }) |>
    mutate(brick_id = 1:n())
}

build_wall(40, 6) |>
  ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
  coord_fixed()

df_wall <- map_dfr(1:nrow(df_village), ~{

  z <- df_base |>
    filter(village == df_village$village[.x])

  df <- build_wall(df_village$n[.x], 6) |>
    mutate(village = df_village$village[.x])

  wall_type <- map(1:nrow(z), function(i) {
    rep(z$respondent_wall_type[i], round(z$p[i]*nrow(df)))
    }) |>
    reduce(c)

  if(length(wall_type) < nrow(df)) {
    wall_type <- c(wall_type, tail(wall_type, 1))
  }

  df |>
    mutate(wall_type = wall_type)

})

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}SAFI survey")

# 📊 plot --------------------------------------------------------------------

df_wall |>
  ggplot(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = wall_type)) +
  geom_rect() +
  facet_wrap(~village, nrow = 1) +
  coord_fixed() +
  labs(
    title = "SAFI Survey",
    subtitle = "Studying African Farmer-Led Irrigation Survey
Wall type for each survey respondent by Village.",
    fill = "Wall type",
    caption = caption,
    y = "Number of respondents"
    ) +
  scale_fill_manual(values = pal) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.title = element_text(size = 128, face = "bold", margin = margin(b = 20)),
    plot.subtitle = element_text(margin = margin(b = 30)),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text.y = element_text(margin = margin(r = 30)),
    axis.title.y = element_text(angle = 90, margin = margin(r = 10)),
    strip.text = element_text(size = 64)
  )

ggsave("scripts/2023/week-24-safi/safi.png", height = 12, width = 12)
