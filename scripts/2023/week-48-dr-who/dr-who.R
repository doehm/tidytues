# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(cropcircles)
library(ggimage)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 48)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- "grey20"
point <- "grey"
pal <- c('#081055', '#122E8D', '#91969B', '#C9CBCC', '#F0E194', '#BBAE78', '#6E7073', '#464C60')
pal_rating <- pal[c(5, 6, 2, 1)]

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
ft <- "bar"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df <- dat$drwho_episodes |>
  rename(
    season = season_number,
    episode = episode_number
  ) |>
  mutate(focus = season) |>
  filter(!is.na(season))

df_doctors <- tribble(
  ~season, ~doctor,
  1, "Christopher Eccleston",
  2, "David Tennant",
  3, "David Tennant",
  4, "David Tennant",
  5, "Matt Smith",
  6, "Matt Smith",
  7, "Matt Smith",
  8, "Peter Capaldi",
  9, "Peter Capaldi",
  10, "Peter Capaldi",
  11, "Jodie Whittaker",
  12, "Jodie Whittaker",
  13, "Jodie Whittaker"
)

df_mean <- df |>
  group_by(season) |>
  summarise(
    uk_viewers = mean(uk_viewers),
    rating = mean(rating)
  ) |>
  left_join(df_doctors, by = "season") |>
  mutate(
    rating_scaled = round(min_max(rating, 1, 100)),
    col = colorRampPalette(pal_rating)(100)[rating_scaled],
    img = glue("scripts/2023/week-48-dr-who/images/{doctor}.jpg"),
    img = crop_circle(img, border_size = 6, border_colour = col, just = "top"),
    focus = season
    )

df_all <- map_dfr(1:13, ~{
  df |>
    mutate(focus = .x)
}) |>
  bind_rows(tibble(focus = c(-1,0)))

df_title <- tibble(
  focus = -1,
  x = mean(df$rating),
  y = mean(df$uk_viewers)*1.3,
  img = "scripts/2023/week-48-dr-who/images/logo.png"
)

df_subtitle <- tibble(
  x = range(df$rating)[1],
  y = range(df$uk_viewers)[2]*0.85,
  focus = 0,
  lab = "Doctor Who episode ratings by the number of UK viewers. Generally, the better the episode
  the more viewers, but not always e.g. season 1 and 11."
)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

# 📊 plot --------------------------------------------------------------------

df_all |>
  ggplot() +
  geom_rect(aes(xmin = range(df$rating)[1]*0.99, xmax = range(df$rating)[2]*1.01, ymin = range(df$uk_viewers)[1]*0.99, ymax = range(df$uk_viewers)[2]*1.01), df_mean,
            fill = df_mean$col, alpha = 0.1) +
  geom_point(aes(rating, uk_viewers), alpha = 0.5, colour = point, size = 1) +
  geom_point(aes(rating, uk_viewers, colour = rating), df, size = 2) +
  geom_image(aes(rating, uk_viewers, image = img), df_mean, size = 0.15) +
  geom_image(aes(x, y, image = img), df_title, size = 1.3) +
  geom_text(aes(range(df$rating)[1], range(df$uk_viewers)[2]*0.98, label = paste("Season", season)), df_mean, family = ft, colour = txt, size = 16, fontface = "bold", vjust = 1, hjust = 0) +
  geom_text(aes(x, y, label = str_wrap(lab, 25)), df_subtitle, family = ft, colour = txt, size = 16, vjust = 1, hjust = 0, lineheight = 0.3) +
  scale_y_continuous(breaks = seq(3, 12, 3), labels = paste0(seq(3, 12, 3), "m"), position = "right") +
  facet_wrap(~focus, nrow = 3) +
  scale_colour_gradientn(colours = pal_rating) +
  coord_cartesian(clip = "off") +
  labs(
    caption = caption,
    x = "Rating",
    y = "Viewers",
    colour = "Rating"
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text = element_text(),
    strip.text = element_blank(),
    legend.position = "bottom"
  )

ggsave("scripts/2023/week-48-dr-who/dr-who.png", height = 12, width = 20)
