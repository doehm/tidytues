# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggfx)
library(ggforce)
library(ggimage)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 50)
movies <- dat$holiday_movies

pal_xmas <- c('#1d463e', '#277d58', '#fec544', '#f05253', '#d1383a')

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- "grey20"

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add("xmas", regular = "assets/fonts/christmas_dream/Christmas Dream.ttf")
font_add_google("Barlow", "bar")
font_add_google("Barlow", "bar700", regular.wt = 700)
ft <- "bar"
ft700 <- "bar700"
ft_title <- "xmas"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------


df_base <- movies |>
  slice_max(num_votes, n = 20) |>
  mutate(
    d_mins = runtime_minutes - mean(runtime_minutes),
    d_rating = average_rating - mean(average_rating),
    std_rating = min_max(1/average_rating, 0, 0.5),
    x = runif(n(), 0.5-std_rating, 0.5+std_rating),
    y_text = breathing_space_on_y(average_rating, 0.1),
    cat = case_when(
      christmas ~ "Christmas",
      holiday ~ "Holiday",
      all(christmas, holiday) ~ "Christmas and Holiday"
    ),
    genres = str_replace(genres, ",", ", "),
    details = glue("{year}, {runtime_minutes} mins, {genres}")
  )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_image(aes(0.5, 6.7, image = "scripts/2023/week-50-christmas-movies/images/tree.png"), size = 1.2) +
  geom_segment(aes(x = 1.19, xend = x, y = y_text, yend = average_rating), linewidth = 0.2, colour = txt) +
  with_outer_glow(
    geom_point(aes(x, average_rating, size = num_votes, colour = cat)),
  colour = "black",
  sigma = 10
  ) +
  geom_point(aes(1.19, y_text), colour = txt) +
  geom_text(aes(1.2, y_text, label = primary_title), family = ft700, size = 12, colour = txt, hjust = 0, fontface = "italic") +
  geom_text(aes(1.2, y_text-0.045, label = details), family = ft, size = 8, colour = txt, hjust = 0, fontface = "italic") +
  geom_text(aes(x, average_rating, label = average_rating), family = ft700, size = 12, colour = txt, hjust = 0.1, fontface = "italic") +
  geom_segment(aes(x = 0, xend = 1.19, y = mean(movies$average_rating), yend = mean(movies$average_rating)), linetype = 2, linewidth = 0.6, colour = pal_xmas[2]) +
  annotate("text", x = 1.19, y = mean(movies$average_rating)+0.045, label = "Overall average", hjust = 1, family = ft, colour = pal_xmas[2], fontface = "italic", size = 12) +
  scale_size_binned(range = c(10, 25)) +
  scale_y_continuous(breaks = seq(5, 8.5, 0.5)) +
  scale_colour_manual(values = pal_xmas[c(3, 5)]) +
  xlim(0, 1.6) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Christmas Movies",
    subtitle = "Looking for a good Christmas movie to watch? Here are the 20 most popular movies (the most votes) and their IMDb rating.",
    caption = caption,
    size = "Number of votes",
    colour = "Title contains...",
    y = "IMDb\nrating"
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 256, hjust = 0.5, family = ft_title, face = "bold", colour = pal_xmas[5]),
    plot.subtitle = element_text(margin = margin(t = 15), hjust = 0.5),
    plot.caption = element_markdown(colour = txt, hjust = 1, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text.y = element_text(margin = margin(r = 10)),
    axis.ticks.y = element_line(linetype = 1, linewidth = 0.5),
    axis.ticks.length.y = unit(0.2, "cm"),
    axis.line.y = element_line(),
    axis.title.y = element_text(margin = margin(r = 10))
  )

ggsave("scripts/2023/week-50-christmas-movies/christmas-movies.png", height = 12, width = 18)

