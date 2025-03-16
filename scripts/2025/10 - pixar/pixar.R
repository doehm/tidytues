# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggchicklet)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

pixar_films <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/pixar_films.csv')
public_response <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-11/public_response.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "grey20"
bg <- "white"
accent <- txt

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
autumn <- c('#1c2426', '#455852', '#9d8e71', '#d4935d', '#cc612b', '#a5371e', '#ab443b')

pal <- c('#f8ebcc', '#ebd2a2', '#d7be90', '#c4ad82')
pal <- c('grey90', colorRampPalette(eyedroppeR::pencil_case$bright$cat)(6))

font_add_google("Poppins", "pop", regular.wt = 200)
font_add_google("Josefin Sans", "jose")
font_add_google("Fredericka the Great", "ftg")
ft <- "pop"
ft1 <- "ftg"
ft2 <- "jose"
showtext_auto()

# 🚙 functions ---------------------------------------------------------------

build_cinema_strip <- function(
    strip_id = 1,
    rt = 0,
    xlim_min = 40,
    xlim_max = 100,
    n_frames = 6,
    n_dots = 7*n_frames,
    d = 0.5,
    d_dots = 0.25,
    dots = 0.18
    ) {

  strip <- tibble(
      strip_id = strip_id,
      xmin = xlim_min,
      xmax = xlim_max,
      ymin = 0,
      ymax = 1
    )

  frame <- map2_dfr(strip_id, rt, ~{
    tibble(
      strip_id = .x,
      xmin = seq(xlim_min, xlim_max, length = n_frames+1)[-(n_frames+1)]+d,
      xmax = seq(xlim_min, xlim_max, length = n_frames+1)[-1]-d,
      ymin = 0+dots,
      ymax = 1-dots,
      fill = .y
    )
  }) |>
    mutate(fill = as.factor(ifelse(fill > xmin & fill <= xmax, fill, 0)))


  tracks_top <- map_dfr(strip_id, ~{
    tibble(
      strip_id = .x,
      xmin = seq(xlim_min, xlim_max, length = n_dots+1)[-(n_dots+1)]+d_dots,
      xmax = seq(xlim_min, xlim_max, length = n_dots+1)[-1]-d_dots,
      ymin = 1-dots*0.8,
      ymax = 1-dots*0.2
    )
  })

  tracks_bottom <- map_dfr(strip_id, ~{
    tibble(
      strip_id = .x,
      xmin = seq(xlim_min, xlim_max, length = n_dots+1)[-(n_dots+1)]+d_dots,
      xmax = seq(xlim_min, xlim_max, length = n_dots+1)[-1]-d_dots,
      ymin = dots*0.2,
      ymax = dots*0.8
    )
  })

  tracks <- bind_rows(
    tracks_top,
    tracks_bottom
  )

  list(
    strip = strip,
    frame = frame,
    tracks = tracks
  )

}

# 🤼 wrangle -----------------------------------------------------------------

df_rotten <- public_response |>
  mutate(
    rt = ifelse(rotten_tomatoes == 100, 99, rotten_tomatoes),
    rt = floor(rt/10)*10+5,
    alpha = rotten_tomatoes - rt + 5,
    txt = ifelse(rt < 80, "white", txt)
  ) |>
  left_join(pixar_films, join_by(film)) |>
  mutate(
    strip_id = film,
    film_rating_url = case_when(
      film_rating == "G" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/e/eb/Australian_Classification_General_%28G%29.svg/1920px-Australian_Classification_General_%28G%29.svg.png",
      film_rating == "PG" ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1d/Australian_Classification_Parental_Guidance_%28PG%29.svg/1920px-Australian_Classification_Parental_Guidance_%28PG%29.svg.png",
      TRUE ~ NA
    )
  ) |>
  filter(!is.na(rt))
  # filter(film_rating == "PG")

df <- build_cinema_strip(df_rotten$film, df_rotten$rt)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, bg)

# 📊 plot --------------------------------------------------------------------

ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), df$strip, fill = "black") +
  ggchicklet:::geom_rrect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill), df$frame) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), df$tracks, fill = "white") +
  geom_text(aes(rt, 0.5, label = str_wrap(film, 10), colour = txt), df_rotten, family = ft2, size = 14, lineheight = 0.3, fontface = "bold") +
  geom_text(aes(rt, 0.25, label = year(release_date), colour = txt), df_rotten, family = ft, size = 10, lineheight = 0.3) +
  geom_text(aes(rt+4, 0.75, label = glue("{run_time} min"), colour = txt), df_rotten, family = ft, size = 10, lineheight = 0.3, hjust = 1) +
  geom_text(aes(rt-4, 0.75, label = cinema_score, colour = txt), df_rotten, family = ft, size = 10, lineheight = 0.3, hjust = 0) +
  geom_from_path(aes(rt+3.5, 0.28, path = film_rating_url), df_rotten, width = 0.025) +
  scale_x_continuous(breaks = seq(40, 100, 10), labels = seq(40, 100, 10)) +
  scale_fill_manual(values = pal) +
  scale_colour_identity() +
  facet_wrap(~strip_id, ncol = 2) +
  labs(
    caption = caption,
    x = "Rotten Tomatoes Score",
    title = "Pixar Films",
    subtitle = "16/23 Pixar films have a Rotten Tomatoes rating >90. Cinema Score in the top left."
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    # plot.background = element_rect(fill = grid::radialGradient(c("white", "grey80"), r1 = unit(0.8, "npc")), colour = bg),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 300, hjust = 0.5, family = ft1),
    plot.subtitle = element_text(margin = margin(t = 10, b = 10), hjust = 0.5),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 70, l = 70),
    strip.text = element_blank(),
    axis.text.x = element_text(),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "none"
  )

ggsave("scripts/2025/10 - pixar/pixar.png", height = 23, width = 20)
