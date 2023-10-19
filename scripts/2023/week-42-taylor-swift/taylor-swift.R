# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(magick)
library(ggforce)
library(ggpath)
library(ggbump)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 42)
album_songs <- dat$taylor_album_songs
albums <- dat$taylor_albums

eras <- read_csv("scripts/2023/week-42-taylor-swift/eras-tour-set-list.txt")

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "black"
bg <- "white"
accent <- "grey20"
pal <- c('#2B1319', '#5A2E47', '#813C6F', '#A55589', '#F2E0E3', '#E0BAB5', '#C58C91', '#A65D47')
pal_lover <- c('#6A5C56', '#5B98C7', '#9CB9D9', '#CAC3DA', '#F4D1D8', '#F2B5D3', '#AFA2AD', '#9C7D7B')
pal_purp <- colorRampPalette(pal[4:5])(16)
pal_purp <- pal[c(5,3)]

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add("rojal", regular = "assets/fonts/taylor-swift/Rojal.ttf")
font_add("pist", regular = "assets/fonts/taylor-swift/Pistilli-Roman.otf")
font_add_google("Barlow", "bar")
font_add_google("Dancing Script", "dancing")
font_add_google("Cormorant Garamond", "corm")
ft <- "corm"
ft1 <- "dancing"
ft2 <- "rojal"
ft3 <- "pist"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_n_tracks <- album_songs |>
  count(album_name, name = "n_tracks")

df_records <- albums |>
  filter(
    !ep,
    !album_name %in% c("Fearless", "Red")
    ) |>
  mutate(
    album_covers = glue("scripts/2023/week-42-taylor-swift/albums/{album_name}.png"),
    y = n():1
    ) |>
  left_join(df_n_tracks, by = "album_name") |>
  mutate(n_tracks = replace_na(n_tracks, 1)) |>
  left_join(
    album_songs |>
      group_by(album_name) |>
      summarise(length = to_hm(sum(duration_ms, na.rm = TRUE)/1000)),
    by = "album_name"
  )

df_box <- album_songs |>
  mutate(
    mu_tempo = mean(tempo, na.rm = TRUE),
    mu_energy = mean(energy, na.rm = TRUE),
    mu_valence = mean(valence, na.rm = TRUE),
    mu_danceability = mean(danceability, na.rm = TRUE)
  ) |>
  group_by(album_name) |>
  summarise(
    lab_tempo = ifelse(mean(tempo, na.rm = TRUE) > mu_tempo[1], pal_purp[2], pal_purp[1]),
    lab_energy = ifelse(mean(energy, na.rm = TRUE) > mu_energy[1], pal_purp[2], pal_purp[1]),
    lab_valence = ifelse(mean(valence, na.rm = TRUE) > mu_valence[1], pal_purp[2], pal_purp[1]),
    lab_danceability = ifelse(mean(danceability, na.rm = TRUE) > mu_danceability[1], pal_purp[2], pal_purp[1])
  ) |>
  pivot_longer(starts_with("lab"), values_to = "col", names_to = "box") |>
  group_by(album_name) |>
  mutate(
    x_box = c(0, 0, 1, 1)*0.5,
    y_box = c(0, 1, 0, 1)*0.5*0.25
  ) |>
  left_join(
    df_records |>
      select(album_name, metacritic_score, y),
    by = "album_name"
  )

df_eras <- eras |>
  left_join(album_songs, by = "track_name") |>
  left_join(
    df_records |>
      select(album_name, y, metacritic_score),
    by = "album_name"
  ) |>
  mutate(y1 = seq(nrow(df_records), 1, length = n()))

df_bump <- df_eras |>
  select(track_name, metacritic_score, energy, y, y1) |>
  pivot_longer(c(y, y1)) |>
  mutate(x = ifelse(name == "y1", 105, metacritic_score+3))

offset <- 0.03
a <- 5
df_tracks <- map_dfr(1:nrow(df_records), ~{
  df_records[rep(.x, df_records$n_tracks[.x]),]
  }) |>
  group_by(album_name) |>
  mutate(
    a = seq(a*0.45/3+4*offset, a*0.45-4*offset, length = n()),
    b = seq(0.45/3+offset, 0.45-offset, length = n())
  )

  df_era_segment <- df_eras |>
  group_by(album_name) |>
  summarise(
    ymin = min(y1)-0.075,
    ymax = max(y1)+0.075
  ) |>
  drop_na()

df_aixs <- tibble(
  x = seq(60, 90, 10),
  y = 10.6
)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

# 📊 plot --------------------------------------------------------------------

rd <- 3

g_base <- df_records |>
  ggplot() +

  annotate("segment", x = 60, xend = 90, y = 10.6, yend = 10.6, colour = txt, linetype = 3) +
  geom_richtext(aes(x, y, label = x), df_aixs, colour = txt, family = ft, size = 14, label.colour = NA) +

  geom_ellipse(aes(x0 = metacritic_score, y0 = y, a = a*0.45, b = 0.45, angle = 0), fill = "grey10") +
  geom_ellipse(aes(x0 = metacritic_score, y0 = y, a = a*0.45/3, b = 0.45/3, angle = 0, fill = user_score/100)) +
  geom_ellipse(aes(x0 = metacritic_score, y0 = y, a = a, b = b, angle = 0), df_tracks, fill = NA, colour = "grey30", linewidth = 0.1) +
  geom_bump(aes(x = x, y = value, group = track_name, colour = energy), df_bump) +
  geom_point(aes(x = x, y = value, colour = energy), filter(df_bump, name == "y1"), size = 8) +
  geom_text(aes(x = 107, y = y1, label = track_name), df_eras, family = ft, size = 18, hjust = 0, colour = txt, fontface = "italic") +
  geom_text(aes(x = x, y = value, label = round(energy, 2)), filter(df_bump, name == "y1", !is.na(energy)), family = ft, size = 10, hjust = 0.5, colour = bg, fontface = "italic") +
  geom_text(aes(x = metacritic_score-5, y = y+0.35, label = album_name), family = ft1, size = 28, hjust = 1, colour = txt, fontface = "bold") +
  geom_text(aes(x = metacritic_score-5, y = y+0.1, label = length), family = ft, size = 20, hjust = 1, colour = txt) +
  geom_text(aes(x = metacritic_score+0.3, y = y, label = user_score), family = ft, size = 14, colour = txt, angle = 270) +

  # era segment
  geom_segment(aes(x = 106, xend = 106, y = ymin, yend = ymax), df_era_segment, colour = txt, linewidth = 4) +

  # titles
  annotate("text", x = 40, y = 2.75, label = "TAYLOR SWIFT", family = ft2, size = 200, colour = txt, hjust = 0, vjust = 1) +
  annotate("text", x = 40, y = 1.5, label = "THE ERAS TOUR", family = ft3, size = 79, colour = txt, hjust = 0, vjust = 1) +
  annotate("text", x = 40, y = 0.9, label = "The Eras tour setlist and discography analysis", family = ft, size = 18, colour = txt, hjust = 0, vjust = 1) +
  annotate("text", x = 107, y = 10.4, label = "Setlist", family = ft3, size = 32, colour = txt, hjust = 0) +
  annotate("text", x = 105, y = 10.4, label = "Energy", family = ft, size = 20, colour = txt, hjust = 0.5) +
  annotate("text", x = 59, y = 10.6, label = "Metacritic score", family = ft, size = 14, colour = txt, hjust = 1) +

  # album images
  geom_from_path(aes(x = metacritic_score-2.2, y = y, path = album_covers), width = 0.05) +

  # box
  geom_point(aes(metacritic_score-5-x_box-0.2, y+y_box-0.3), df_box, size = 5, colour = df_box$col) +

  # legend
  geom_from_path(aes(x = 54, y = 4.5, path = "scripts/2023/week-42-taylor-swift/legend.png"), width = 0.2) +

  # the rest
  scale_color_gradientn(colours = pal_purp, na.value = "white") +
  scale_fill_gradientn(colours = pal_purp, na.value = "white") +
  labs(caption = caption) +
  xlim(40, 120) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 20),
    legend.position = "none"
  )

ggsave("scripts/2023/week-42-taylor-swift/taylor-swift.png", plot = g_base, height = 18, width = 30)

