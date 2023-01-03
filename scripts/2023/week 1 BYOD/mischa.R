# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(glue)
library(ggtext)
library(lubridate)
library(ggpubr)
library(magick)

# 💾 load data ---------------------------------------------------------------

pantry <- image_read("scripts/2023/week 1 BYOD/pantry.jpg")
pantry_blurred <- image_blur(pantry, 25, 30)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"

pal1 <- colorRampPalette(c('#91A0B2', '#6C6C6D', "white"))(16)

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Covered By Your Grace", "grace")
font_add_google("Rock Salt", "salt")
showtext_auto()

ft <- "grace"

# 🤼 wrangle -----------------------------------------------------------------

df_mischa <- tribble(
  ~date, ~height,
  "2020-11-04", 50,
  "2021-11-04", 73,
  "2022-01-07", 77,
  "2022-03-11", 78,
  "2022-05-08", 81,
  "2022-08-21", 83,
  "2022-11-04", 84
) |>
  mutate(
    date = ymd(date),
    date_lab = paste0(round(height),"cm  ", format(date, "%d %b %Y"))
    )

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
caption <- glue(
"{mastodon} @danoehm@{space}fosstodon.org<br>
{github} doehm/tidytues<br>
{floppy} The pantry<br>
{twitter} @danoehm")

# 📊 plot --------------------------------------------------------------------

a <- -10

df_mischa |>
  ggplot() +
  background_image(pantry_blurred) +
  geom_rect(aes(xmin = -2, xmax = 5, ymin = 0, ymax = 90), fill = pal1[13]) +
  geom_rect(aes(xmin = -1.9, xmax = -0.3, ymin = 0, ymax = 90), fill = pal1[12]) +
  geom_rect(aes(xmin = -0.3, xmax = -0.18, ymin = 0, ymax = 90), fill = pal1[10]) +
  geom_rect(aes(xmin = -1.9, xmax = 5, ymin = 0, ymax = 8), fill = pal1[12]) +
  geom_segment(aes(x = -0.3, xend = -0.3, y = 0, yend = 8), colour = pal1[10]) +
  geom_segment(aes(x = -0.3, xend = 5, y = 8, yend = 8), colour = pal1[11], size = 1) +
  geom_segment(aes(x = 0, xend = 1, y = height, yend = height), colour = txt) +
  geom_text(aes(x = 1.25, y = height, label = date_lab), family = ft, colour = txt, size = 20, hjust = 0) +
  annotate("text", x = 0.4, y = 13, label = "MISCHA", family = "salt", size = 40,
           lineheight = 0.32, colour = txt, hjust = 0, fontface = "bold") +
  annotate("richtext", x = 4.8, y = 4, label = caption, family = ft, size = 16,
           lineheight = 0.28, colour = txt, hjust = 1, label.color = NA, fill = NA) +
  xlim(-5, 5) +
  ylim(0, 90) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 48),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(hjust = 0.5),
    plot.title = element_text(family = ft, size = 128, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(family = ft, size = 48, colour = txt, hjust = 0.5),
    plot.margin = margin(b = a, t = a, r = a, l = a)
  )

ggsave("2023/week 1 BYOD/mischa.png", height = 16, width = 8)

