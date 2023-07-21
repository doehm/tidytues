# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(lubridate)

# 💾 load data ---------------------------------------------------------------

global_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/global_temps.csv')
nh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/nh_temps.csv')
sh_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/sh_temps.csv')
zonann_temps <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-11/zonann_temps.csv')

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "white"
bg <- "black"
pal <- c('#08306b', '#08519c', '#2171b5', '#4292c6', '#6baed6', '#9ecae1', '#c6dbef', '#deebf7', '#ffffff',
         '#fee0d2', '#fcbba1', '#fc9272', '#fb6a4a', '#ef3b2c', '#cb181d', '#a50f15', '#67000d')

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
showtext_auto()

ft <- "bar"

# 🤼 wrangle -----------------------------------------------------------------

clean_data <- function(df) {
  df |>
    select(1:14) |>
    pivot_longer(-c(series, Year), names_to = "month", values_to = "temp") |>
    clean_names() |>
    group_by(series) |>
    mutate(
      ym = ymd(paste0(year, "-", month, "-01")),
      t = 1:n()
    ) |>
    drop_na()
}

df_base <- bind_rows(
  "Global" = global_temps,
  "Northern hemisphere" = nh_temps,
  "Southern hemisphere" = sh_temps,
  .id = "series"
  ) |>
  clean_data()

df_years <- df_base |>
  filter(
    series == "Global",
    year %in% seq(1900, 2000, 25),
    month == "Jan"
    )

df_base |>
  group_by(year) |>
  summarise(temp = mean(temp)) |>
  arrange(desc(temp))

# 🔡 text --------------------------------------------------------------------

pal_cap <- pal[seq(2, 16, length = 4)]
mastodon <- glue("<span style='font-family:fa-brands; color:{pal_cap[1]}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{pal_cap[2]}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{pal_cap[3]}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{pal_cap[4]}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>-</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2} NASA GISS Surface Temperature Analysis")

subtitle <- "Global surface temperatures have been steadily increasing.
So far, 2023 is the 3rd hottest year on record and this data is yet to include Summer
for the Northern Hemisphere."

# 📊 plot --------------------------------------------------------------------

df_base |>
  filter(series == "Global") |>
  mutate(y = min_max(temp, 0, 1)) |>
  ggplot() +
  geom_col(aes(t, 1, fill = temp), width = 1) +
  annotate("text", x = 860, y = 0.5, label = "Global Surface Temperatures", family = ft, size = 48, colour = bg, fontface = "bold") +
  geom_text(aes(t, 0.45, label = year), df_years, family = ft, size = 16, colour = bg) +
  annotate("segment", x = 241, xend = 1441, y = 0.47, yend = 0.47, colour = bg) +
  geom_point(aes(t, 0.47), df_years, colour = bg) +
  scale_fill_gradientn(colours = pal) +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 36, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(margin = margin(t=-20), hjust = 0.5),
    plot.margin = margin(b = 20),
    legend.position = "none"
  )

ggsave("scripts/2023/week-28-temperatures/temperatures.png", height = 18, width = 10)

df_base |>
  filter(series == "Northern hemisphere") |>
  mutate(y = min_max(temp, 0, 1)) |>
  ggplot() +
  geom_col(aes(t, 1, fill = temp), width = 1) +
  annotate("text", x = 860, y = 0.5, label = "Northern Hemisphere", family = ft, size = 48, colour = bg, fontface = "bold") +
  geom_text(aes(t, 0.45, label = year), df_years, family = ft, size = 16, colour = bg) +
  annotate("segment", x = 241, xend = 1441, y = 0.47, yend = 0.47, colour = bg) +
  geom_point(aes(t, 0.47), df_years, colour = bg) +
  scale_fill_gradientn(colours = pal) +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 36, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(margin = margin(t=-20), hjust = 0.5),
    plot.margin = margin(b = 20),
    legend.position = "none"
  )

ggsave("scripts/2023/week-28-temperatures/nh-temperatures.png", height = 18, width = 10)

df_base |>
  filter(series == "Southern hemisphere") |>
  mutate(y = min_max(temp, 0, 1)) |>
  ggplot() +
  geom_col(aes(t, 1, fill = temp), width = 1) +
  annotate("text", x = 860, y = 0.5, label = "Southern Hemisphere", family = ft, size = 48, colour = bg, fontface = "bold") +
  geom_text(aes(t, 0.45, label = year), df_years, family = ft, size = 16, colour = bg) +
  annotate("segment", x = 241, xend = 1441, y = 0.47, yend = 0.47, colour = bg) +
  geom_point(aes(t, 0.47), df_years, colour = bg) +
  scale_fill_gradientn(colours = pal) +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 36, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(margin = margin(t=-20), hjust = 0.5),
    plot.margin = margin(b = 20),
    legend.position = "none"
  )

ggsave("scripts/2023/week-28-temperatures/sh-temperatures.png", height = 18, width = 10)

txt1 <- colorspace::darken(txt, 0.7)
df_base |>
  filter(series == "Global") |>
  ggplot() +
  geom_col(aes(t, temp, fill = temp), width = 1) +
  annotate("rect", xmin = 0, xmax = 1722, ymin = -0.025, ymax = 0.025, fill = bg) +
  geom_text(aes(t, 0, label = year), df_years, family = ft, size = 16, colour = txt) +
  annotate("text", x = 1, y = 1.25, label = "Global Surface\nTemperatures", family = ft, size = 48, colour = txt, hjust = 0, fontface = "bold", lineheight = 0.3, vjust = 1) +
  annotate("text", x = 1, y = 1.05, label = str_wrap(subtitle, 32), family = ft, size = 24, colour = txt, hjust = 0, lineheight = 0.3, vjust = 1) +
  scale_y_continuous(breaks = seq(-0.75, 1.25, 0.25), position = "right") +
  scale_x_continuous(breaks = df_years$t, labels = df_years$year) +
  scale_fill_gradientn(colours = pal) +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 36, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(margin = margin(t=20), hjust = 0.5),
    axis.text.y = element_text(family = ft, size = 48, colour = txt, margin = margin(t=5, b=5, l=5, r=5)),
    axis.ticks.y = element_line(colour = txt1),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "none"
  )

ggsave("scripts/2023/week-28-temperatures/hist.png", height = 18, width = 10)
