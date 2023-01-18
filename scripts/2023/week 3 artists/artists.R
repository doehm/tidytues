# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(forcats)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 3)
artists <- dat$artists

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"

bg <- "black"
txt <- "grey50"
pal <- c("pink", "grey50", "grey50")

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
showtext_auto()

ft <- "mukta"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- artists |>
  filter(artist_gender %in% c("Male", "Female")) |>
  group_by(edition_number, artist_name, artist_gender) |>
  summarise(r = mean(space_ratio_per_page_total), .groups = "drop") |>
  mutate(
    artist_name = str_sub(artist_name, 1, 40),
    artist_name = fct_reorder(artist_name, r, max),
    x = ifelse(artist_gender == "Female", edition_number + 19, edition_number)
    )

df_gender <- df_base |>
  group_by(artist_gender) |>
  summarise(
    r = sum(r),
    n = n_distinct(artist_name),
    x = mean(x)
  ) |>
  ungroup() |>
  mutate(
    pr = paste0(round(r/sum(r), 2)*100, "%"),
    pn = round(n/sum(n), 2)*100,
    lab = paste(n, artist_gender, "artists /", pr, "coverage")
    )

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
caption <- glue("{mastodon} @danoehm@{space}fosstodon.org • {twitter} @danoehm • {github} doehm/tidytuesday • {floppy} arthistory data package")

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot(aes(x, artist_name, size = 8*r, colour = artist_gender)) +
  geom_text(aes(19, artist_name, label = artist_name), size = 11, family = ft) +
  geom_text(aes(x, -10, label = lab), df_gender, family = ft, size = 64) +
  geom_jitter() +
  scale_size_identity() +
  scale_colour_manual(values = pal) +
  labs(caption = caption) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 24, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t=80), size = 64),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "none",
)

ggsave("scripts/2023/week 3 artists/artists.png", height = 48, width = 30)
