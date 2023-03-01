# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(gggibbous)
library(forcats)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 9)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "grey80"
accent <- "#15616d"

pal <- c(Positive = "#15616d", Neutral =  "grey50", Negative = "#ff7d00")

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
showtext_auto()

ft <- "mukta"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- dat$afrisenti |>
  left_join(dat$language_countries) |>
  count(country, label) |>
  mutate(
    country = factor(country),
    label = snakecase::to_title_case(label),
    label = factor(label, level = c("Positive", "Neutral", "Negative")),
    sort_id = 3-as.numeric(label)
    ) |>
  arrange(country, sort_id) |>
  group_by(country) |>
  mutate(
    p = n/sum(n),
    cm = cumsum(p),
    # cm0 = 1-cm,
    y_text = ifelse(is.na(lag(cm)), 0, lag(cm)) + p/2
    ) |>
  ungroup() |>
  mutate(
    middle = ifelse(label == "Neutral", p, 0),
    pct = paste0(round(p, 2)*100, "%")
    ) |>
  filter(country != "Eswatini") |>
  mutate(
    cty = str_replace(tolower(country), "[:space:]", "-"),
    flag = glue("C:/Users/Dan/Documents/Assets/flag-png-round-medium/{cty}_flag-png-round-medium.png")
    )

df_total <- dat$afrisenti |>
  left_join(dat$language_countries) |>
  filter(country != "Eswatini") |>
  count(country) |>
  mutate(n = paste0(country, "\n", round(n/1000, 0), "k"))

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}AfriSenti via @shmuhammad2004")

subtitle <- "Sentiment analysis of tweets from 12 African countries. Most tweets recorded from Nigeria at 64k"

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_col(aes(3, p, fill = label, colour = label), width = 1, alpha = 0.9) +
  geom_from_path(aes(0, 0, path = flag), distinct(df_base, country, flag), width = 0.3) +
  geom_text(aes(4.4, y_text, label = pct), family = ft, size = 12, colour = txt) +
  geom_text(aes(5, 1, label = n), df_total, family = ft, size = 12, colour = txt, lineheight = 0.3) +
  facet_wrap(~country) +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  xlim(0, 5) +
  coord_polar("y", clip = "off") +
  labs(
    title = "AfriSenti: Sentiment Analysis",
    subtitle = subtitle,
    caption = caption,
    fill = "Sentiment",
    colour = "Sentiment"
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 84, face = "bold", margin = margin(b = 10), hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 10)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 0, t = 0, r = 30, l = 30),
    # strip.text = element_text(margin = margin(t = 5, b = 5)),
    strip.text= element_blank(),
    legend.position = "bottom"
  )

ggsave("scripts/2023/week 9 afrisenti/afrisenti.png", height = 12, width = 12)

