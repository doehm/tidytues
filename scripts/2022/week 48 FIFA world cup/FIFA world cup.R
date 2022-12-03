# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggpath)
library(lubridate)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2022, week = 48)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "white"
bg <- "grey20"
line <- "grey40"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/webfonts/fa-solid-900.ttf")
font_add("fifa", regular = "assets/fonts/fifa/qatar2022arabic-heavy.ttf")
font_add_google("Mukta", "mukta")
showtext_auto()

ft <- "mukta"

# 🤼 wrangle -----------------------------------------------------------------

top_20 <- dat$wcmatches |>
  filter(year(date) > 1990) |>
  select(home_team, away_team) |>
  pivot_longer(everything()) |>
  count(value) |>
  arrange(desc(n)) |>
  head(20) |>
  mutate(country = factor(value))

df_base <- dat$wcmatches |>
  mutate(d = home_score - away_score) |>
  group_by(country = home_team) |>
  summarise(
    n_home = n(),
    d_mean_home = mean(d),
    d_sd_home = sd(d),
    n_wins_home = sum(outcome == "H")
  ) |>
  mutate(p_wins_home = n_wins_home/n_home) |>
  semi_join(top_20, by = "country") |>
  arrange(desc(d_mean_home)) |>
  left_join(
    dat$wcmatches |>
      mutate(d = away_score - home_score) |>
      group_by(country = away_team) |>
      summarise(
        n_away = n(),
        d_mean_away = mean(d),
        d_sd_away = sd(d),
        n_wins_away = sum(outcome == "A")
      ) |>
      mutate(p_wins_away = n_wins_away/n_away)
  ) |>
  mutate(
    y = n():1,
    country = case_when(
      country == "United States" ~ "United States of America",
      TRUE ~ country
    ),
    cntry = str_replace_all(tolower(country), " ", "-"),

    # flags from https://flagpedia.net/download
    flag = glue("C:/Users/Dan/Documents/Assets/flag-png-round-medium/{cntry}_flag-png-round-medium.png")
  )

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
caption <- glue("{mastodon} @danoehm@{space}fosstodon.org • {twitter} @danoehm • {github} doehm/tidytuesday • {floppy} Kaggle FIFA World Cup")

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_hline(yintercept = 0, colour = txt) +
  geom_vline(xintercept = 0, colour = txt) +
  geom_from_path(aes(d_mean_home, d_mean_away, path = flag), width = 0.05) +
  annotate(
    "richtext", -1.5, 2, label = "Mean difference<br>for away games", label.colour = NA,
    fill = bg, family = ft, colour = txt, size = 18, lineheight = 0.3, hjust = 0) +
  xlim(-1.5, 1.5) +
  ylim(-2, 2) +
  labs(
    caption = caption,
    title = "FIFA World Cup",
    subtitle =
      "Mean difference in scores for home and away games for the 20 countries
      <br>with the most games played since 1990. Belgium, Sweden and Argentina
      <br>tend to win more games at home suggesting a home ground advantage",
    x = "Mean difference for home games"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 48),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(family = "fifa", size = 128),
    plot.subtitle = element_markdown(margin = margin(b = 30, t = 10), lineheight = 0.35, halign = 0),
    plot.margin = margin(t = 50, b = 30, l = 50, r = 80),
    plot.caption = element_markdown(size = 36, hjust = 0.5, margin = margin(t = 20)),
    axis.title.x = element_text(),
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    axis.ticks = element_line(colour = line),
    axis.line = element_line(colour = line, arrow = arrow(type = "closed", length = unit(0.15, "inches"))),
    panel.grid = element_line(colour = line, linetype = 3)
  )

ggsave("2022/week 48 FIFA world cup/FIFA world cup.png", height = 12, width = 12)
