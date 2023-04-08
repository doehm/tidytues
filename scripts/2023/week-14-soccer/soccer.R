# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggfx)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 14)

soccer <- dat$`soccer21-22` |>
  clean_names()

# Thank you @issa_madjid for the logos!
# If you haven't checked out his work you absolutely should
# https://twitter.com/issa_madjid
df_logos <- read_csv("scripts/2023/week-14-soccer/logos.csv")

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
pal <- c('#ff3e35', '#e0f342')
accent <- txt

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
font_add_google("Anton", "ant")
showtext_auto()

ft <- "mukta"
ft1 <- "ant"

# 🤼 wrangle -----------------------------------------------------------------

gr <- 1.34
wd <- 2.6
ht <- wd*gr
ht1 <- 2*1.2

df_teams <- bind_rows(
  soccer |>
    group_by(team = home_team) |>
    summarise(
      red = sum(hr),
      yellow = sum(hy),
      n_games = n()
    ),
  soccer |>
    group_by(team = away_team) |>
    summarise(
      red = sum(ar),
      yellow = sum(ay),
      n_games = n()
    )
  ) |>
  group_by(team) |>
  summarise(
    red = sum(red),
    yellow = sum(yellow),
    n_games = sum(n_games)
  ) |>
  ungroup() |>
  arrange(yellow, red) |>
  left_join(df_logos, by = "team") |>
  mutate(y = ht*ht1*(1:n()) + ht)

df_red <- map_dfr(df_teams$team, ~{
  df_x <- df_teams |>
    filter(team == .x)
  y_nudge <- rnorm(df_x$red, 0, 0.2)
  tibble(
    team = .x,
    xmin = 1:df_x$red,
    xmax = xmin + wd,
    ymin = ht*ht1*which(df_teams$team == .x) + y_nudge,
    ymax = ht*ht1*which(df_teams$team == .x) + ht + y_nudge
  )
})

df_yellow <- map_dfr(df_teams$team, ~{
  df_x <- df_teams |>
    filter(team == .x)
  y_nudge <- rnorm(df_x$yellow, 0, 0.2)
  tibble(
    team = .x,
    xmin = 1:df_x$yellow,
    xmax = xmin + wd,
    ymin = ht*ht1*which(df_teams$team == .x) + y_nudge + ht*ht1/2,
    ymax = ht*ht1*which(df_teams$team == .x) + ht + y_nudge + ht*ht1/2
  )
})


# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}Premier League Match Data 2021-2022")

# went with read and yellow colours but it looked a bit janky
sub <- glue("<span style='color:{txt}'>Red</span> and <span style='color:{txt}'>yellow</span> card tally for the season")

# 📊 plot --------------------------------------------------------------------

ggplot() +
  annotate("from_path", x = 70, y = 22, path = "scripts/2023/week-14-soccer/soccer-ball.png", width = 0.5) +
  with_shadow(
    ggchicklet:::geom_rrect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      df_red, fill = pal[1], colour = txt, radius = grid::unit(1, "pt"))
  ) +
  with_shadow(
    ggchicklet:::geom_rrect(
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      df_yellow, fill = pal[2], colour = txt, radius = grid::unit(1, "pt"))
  ) +
  geom_from_path(aes(-36, y, path = logo), df_teams, width = 0.05) +
  geom_text(aes(-31, y, label = team), df_teams, family = ft1, size = 24, colour = txt, hjust = 0, vjust = 0) +
  geom_text(aes(-1, y+ht/2, label = yellow), df_teams, family = ft1, size = 16, colour = txt, hjust = 1) +
  geom_text(aes(-1, y-ht/2, label = red), df_teams, family = ft1, size = 16, colour = txt, hjust = 1) +
  coord_fixed(clip = "off") +
  labs(
    title = toupper("Premier League Match Data 2021-2022"),
    subtitle = sub,
    caption = caption
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 160, family = ft1),
    plot.subtitle = element_markdown(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
  )

ggsave("scripts/2023/week-14-soccer/soccer.png", height = 16, width = 16)
