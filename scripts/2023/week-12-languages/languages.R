# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(brms)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 12)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey90"
bg <- "#000080"
accent <- "grey90"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
showtext_auto()

ft <- "mukta"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- dat$languages |>
  filter(
    github_repo_stars > 0,
    github_repo_subscribers > 0
  ) |>
  mutate(
    stars = log(github_repo_stars),
    subs = log(github_repo_subscribers)
  )

mod <- brm(subs ~ stars, data = df_base)

new_data <- tibble(stars = seq(0.1, 12, 0.1))

pred <- posterior_predict(mod, newdata = new_data)

df_pred <- t(pred) |>
  as_tibble() |>
  bind_cols(new_data) |>
  pivot_longer(-stars, names_to = "id", values_to = "draw") |>
  group_by(stars) |>
  summarise(
    q5 = quantile(draw, 0.05),
    q10 = quantile(draw, 0.1),
    q25 = quantile(draw, 0.25),
    q50 = quantile(draw, 0.5),
    q75 = quantile(draw, 0.75),
    q90 = quantile(draw, 0.9),
    q95 = quantile(draw, 0.95)
  )

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}Programming languages")

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_ribbon(aes(x = stars, ymin = q5, ymax = q95), df_pred, alpha = 0.15, fill = accent) +
  geom_ribbon(aes(x = stars, ymin = q10, ymax = q90), df_pred, alpha = 0.15, fill = accent) +
  geom_ribbon(aes(x = stars, ymin = q25, ymax = q75), df_pred, alpha = 0.15, fill = accent) +
  geom_line(aes(x = stars, y = q50), df_pred, colour = "white") +
  geom_point(aes(stars, subs, size = github_repo_issues), alpha = 0.5, colour = "white") +
  labs(
    title = "PROGRAMMING LANGUAGES",
    subtitle = "Relationsip between Github stars and subscriptions. Each point is a programming language.
Size of the point represents the number of Github issues.",
    caption = caption,
    size = "Github\nissues :(",
    x = "Github Stars (log scale)",
    y = "Github Subscription (log scale)"
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.title = element_text(size = 100),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text = element_text(),
    axis.title = element_text(margin = margin(t = 5)),
    axis.title.y = element_text(angle = 90)
  )

ggsave("scripts/2023/week 12 languages/languages.png", height = 12, width = 12)
