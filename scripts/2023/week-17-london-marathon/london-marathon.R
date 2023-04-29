# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 17)

london_marathon <-dat$london_marathon |>
  clean_names()

winners <- dat$winners |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey90"
bg <- "#202e53"
accent <- "grey90"
pal <- c("#ce6a6c", "#ebada2", "#bed3c4", "#49919d", "#202e53")

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Fira Sans", "fira")
showtext_auto()

ft <- "fira"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- london_marathon |>
  mutate(
    DNF = starters-finishers,
    DNS = accepted-starters
  ) |>
  select(year, DNS, DNF, Finishers = finishers) |>
  pivot_longer(-year, names_to = "cat", values_to = "n") |>
  mutate(cat = factor(cat, levels = c("DNS", "DNF", "Finishers"))) |>
  group_by(year) |>
  mutate(p = n/sum(n)) |>
  filter(year <= 2019)

df_base |>
  group_by(cat) |>
  summarise(n = sum(n)) |>
  ungroup() |>
  mutate(p = n/sum(n))

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}London Marathon Package")

subtitle <- glue("For the runners that held a ticket, on average
<span style='color:{pal[2]}'>26% did not start (DNS)</span> the race,<br>
<span style='color:{pal[3]}'>2% did not finish (DNF)</span>
and the remaining <span style='color:{pal[4]}'>72% finished the race</span>")

y_labs <- scales::comma(seq(0, 70000, 5000))
y_labs[13] <- ""
y_labs[14] <- "Number of\nrunners"

# 📊 plot --------------------------------------------------------------------

g_hist <- df_base |>
  ggplot() +
  geom_histogram(aes(p, fill = cat)) +
  facet_wrap(~cat, scales = "free_x") +
  scale_fill_manual(values = pal[2:4]) +
  scale_x_continuous(breaks = seq(0, 1, 0.05), labels = paste0(seq(0, 100, 5), "%")) +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "none",
    panel.border = element_rect(fill = NA, colour = txt),
    axis.text.x = element_text(margin = margin(t=10)),
    strip.text = element_text(margin = margin(b=10), face = "bold")
  )

g_col <- df_base |>
  ggplot() +
  geom_col(aes(year, n, fill = cat)) +
  scale_fill_manual(values = pal[2:4]) +
  scale_y_continuous(breaks = seq(0, 70000, 5000), labels = y_labs, position = "right", limits = c(NA, 65000)) +
  labs(
    title = "London Marathon Runners",
    subtitle = subtitle,
    fill = ""
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.4, colour = txt),
    plot.title = element_text(size = 108, face = "bold"),
    plot.subtitle = element_markdown(),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text = element_text(size = 32, lineheight = 0.3),
    legend.position = "bottom",
    legend.margin = margin(t = 10)
  )

g_col / g_hist +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave("scripts/2023/week-17-london-marathon/london-marathon.png", height = 12, width = 12)

