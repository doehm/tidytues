# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 7)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"

pal <- c(man = '#15616d', woman = "#ff7d00")

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
showtext_auto()

ft <- "bar"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- dat$age_gaps |>
  filter(character_1_gender != character_2_gender) |>
  count(actor_1_age, actor_2_age, character_1_gender)

lm(actor_2_age ~ actor_1_age-1, filter(dat$age_gaps, character_1_gender == "man"))

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>")
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytuesday{space2}{floppy}{space2}Data Is Plural")

title <- "Hollywood Relationships"

subtitle1 <- glue("Age difference between <span style='color:{pal[1]}'>Men</span> and <span style='color:{pal[2]}'>Women</span> actors")

subtitle <-
"In many Hollywood movies where there are couples in a romantic relationship, it is common the
male actor is older than the female actor. This age gap can range from a few years to
several decades. This trend can be observed across various movie genres, from
romantic comedies to dramas and even action movies. While there are certainly
exceptions to this pattern, it is not difficult to find examples of male
leads who are much older than their female co-stars"

df_poly <- tribble(
  ~x, ~y,
  15, 15,
  80, 80,
  80, 15
)

# 📊 plot --------------------------------------------------------------------

g_bar <- dat$age_gaps |>
  count(character_1_gender, age_difference) |>
  mutate(n = ifelse(character_1_gender == "man", n, -n)) |>
  ggplot(aes(age_difference, n, fill = character_1_gender)) +
  geom_col() +
  annotate("text", x = 30, y = 50, label = "Age difference\ndistribution\n0-50 years", family = ft, size = 14, colour = txt, vjust = 1, hjust = 0, lineheight = 0.4) +
  annotate("text", x = 30, y = -10, label = "Colour is the gender\nof the older actor", family = ft, size = 14, colour = txt, vjust = 1, hjust = 0, lineheight = 0.4) +
  annotate("text", x = 7, y = 10, label = "Male", family = ft, size = 14, colour = bg, hjust = 0.5, lineheight = 0.4, fontface = "bold") +
  annotate("text", x = 7, y = -10, label = "Female", family = ft, size = 14, colour = txt, hjust = 0.5, lineheight = 0.4, fontface = "bold") +
  scale_fill_manual(values = pal) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    legend.position = "none"
  )

g_base <- df_base |>
  ggplot() +
  geom_polygon(aes(x, y + 1), df_poly, fill = "grey90") +
  geom_point(aes(actor_1_age, actor_2_age, colour = character_1_gender, size = n), alpha = 0.75) +
  annotate("text", x = 15, y = 75, label = subtitle, family = ft, size = 14, colour = txt, vjust = 1, hjust = 0, lineheight = 0.5) +
  annotate("text", x = 15, y = 82, label = title, family = ft, size = 32, colour = txt, vjust = 1, hjust = 0, lineheight = 0.5, fontface = "bold") +
  annotate("richtext", x = 15, y = 79, label = subtitle1, family = ft, size = 14, colour = txt, vjust = 1, hjust = 0, lineheight = 0.5, label.color = NA) +
  scale_size_identity() +
  scale_colour_manual(values = pal) +
  scale_y_continuous(breaks = seq(15, 80, 5), labels = seq(15, 80, 5), position = "right") +
  scale_x_continuous(breaks = seq(15, 80, 5), labels = seq(15, 80, 5)) +
  labs(
    caption = caption,
    x = "Age of older actor",
    y = "Age of younger actor"
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = NA),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t=20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text = element_text(size = 32),
    axis.title = element_text(size = 32, margin = margin(t = 10, l = 10)),
    axis.title.y = element_text(angle = 270),
    legend.position = "none"
)

g_base +
  inset_element(g_bar, left = 0.03, top = 0.65, bottom = 0.35, right = 0.35) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(colour = NA, fill = bg)
    )
  )

ggsave("scripts/2023/week 7 hollywood/hollywood.png", height = 12, width = 12)

