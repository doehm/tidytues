# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 22)

cent <- dat$centenarians |>
  rename(country = place_of_death_or_residence) |>
  mutate(
    country_lab = tolower(country),
    country_lab = case_when(
      country == "United States" ~ "united-states-of-america",
      TRUE ~ country_lab
    )
  )

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "#D8D2CC"
accent <- txt
pal <- c(female = "#CD5555", male = "#2F4F4F")

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
showtext_auto()

ft <- "bar"

# 🤼 wrangle -----------------------------------------------------------------



# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}frankiethull: Centenarians")

# 📊 plot --------------------------------------------------------------------

g1 <- cent |>
  ggplot() +
  geom_boxplot(aes(gender, age, fill = gender, colour = gender), alpha = 0.4, outlier.colour = NA, width = 0.25) +
  geom_jitter(aes(gender, age, colour = gender), size = 4, alpha = 0.6, width = 0.25) +
  scale_y_continuous(breaks = seq(110, 126, 1), labels = seq(110, 126, 1)) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    subtitle = "On average the oldest women are 3 years older
than the oldest men",
    y = "Age",
    fill = "Gender",
    colour = "Gender"
    ) +
  theme_minimal() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.subtitle = element_text(),
    plot.margin = margin(b = 20, t = 0, r = 50, l = 50),
    axis.text.y = element_text(),
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    legend.position = "bottom"
  )

g2 <- cent |>
  ggplot() +
  geom_point(aes(birth_date, age, colour = gender), size = 4, alpha = 0.6) +
  geom_smooth(aes(birth_date, age, colour = gender, fill = gender)) +
  scale_y_continuous(breaks = seq(110, 126, 1), labels = seq(110, 126, 1)) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    y = "Age",
    x = "Date of birth",
    subtitle = "No trend that the oldest are getting older"
    ) +
  theme_minimal() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(b = 20, t = 0, r = 50, l = 50),
    axis.text.x = element_text(),
    legend.position = "none"
  )

g_alive <- cent |>
  filter(still_alive == "alive") |>
  mutate(
    dob = format(birth_date, "%d %B %Y"),
    flag = glue("C:/Users/Dan/Documents/Assets/flag-png-round-medium/{country_lab}_flag-png-round-medium.png"),
    y = n():1,
    x = 0.15
    ) |>
  ggplot() +
  geom_from_path(aes(x, y, path = flag), width = 0.12) +
  geom_text(aes(x+0.15, y = y+0.3, label = name, colour = gender), family = ft, size = 24, fontface = "bold", hjust = 0) +
  geom_text(aes(x+0.15, y = y-0.16, label = round(age)), family = ft, size = 24, fontface = "bold", colour = txt, hjust = 0) +
  geom_text(aes(x+0.3, y = y-0.2, label = dob), family = ft, size = 12, colour = txt, hjust = 0) +
  scale_colour_manual(values = pal) +
  coord_cartesian(clip = "off") +
  labs(
    subtitle = "Oldest people in the world",
    ) +
  xlim(0, 1) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.subtitle = element_text(size = 48, hjust = 0.5),
    plot.margin = margin(b = 20, t = 0, r = 50, l = 50),
    legend.position = "none"
  )

g_alive +
  (g1 / g2) +
  plot_annotation(
    title = "Verified Oldest People",
    subtitle = str_wrap("These are lists of the 100 known verified oldest people sorted in descending
order by age in years and days. The oldest person ever whose age has been independently
verified is Jeanne Calment (1875–1997) of France, who lived to the age of 122 years and
164 days", 150),
  caption = caption,
  theme = theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 36, hjust = 0, margin = margin(b = 20)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(t = 50, b = 20),
    legend.position = "none"
  )
  )

ggsave("scripts/2023/week-22-centenarians/centenarians.png", height = 12, width = 12)

