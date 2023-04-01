# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(lubridate)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 10)
numbats <- dat$numbats |>
  clean_names() |>
  mutate(
    wday = factor(wday, levels = rev(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))),
    month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    )

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- "#84a98c"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
showtext_auto()

ft <- "mukta"

# 🤼 wrangle -----------------------------------------------------------------

df_day_hour <- numbats |>
  count(wday, hour) |>
  drop_na()

df_year <- numbats |>
  mutate(year = year(event_date)) |>
  count(year)

df_month <- numbats |>
  count(month) |>
  drop_na()

df_seg <- tribble(
  ~x, ~xend, ~y, ~yend,
  0.3, 0.8, 3.25, 3.25,
  0.04, 0.28, 3.25, 3.25,
  0.04, 0.04, 3.25, 3.3,
  0.28, 0.28, 3.25, 3.3,
  0.3, 0.3, 3.25, 3.3,
  0.8, 0.8, 3.25, 3.3,
)

df_length <- tribble(
  ~x, ~y, ~label,
  0.16, 3.25, "15-18cm",
  0.55, 3.25, "20-25cm"
)

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}Atlas of Living Australia")

subtitle <- "The numbat (Myrmecobius fasciatus), also known as the noombat or walpurti, is an insectivorous marsupial. It is
diurnal and its diet consists almost exclusively of termites.

The species was once widespread across southern Australia, but is now restricted to several small colonies in Western
Australia. It is therefore considered an endangered species and protected by conservation programs. Numbats were
recently re-introduced to fenced reserves in South Australia and New South Wales. The numbat is the faunal emblem
of Western Australia."

subtitle1 <- "> Almost all numbats sighted between 1-2pm. Assuming a lunchtime numbat sighting group
> Most numbats were sighted in 2014
> Most numbats were sighted in Summer"

# 📊 plot --------------------------------------------------------------------

g_base <- ggplot() +
  geom_from_path(aes(0.5, 4.25, path = "scripts/2023/week 10 numbats/numbat-pic-no-bg.png")) +
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), df_seg) +
  geom_richtext(aes(x, y, label = label), df_length, family = ft, size = 12, colour = txt) +
  ylim(0, 5) +
  xlim(0, 1) +
  labs(
    title = "NUMBATS",
    subtitle = subtitle,
    caption = caption
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 160, face = "bold"),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
  )

g_day_hour <- df_day_hour |>
  ggplot() +
  geom_tile(aes(hour, wday, fill = n), width = 0.75, height = 0.8, colour = txt) +
  scale_x_continuous(breaks = 0:23, labels = 0:23) +
  scale_fill_gradient(low = "white", high = accent) +
  labs(
    title = "Numbat sightings",
    subtitle = subtitle1,
    x = "Hour of the day",
    fill = "Numbat\nsightings"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 64, face = "bold", margin = margin(b = 5)),
    plot.subtitle = element_text(margin = margin(b = 30)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 0, t = 0, r = 50, l = 50),
    axis.text = element_text(),
    axis.text.y = element_text(hjust = 1),
    axis.title.x = element_text(margin = margin(t = 10))
  )

g_year <- df_year |>
  filter(year > 2000) |>
  ggplot() +
  geom_area(aes(year, n), fill = accent, alpha = 0.2) +
  geom_line(aes(year, n), colour = accent, size = 1) +
  geom_point(aes(year, n), colour = accent, size = 3) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text = element_text(),
    axis.text.y = element_text(hjust = 1)
  )

g_month <- df_month |>
  ggplot() +
  geom_col(aes(month, n), fill = accent) +
  geom_text(aes(month, n+15, label = n), family = ft, colour = txt, size = 12) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text.x = element_text(angle = 45)
  )

g_base +
  inset_element(g_year, top = 0.3, bottom = 0, left = 0, right = 0.5) +
  inset_element(g_month, top = 0.3, bottom = 0, left = 0.5, right = 1) +
  inset_element(g_day_hour, top = 0.58, bottom = 0.27, left = 0, right = 1)
ggsave("scripts/2023/week 10 numbats/numbats.png", height = 16, width = 12)
