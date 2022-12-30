# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2022, week = 52)
attach(dat)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "white"
txt2 <- pal[1]
bg <- "grey10"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add("trek", regular = "assets/fonts/star trek/TNG_Title.ttf")
font_add("jeff", regular = "assets/fonts/star trek/jeffries.ttf")
font_add_google("Mukta", "mukta")
showtext_auto()

ft <- "mukta"
ft2 <- "jeff"

pal <- c('#d7ad19', '#be293d', '#019dcd')

# 🤼 wrangle -----------------------------------------------------------------

df_base <- tlBooks |>
  count(year) |>
  filter(
    year > 0,
    year < 3000
  ) |>
  mutate(alpha = n/max(n))

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
spock <- glue("<span style='font-family:fa-solid; color:{pal[2]}'>&#xf259;</span>")
caption <- glue("{mastodon} @danoehm@{space}fosstodon.org • {twitter} @danoehm • {github} doehm/tidytues • {floppy} rtrek package")

subtitle <- glue(
"Almost 60% of Star Trek narratives<br>
across all formats occur between the<br>
years 2351 and 2380 {spock}")

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_segment(aes(x = year, xend = year, y = 0, yend = n), colour = txt) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = 159, alpha = alpha), colour = txt) +
  annotate("text", x=0, y=159, label = "STAR TREK", colour = pal[3], family = "trek", size = 164, hjust = 0, vjust = 1) +
  annotate("richtext", x=0, y=130, label = subtitle, colour = pal[1], family = ft2, size = 16, hjust = 0, vjust = 1,
           lineheight = 0.6, label.color = NA, fill = NA) +
  scale_x_continuous(breaks = seq(0, 3000, 1000), labels = seq(0, 3000, 1000)) +
  scale_alpha_identity() +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t=20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text.x = element_text()
  )

ggsave("scripts/2022/week 52 star trek/star trek.png", height = 12, width = 16)
