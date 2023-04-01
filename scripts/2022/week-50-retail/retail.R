# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(lubridate)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2022, week = 50)

# ✍️ fonts and palettes ------------------------------------------------------

pal <- colorRampPalette(c('#173f5f', '#20639b', '#3caea3', '#f6d55c', '#ed553b'))(11)

txt <- "grey20"
bg <- "white"
line = "grey80"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Kanit", "mukta")
showtext_auto()

ft <- "mukta"

# 🤼 wrangle -----------------------------------------------------------------

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
caption <- glue("{mastodon} @danoehm@{space}fosstodon.org<br>{twitter} @danoehm<br>{github} doehm/tidytues<br>{floppy} US Census Bureau Monthly State Retails Sales")

# 📊 plot --------------------------------------------------------------------

dat$state_retail |>
  filter(
    state_abbr == "USA",
    subsector != "total"
  ) |>
  mutate(
    ym = ymd(paste0(year, "-", month, "-01")),
    change_yoy = as.numeric(change_yoy),
    change_yoy_se = as.numeric(change_yoy_se)
  ) |>
  group_by(subsector) |>
  mutate(max = max(change_yoy)) |>
  ggplot(aes(ym, change_yoy, colour = max, fill = max)) +
  geom_area() +
  geom_text(aes(ymd("2020-01-01"), 600, label = str_wrap(subsector, 12)), family = ft, size = 24, lineheight = 0.3, vjust = 1, colour = txt) +
  facet_wrap(~subsector, ncol = 2)+
  scale_colour_gradientn(colours = pal) +
  scale_fill_gradientn(colours = pal) +
  labs(
  caption = caption,
  title = "US Monthly Retail Sales",
  subtitle = "Percentage change year on year<br>
  There was a 7 fold increase in clothing sales in 2021",
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, colour = txt, lineheight = 0.3),
    plot.background = element_rect(fill = bg),
    plot.caption = element_markdown(family = ft, size = 48, colour = txt, hjust = 0, lineheight = 0.3),
    plot.title = element_text(family = ft, size = 128, colour = txt, hjust = 0.2, face = "bold"),
    plot.subtitle = element_markdown(family = ft, size = 48, colour = txt, hjust = 0.15, margin = margin(b = 20), halign = 0),
    plot.margin = margin(b = 20, t = 30, r = 50, l = 50),
    strip.text = element_blank(),
    legend.position = "none",
    axis.text = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    axis.ticks = element_line(colour = line),
    axis.line = element_line(colour = line),
    panel.grid = element_line(colour = line, linetype = 3)
  )

ggsave("scripts/2022/week 50 retail/retail.png", height = 16, width = 10)
