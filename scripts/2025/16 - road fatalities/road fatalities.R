# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(fable)
library(fabletools)
library(feasts)
library(tsibble)

# 💾 load data ---------------------------------------------------------------

daily_accidents <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-22/daily_accidents.csv')
daily_accidents_420 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-22/daily_accidents_420.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "grey20"
bg <- "white"
accent <- txt

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")

font_add_google("Raleway", "raleway", regular.wt = 200)
ft <- "raleway"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df <- daily_accidents |>
  mutate(date = ymd(glue("{year(date)}-{month(date)}-01"))) |>
  group_by(date) |>
  summarise(fatalities_count = sum(fatalities_count)) |>
  mutate(
    year = year(date),
    month = month(date),
    ym = yearmonth(paste(year(date), month(date))),
    series = "Actual"
  )

df_ts <- df |>
  as_tsibble(index = ym)

mod <- df_ts |>
  filter(year <= 2006) |>
  model(
    TSLM(fatalities_count ~ trend() + season())
  )

fcst <- forecast(mod, h = 120)

df_base <- df |>
  bind_rows(
    fcst |>
      as_tibble() |>
      transmute(
        ym,
        fatalities_count = .mean,
        series = "Counterfactual"
      )
  )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, bg)
text <- str_wrap("Road safety initiates saved 141k lives", 30)

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_vline(aes(xintercept = ymd("2007-01-01")), linetype = 3, linewidth = 1) +
  geom_line(aes(ym, fatalities_count, colour = series, linetype = series)) +
  annotate("text", x = ymd("2007-06-01"), y = 6000, label = text, family = ft, colour = txt, size = 16, hjust = 0, lineheight = 0.3) +
  scale_colour_manual(values = c(Actual = "grey20", Counterfactual = sunset[4])) +
  scale_y_continuous(breaks = seq(0, 8000, 500), labels = seq(0, 8000, 500)) +
  labs(
    title = "Fatal Car Crashes in the US",
    subtitle = str_wrap("In 2007, the United States saw a decrease in road fatalities,
    partly due to the early stages of the Great Recession and a corresponding
    decline in travel. Additionally, ongoing safety initiatives, such as seatbelt
    laws and Graduated Driver Licensing programs, were likely contributing factors.
    These initiatives saved approximately 141k lives from 2007 to 2016. The counterfactual
    estimates the fatalities assuming the initiatives weren't implemented", 130),
    caption = caption,
    colour = "Series",
    linetype = "Series"
  ) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0, face = "bold", margin = margin(b = 30)),
    plot.subtitle = element_text(hjust = 0, margin = margin(b = 30)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 50, t = 80, r = 80, l = 80),
    axis.text = element_text(margin = margin(t = 10)),
    axis.line = element_line(),
    axis.ticks = element_line(),
    axis.ticks.length = unit(0.25, "cm"),
    panel.grid.major = element_line(colour = "grey95"),
    legend.position = "top"
  )

ggsave("scripts/2025/16 - road fatalities/road fatalities.png", height = 12, width = 16)
