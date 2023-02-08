# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

big_tech_stock_prices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv')

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "white"
bg <- "black"

pal <- c(
  TSLA = "#e82127",
  MSFT = "#00a1f1",
  AAPL = "#555555",
  ADBE = "#ED2224",
  AMZN = "#FF9900",
  CSCO = "#15495d",
  GOOGL = "#3cba54",
  IBM = "#006699",
  INTC = "#0071c5",
  META = "#0668E1",
  NFLX = "#E50914",
  NVDA = "#76b900",
  ORCL = "#f80000"
)

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
showtext_auto()

ft <- "bar"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- map_dfr(unique(big_tech_stock_prices$stock_symbol), ~{
  big_tech_stock_prices |>
    mutate(
      group = stock_symbol,
      stock_symbol = .x
      )
}) |>
  left_join(big_tech_companies, by = "stock_symbol")

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
caption <- glue("{mastodon} @danoehm@{space}fosstodon.org • {twitter} @danoehm • {github} doehm/tidytuesday • {floppy} Big Tech Stock Prices on Kaggle")

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_line(aes(date, close, group = group), alpha = 0.5, size = 0.1, colour = "grey") +
  geom_area(aes(date, close, colour = stock_symbol, fill = stock_symbol), big_tech_stock_prices, alpha = 0.2, size = 0.5) +
  geom_text(aes(lubridate::ymd("2011-01-01"), 500, label = str_wrap(company, 20), colour = stock_symbol), big_tech_companies,
            family = ft, size = 16, hjust = 0, vjust = 1, lineheight = 0.3, fontface = "bold") +
  facet_wrap(~stock_symbol, ncol = 3) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  labs(
    title = "Big Tech Stock Prices",
    caption = caption
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = "white"),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 84, hjust = 0.5, margin = margin(b = 20), face = "bold"),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t=20), size = 32),
    plot.margin = margin(b = 20, t = 20, r = 50, l = 50),
    axis.text = element_text(size = 24),
    strip.text = element_blank(),
    legend.position = "none"
  )

ggsave("scripts/2023/week 6 tech/tech.png", height = 12, width = 12)
