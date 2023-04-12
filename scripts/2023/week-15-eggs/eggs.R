# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(forecast)
library(fable)
library(fabletools)
library(tsibble)
library(feasts)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 15)

egg_prod <- dat$`egg-production`

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
pal <- c('#efeef3', '#bfac9e', '#be8b60', '#8a5430', '#5a2e23')
pal <- colorRampPalette(pal)(18)
accent <- tail(pal, 1)
bg <- pal[2]

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
showtext_auto()

ft <- "bar"

# 🤼 wrangle -----------------------------------------------------------------

# make base table
df_base <- egg_prod |>
  filter(
    prod_type == "table eggs",
    prod_process == "all"
  ) |>
  mutate(
    n_eggs = n_eggs/1e6,
    date = floor_date(observed_month, "month"),
    month = 1:n(),
    n_egg_p_hen = n_eggs/n_hens,
    grp = 1:n()
  )

# binding like this to make the ribbons
df_base0 <- df_base |>
  bind_rows(
    df_base |>
      mutate(grp = 1:n()+1)
  ) |>
  arrange(month) |>
  group_by(grp) |>
  mutate(fill = tail(n_eggs, 1))

# set as tsibble for forecasting
df_ts <- df_base |>
  as_tsibble(index = month)

# forecasting with fable
df_fcst <- df_ts |>
  filter(month <= 42) |>
  model(mod_ets = ETS(n_eggs)) |>
  forecast(h = 14) |>
  left_join(
    df_base |>
      select(date, month),
    by = "month"
  )

# estimating the trend from {feasts}
df_trend <- df_ts |>
  model(stl = STL(n_eggs)) |>
  components() |>
  left_join(
    df_ts |>
      mutate(date = floor_date(observed_month, "month")) |>
      select(date, month),
    by = "month"
  ) |>
  left_join(
    df_fcst |>
      as_tibble() |>
      select(.mean, month),
    by = "month"
  )

# where the eggs go
df_egg <- df_trend |>
  filter(month(date) == 1) |>
  mutate(egg_lab = paste0(round(n_eggs/1000, 1), "B"))

# adding the trend estimates onto df_base0
df_base0 <- df_base0 |>
  left_join(
    df_trend |>
      select(date, trend),
    by = "date"
  ) |>
  group_by(grp) |>
  mutate(fill = max(trend))

# estimating the total loss of production
total_loss <- df_trend |>
  filter(!is.na(.mean)) |>
  as_tibble() |>
  summarise(total = round(sum(.mean-trend)/1000, 1))

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}<br>
{twitter}{space2}@danoehm{space2}<br>
{github}{space2}doehm/tidytues{space2}<br>
{floppy}{space2}US Egg Production Data 2007-2021")

egg <- glue("<span style='font-family:fa-solid; color:{pal[2]}'>&#xf7fb;</span>")
egg1 <- glue("<span style='font-family:fa-solid; color:{pal[8]}'>&#xf7fb;</span>")

title <- "US Egg Production"
subtitle <- "The COVID-19 pandemic caused a decrease in table egg production estimated at 4.7 Billion eggs for 2020"

peak <- "US Egg production hit it's peak in Dec 2019 just prior to the COVID-19 pandemic at 8.6 Billion eggs"
covid <- glue("The outbreak of COVID-19 led to widespread disruptions in the supply chain, including
shortages of feed, labor shortages due to illness or quarantine, and logistical challenges
related to transportation and processing. These disruptions resulted in reduced egg
production in some regions, as well as higher costs for producers due to increased
expenses for protective equipment and biosecurity measures.

Additionally, the closure of many restaurants and other foodservice establishments
during the pandemic led to a decrease in demand for eggs from these customers. This
shift in demand from foodservice to retail channels further disrupted the supply chain,
as producers and distributors had to adapt to new patterns of demand and distribution.

The total loss of production is estimated to be {total_loss} Billion table eggs for 2020.")

trough <- "In June 2020 US egg production reached it's lowest point and lowest June production since 2017 at 7.6 Billion eggs."

df_annotations <- tribble(
  ~date, ~y, ~yend, ~lab,
  "2019-12-01", 8601, 8520, peak,
  "2020-07-01", 8280, 7700, covid,
  "2020-06-01", 7598, 7490, trough
) |>
  mutate(
    xend = ymd("2021-03-01"),
    date = ymd(date)
  )

# 📊 plot --------------------------------------------------------------------

df_base0 |>
  ggplot() +
  geom_line(aes(date, n_eggs), df_base) +
  geom_ribbon(aes(date, ymin = 7000, ymax = n_eggs, group = grp, fill = fill)) +
  geom_line(aes(date, trend), df_trend, size = 3, colour = pal[8]) +
  geom_line(aes(date, trend), df_trend, size = 2, colour = pal[4]) +

  # forecast
  geom_line(aes(date, .mean), df_fcst, size = 3, colour = pal[4], linetype = 2) +
  geom_ribbon(aes(date, ymin = trend, ymax = .mean), filter(df_trend, !is.na(.mean)), fill = pal[8], alpha = 0.5) +

  # eggs
  geom_richtext(aes(date, trend, label = egg1), df_egg, label.color = NA, fill = NA, size = 68) +
  geom_richtext(aes(date, trend, label = egg), df_egg, label.color = NA, fill = NA, size = 64) +
  geom_point(aes(date, trend+10), df_egg, size = 16, colour = pal[2]) +
  geom_text(aes(date, trend, label = egg_lab), df_egg, family = ft, size = 20, colour = txt, fontface = "bold") +

  # annotations
  geom_segment(aes(x = date, xend = xend, y = y, yend = y), df_annotations, colour = txt) +
  geom_segment(aes(x = xend, xend = xend, y = y, yend = yend), df_annotations, colour = txt) +
  geom_point(aes(x = date, y = y), df_annotations, colour = txt, size = 3) +
  geom_text(aes(x = xend+months(1), y = y, label = str_wrap(lab, 55)), df_annotations, family = ft, size = 14, colour = txt, lineheight = 0.35, vjust = 1, hjust = 0) +

  # titles
  annotate("text", x = ymd("2016-07-01"), y = 8600, label = title, family = ft, size = 48, fontface = "bold", colour = txt, hjust = 0) +
  annotate("text", x = ymd("2016-07-01"), y = 8500, label = str_wrap(subtitle, 60), family = ft, size = 18, colour = txt, hjust = 0, lineheight = 0.35) +
  annotate("richtext", x = ymd("2021-04-01"), y = 7100, label = caption, family = ft, size = 14, colour = txt, hjust = 0, lineheight = 0.5, label.color = NA, fill = NA) +

  scale_x_date(breaks = seq.Date(ymd("2016-01-01"), ymd("2021-01-01"), "year"), limits = c(ymd("2016-07-01"), ymd("2022-09-01")), date_labels = "%Y") +
  scale_y_continuous(breaks = seq(7000, 8500, 500), label = c("7.0B", "7.5B", "8.0B", "8.5B")) +
  scale_fill_gradientn(colours = rev(pal)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text = element_text(),
    legend.position = "none"
  )

ggsave("scripts/2023/week-15-eggs/eggs.png", height = 12, width = 20)
