# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(geofacet)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2022, week = 51)

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey90"
bg <- "grey10"
line <- "grey20"

pal <- c('#92001d', '#cb1a1e', '#ee5634', '#fb9e4f', '#fdda7e', '#ffffb2', '#d9f0f6', '#9cd0e3', '#629cc6', '#3660a5', '#252383')[1:9]

font_add("fa-brands", regular = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/fontawesome-free-6.2.0-web/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/fontawesome-free-6.2.0-web/webfonts/fa-solid-900.ttf")
font_add_google("Karla", "karla")
font_add_google("Merriweather Sans", "ms")
showtext_auto()

ft <- "karla"
ft_title <- "ms"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- dat$weather_forecasts |>
  filter(
    high_or_low == "high",
    !is.na(forecast_outlook),
    !is.na(forecast_hours_before)
    ) |>
  mutate(
    error = forecast_temp - observed_temp,
    hrs_before = paste("hrs", forecast_hours_before)
    )

df_state <- dat$weather_forecasts |>
  filter(
    high_or_low == "high",
    !is.na(forecast_outlook),
    !is.na(forecast_hours_before)
  ) |>
  mutate(
    error = observed_temp - forecast_temp,
    hrs_before = paste("hrs", forecast_hours_before)
  ) |>
  group_by(hrs_before, state) |>
  summarise(mean = mean(error, na.rm = TRUE))


mod <- glm(error ~ forecast_outlook + hrs_before, data = df_base)

new_data <- df_base |>
  distinct(forecast_outlook, hrs_before, high_or_low)

pred <- predict(mod, newdata = new_data, se.fit = TRUE)

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
caption <- glue("{mastodon} @danoehm@{space}fosstodon.org • {twitter} @danoehm • {github} doehm/tidytues • {floppy} Weather Forecast Capstone Project")

subtitle <-
"Weather forecasts 48hrs before are very accurate
although tend to be slightly worse for states
Alaska, Wisconsin and Vermont

Forecast outlooks that are predicting freezing
rain / drizzle or a snow event tend to be
forecast up to 2F higher than what is observed"


# 📊 plot --------------------------------------------------------------------

g_base <- pred |>
  as_tibble() |>
  bind_cols(new_data) |>
  filter(hrs_before == "hrs 48") |>
  left_join(dat$outlook_meanings, by = "forecast_outlook") |>
  mutate(
    q2.5 = fit + qnorm(0.025)*se.fit,
    q10 = fit + qnorm(0.1)*se.fit,
    q25 = fit + qnorm(0.25)*se.fit,
    q75 = fit + qnorm(0.75)*se.fit,
    q90 = fit + qnorm(0.9)*se.fit,
    q97.5 = fit + qnorm(0.975)*se.fit
  ) |>
  arrange(fit) |>
  mutate(forecast_outlook = n():1) |>
  ggplot(aes(colour = abs(fit))) +
  geom_vline(xintercept = 0, colour = line) +
  geom_segment(aes(x = q2.5, xend = q97.5, y = forecast_outlook, yend = forecast_outlook), size = 6, alpha = 0.3) +
  geom_segment(aes(x = q10, xend = q90, y = forecast_outlook, yend = forecast_outlook), size = 6, alpha = 0.4) +
  geom_segment(aes(x = q25, xend = q75, y = forecast_outlook, yend = forecast_outlook), size = 6, alpha = 0.4) +
  geom_text(aes(x = fit, y = forecast_outlook + 0.45, label = meaning), family = ft, size = 12, colour = txt) +
  annotate("text", x = -1, y = 0, label = "Forecast is lower than observed", family = ft, size = 16, colour = txt, fontface = "bold") +
  annotate("text", x = 1, y = 0, label = "Forecast is higher than observed", family = ft, size = 16, colour = txt, fontface = "bold") +
  annotate("text", x = 2.2, y = 8, label = subtitle, family = ft, size = 16, colour = txt, hjust = 0, lineheight = 0.4, vjust = 1) +
  scale_colour_gradientn(colours = rev(pal)) +
  scale_x_continuous(breaks = seq(-1, 2, 0.5), labels = seq(-1, 2, 0.5), limits = c(-1.5, 4.5)) +
  labs(
    x = "Mean error in forecast (F)",
    caption = caption,
    title = "US 48hr Weather Forecast Error"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, colour = txt, lineheight = 0.3),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(hjust = 0.5, lineheight = 0.3, size = 48),
    plot.title = element_text(family = ft_title, size = 96, colour = txt, hjust = 0.5, face = "bold"),
    plot.subtitle = element_markdown(family = ft, size = 48, colour = txt, hjust = 0.15, margin = margin(b = 20), halign = 0),
    plot.margin = margin(b = 20, t = 30, r = 50, l = 50),
    legend.position = "none",
    axis.text.x = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10)),
    axis.title.x = element_text(margin = margin(t = 10, b = 10, l = 10, r = 10), size = 48),
    axis.line.x = element_line(colour = line)
  )


g_state <- df_state |>
  filter(hrs_before == "hrs 48") |>
  ggplot(aes(fill = abs(mean))) +
  ggchicklet:::geom_rrect(aes(xmin=0, xmax = 1, ymin = 0, ymax = 1, fill = mean), radius = grid::unit(6, "pt")) +
  geom_text(aes(0.5, 0.5, label = state), family = ft, size = 12, colour = bg, fontface = "bold") +
  facet_geo(~state) +
  scale_fill_gradientn(colours = rev(pal)) +
  labs(fill = "Mean error (F)") +
  theme_void() +
  theme(
    strip.text = element_blank(),
    text = element_text(family = ft, size = 32, colour = txt, lineheight = 0.3),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(family = ft, size = 32, colour = txt, hjust = 0.5, lineheight = 0.3, margin = margin(t = 20)),
    plot.subtitle = element_markdown(family = ft, size = 48, colour = txt, hjust = 0.15, margin = margin(b = 20), halign = 0),
    plot.margin = margin(b = 20, t = 30, r = 120, l = 120),
    legend.box.margin = margin(t = 30),
    legend.position = "bottom"
  )

g_base +
  inset_element(g_state, left = 0.5, right = 1.1, bottom = 0.45, top = 0.95) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, color = bg),
      panel.background = element_rect(fill = bg, color = bg)
    )
  )

ggsave("scripts/2022/week 51 weather/weather.png", height = 12, width = 16)
