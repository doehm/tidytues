# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2022, week = 49)
elevators <- dat$elevators |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- '#f7f7f7'
num_col <- "green"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
font_add_google("Orbitron", "orb")
showtext_auto()

ft <- "mukta"
ft1 <- "orb"

# 🤼 wrangle -----------------------------------------------------------------

w <- 6
h <- 10
a <- 0.12
b <- 0.8
d <- 0.025
e <- 0.4
f <- 0.2
g <- 0.5
i <- 0.8
df_elevotor <- tribble(
  ~x0, ~x1, ~y0, ~y1, ~layer, ~col,
  0, w, 0, h, "outer", x[1],
  w*a, w*(1-a), 0, h*b, "inner", x[2],
  w/2, w/2, 0, h*0.8, "middle", x[2],
  w*(1-a+d), w*(1-d), h*e, h*(e+f), "controls", x[3],
  w/2-0.5, w/2+0.5, h+g, h+g+i, "top", "black",
  w/2-w/3, w/2+w/3, h-g, h-g-i, "inner score", "black",
)

df_base <- elevators |>
  mutate(
    floors = str_extract(dv_floor_to, "\\d+"),
    floors = as.numeric(floors)
    ) |>
  filter(device_type == "Passenger Elevator (P)") |>
  group_by(borough) |>
  summarise(
    speed = paste0(round(mean(as.numeric(dv_speed_fpm), na.rm = TRUE), 0), " fpm"),
    floors = round(mean(floors, na.rm = TRUE)),
    weight = paste0(round(mean(dv_capacity_lbs, na.rm = TRUE)/1000, 1), "k lbs")
  ) |>
  arrange(floors) |>
  mutate(
    x = c(-1, 1, -1, 1, -1, 1),
    y = floors*0.5,
    hjust = c(c(1, 0, 1, 0, 1, 0)),
    x_floors = w/2,
    y_floors = h+g+i/2,
    x_weight = w/2,
    y_weight = h-g-i/2,
    lab = paste(weight, "/", speed),
    x_title = w/2,
    y_title = h+g+2*i,
    x_button = (2*w-w*a)/2,
    y_button1 = h/2+0.35,
    y_button2 = h/2-0.35
    ) |>
  drop_na()

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
caption <- glue("{mastodon} @danoehm@{space}fosstodon.org • {twitter} @danoehm • {github} doehm/tidytuesday • {floppy} elevators package")

title <- "The\nElevators\nof NYC"
subtitle <-
"Numbers represent the mean
 • number of floors,
 • weight capacity and
 • speed (floors per minute)
for passenger elevators in NYC
Brooklyn services the most floors
on average. Manhattan has the
fastest."

df_elevotor <- map_dfr(df_base$borough, ~{
  df_elevotor |>
    mutate(borough = .x)
}) |>
  bind_rows(tibble(
    title = title,
    subtitle = subtitle,
    borough = "a"
  ))

# 📊 plot --------------------------------------------------------------------

df_elevotor |>
  ggplot() +
  geom_rect(aes(xmin = x0, xmax = x1, ymin = y0, ymax = y1), fill = df_elevotor$col, colour = "black", size = 2) +
  geom_text(aes(x_floors, y_floors, label = floors), df_base, family = ft1, size = 16, colour = num_col) +
  geom_text(aes(x_weight, y_weight, label = lab), df_base, family = ft1, size = 16, colour = num_col) +
  geom_text(aes(x_title, y_title, label = borough), df_base, family = ft, size = 24, colour = txt) +
  geom_text(aes(w/2, h+2.2, label = title), df_elevotor, family = ft, size = 48, colour = txt, lineheight = 0.3, vjust = 1) +
  geom_text(aes(w*a, h-4, label = subtitle), df_elevotor, family = ft, size = 16, colour = txt, lineheight = 0.4, vjust = 1, hjust = 0) +
  geom_point(aes(x_button, y_button1), df_base, size = 3) +
  geom_point(aes(x_button, y_button2), df_base, size = 3) +
  facet_wrap(~borough) +
  coord_cartesian(clip = "off") +
  labs(
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, colour = txt),
    plot.background = element_rect(fill = bg),
    plot.caption = element_markdown(family = ft, size = 48, colour = txt, hjust = 0.6, margin = margin(t = 40)),
    panel.spacing=unit(4,"lines"),
    plot.margin = margin(50, 50, 10, 50),
    strip.text = element_blank()
  )

ggsave("scripts/2022/week 49 elevators/elevators.png", height = 12, width = 14)

