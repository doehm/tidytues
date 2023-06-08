# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

owid_energy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-06/owid-energy.csv')

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "black"
bg <- "white"
bg <- colorspace::lighten(renewbles[3], 0.9)
accent <- renewbles[2]

renewbles <- c('#066a38', '#009247', '#79c143', '#dde2e5', '#f7fcff', '#f7fcff')
pal <- c(fossil = "black", renewable = renewbles[2])

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Playfair Display", "play")
font_add_google("Mukta", "mukta")
showtext_auto()

ft <- "play"
ft1 <- "mukta"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- owid_energy |>
  filter(
    year %in% c(2011, 2021),
    country == "Australia"
    ) |>
  transmute(
    year,
    country,
    fossil = fossil_elec_per_capita,
    renewable = renewables_elec_per_capita,
    x = 1:2
  ) |>
  pivot_longer(c(fossil, renewable), names_to = "energy_type", values_to = "val") |>
  mutate(
    x = ifelse(energy_type == "renewable", x + 1.25, x),
    x_text = ifelse(year == 2011, x-0.04, x+0.04),
    label = paste0(round(val/1000, 1), "k kWh\n", year),
    hjust = ifelse(year == 2011, 1, 0)
    )

df_text <- df_base |>
  group_by(energy_type) |>
  mutate(change = val - lag(val)) |>
  slice_max(x) |>
  ungroup() |>
  mutate(
    x = c(1.6, 2.75),
    angle = c(-38, 24),
    y = val - change/2,
    label = c("Fossils.", "Renewables.")
  )

df_text_1 <- tribble(
   ~x,  ~y,  ~label,
   1.5, 300, "Down.",
   2.75, 300, "Up."
)

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org<br>
                {twitter}{space2}@danoehm<br>
                {github}{space2}doehm/tidytues<br>
                {floppy}{space2}Energy Data Explorer")

title <- "Australia's Energy Production"
subtitle <- glue("From 2011 to 2021, on a per capita basis, energy production from fossil fuels went down
whereas production from renewables went up.")

# 📊 plot --------------------------------------------------------------------

g_base <- df_base |>
  ggplot() +
  geom_area(aes(x=x, y=val, group = energy_type, fill = energy_type)) +
  geom_text(aes(x, y = y+c(400, 600), colour = energy_type, label = label), df_text,
            family = ft, size = 46, angle = df_text$angle, fontface = "bold") +
  geom_text(aes(x, y = y, label = label), df_text_1, vjust = 0,
            family = ft, size = 46,  fontface = "bold", colour = "white") +
  geom_text(aes(x = x_text, y = val, label = label, colour = energy_type), family = ft,
            size = 12, hjust = df_base$hjust, lineheight = 0.3) +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  coord_cartesian(clip = "off") +
  xlim(0.75, 3.5) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, colour = txt, lineheight = 0.3),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(100, 100, 100, 350),
    legend.position = "none"
  )

g_text <- ggplot() +
  xlim(0, 1) +
  ylim(0, 1) +
  annotate("text", x = 0.5, y = 0.75, label = str_wrap(title, 15), fontface = "bold",
           family = ft, size = 48, hjust = 0.5, lineheight = 0.3) +
  annotate("text", x = 0.5, y = 0.45, label = str_wrap(subtitle, 30),
           family = ft, size = 20, hjust = 0.5, lineheight = 0.4) +
  annotate("richtext", x = 0.1, y = 0.075, label = caption, hjust = 0, fill = NA,
           family = ft1, size = 12, lineheight = 0.5, label.colour = NA) +
  coord_cartesian(clip = "off") +
  theme_void()

g_base +
  inset_element(g_text, left = -0.4, bottom = 0, top = 1, right = 0) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave("scripts/2023/week-23-energy/energy.png", height = 12, width = 16)

