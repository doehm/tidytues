# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(janitor)
library(glue)
library(ggtext)
library(colorspace)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 2)
df1 <- dat$PFW_2021_public
df2 <- dat$PFW_count_site_data_public_2021

# ✍️ fonts and palettes ------------------------------------------------------

pal <- c('#64312e', '#d95232', '#f19540', '#e3db9a', '#c2aa3e', '#545432', '#193536')
bg <- "white"
txt <- '#64312e'

font_add("fa-brands", regular = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/fontawesome-free-6.2.0-web/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "C:/Users/Dan/Documents/R/repos/survivorDev/assets/fonts/fontawesome-free-6.2.0-web/webfonts/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
font_add_google("Rowdies", "rowdies")
showtext_auto()

ft <- "mukta"
ft_title <- "rowdies"

# 🤼 wrangle -----------------------------------------------------------------

df_birds <- df1 |>
  group_by(loc_id, subnational1_code, latitude, longitude) |>
  summarise(n_birds = sum(how_many, na.rm = TRUE))

df_base <- df_birds |>
  separate(subnational1_code, into = c("country", "state"), sep = "-") |>
  filter(state == "WA")

df_map <- map("county", plot = FALSE, fill = TRUE) %>%
  st_as_sf() %>%
  filter(str_sub(ID, 1, 10) == "washington")

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
caption <- glue("{mastodon} @danoehm@{space}fosstodon.org • {twitter} @danoehm • {github} doehm/tidytuesday • {floppy} Project FeederWatch")

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_sf(data = df_map, fill = lighten(pal[7], 0.5), colour = bg) +
  geom_point(aes(longitude, latitude, size = n_birds), colour = pal[1], alpha = 0.75) +
  labs(
    title = "Project FeederWatch",
    subtitle = "Bird sightings in Washington state",
    caption = caption,
    size = "Number\nof birds"
  ) +
  theme_void() +
  theme(
    text = element_text(size = 48, colour = txt, lineheight = 0.3, family = ft),
    plot.title = element_text(family = ft_title, size = 160, hjust = 0.5, margin = margin(b = 20)),
    plot.subtitle = element_text(family = ft, hjust = 0.5, margin = margin(b = 60)),
    plot.background = element_rect(fill = bg, colour = NA),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 50, b = -100)),
    plot.margin = margin(l = 30, r = 30),
    legend.position = "bottom"
  )

ggsave("scripts/2023/week 2 birds/birds.png", height = 12, width = 12)
