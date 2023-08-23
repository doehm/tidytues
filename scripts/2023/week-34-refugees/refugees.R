# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(maps)
library(sf)
library(rgeoboundaries)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 34)
df <- dat$population

world_st <- st_read("https://michaelminn.net/tutorials/r-projections/2022-world-data.geojson") |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

pal_bar <- c('#9db4c0', '#5c6b73')
pal_bar <- colorRampPalette(c("#031926", "#77aca2", "#f4e9cd"))(6)
pal <- c("#f49cbb", "#880d1e")
txt <- "white"
bg <- "#2f3e46"
accent <- "white"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
font_add_google("Signika Negative", "sig")
ft <- "sig"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

# I'm sure there is a better way
df_base <- df |>
  filter(coa_name == "Australia") |>
  mutate_if(is.numeric, ~replace_na(.x, 0)) |>
  mutate(total = refugees + asylum_seekers + idps + oip + stateless + ooc) |>
  group_by(coo_name, coo_iso) |>
  summarise(total = sum(total)) |>
  ungroup()

df_year <- df |>
  filter(coa_name == "Australia") |>
  group_by(year) |>
  summarise(
    refugees = sum(refugees),
    asylum_seekers = sum(asylum_seekers),
    idps = sum(idps),
    oip = sum(oip),
    ooc = sum(ooc),
    stateless = sum(stateless)
  ) |>
  mutate_all(~replace_na(.x, 0)) |>
  mutate(total = refugees + asylum_seekers + idps + ooc + oip + stateless) |>
  ungroup()

world_center <- map_data("world") |>
  group_by(region) |>
  summarise(
    long = mean(long),
    lat = mean(lat)
  )

world_center <- world_center |>
  filter(region != c("Australia", "Fiji")) |>
  bind_cols(
    world_center |>
      filter(region == "Australia") |>
      select(long_aus = long, lat_aus = lat)
  ) |>
  inner_join(df_base, by = c("region" = "coo_name")) |>
  mutate(rank = max(rank(total))-rank(total)+1) |>
  filter(rank <= 10) |>
  mutate(
    lab = paste0(rank, "-", region),
    just = ifelse(region == "Bangladesh", 0, 1)
    )

world_sf <- world_st |>
  st_as_sf()

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "refugees R package")
title = "People Seeking Asylum in Australia"
subtitle <-
"Since 2010 the number of refugees, asylum seekers, international displaced people, stateless
people and others arriving in Australia has shown a notable increase. Circumstances have driven
individuals and families to seek safety and asylum in Australia.

The top 10 countries of origin are shown on the map."

# 📊 plot --------------------------------------------------------------------

g_base <- world_sf |>
  left_join(df_base, by = c("iso3" = "coo_iso")) |>
  filter(!name %in% c("Antarctica")) |>
  mutate(total = ifelse(name == "Australia", NA, total)) |>
  ggplot() +
  geom_sf(aes(fill = total), size = 0.1, colour = bg) +
  geom_curve(aes(x = long, xend = long_aus, y = lat, yend = lat_aus), world_center, colour = "grey90", curvature = -0.2, size = 0.2) +
  geom_richtext(aes(long, lat, label = lab), world_center, family = ft, size = 6, colour = bg, fill = txt, label.colour = bg, hjust = world_center$just) +
  scale_fill_gradientn(colours = pal, na.value = "white", breaks = seq(25000, 125000, 25000), labels = paste0(seq(25, 125, 25), "k")) +
  coord_sf(clip = "off") +
  labs(
    caption = caption,
    fill = " "
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 120)),
    plot.margin = margin(b = -20, t = 50, r = 50, l = 50)
  )

g_bar <- df_year |>
  pivot_longer(-c(year, total)) |>
  mutate(
    name = snakecase::to_sentence_case(name),
    name = case_when(
      name == "Idps" ~ "Internationally displaced persons",
      name == "Ooc" ~ "Other of concern",
      name == "Oip" ~ "Other people in need of protection",
      TRUE ~ name
    )
    ) |>
  ggplot() +
  geom_col(aes(year, value, fill = name), colour = bg) +
  geom_text(aes(year, total + 12000, label = paste0(round(total/1000), "k")), family = ft, size = 10, colour = txt) +
  scale_fill_manual(values = pal_bar, na.value = "white") +
  scale_x_continuous(breaks = 2010:2022) +
  labs(fill = " ") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    axis.text.x = element_text(margin = margin(t = 5, l = 20))
  )

g_title <- ggplot() +
  annotate("text", x = 0, y = 1, label = title, hjust = 0, family = ft, colour = txt, size = 24, fontface = "bold") +
  annotate("text", x = 0, y = 0.4, label = subtitle, hjust = 0, family = ft, colour = txt, size = 10, lineheight = 0.3) +
  xlim(0, 1) +
  ylim(0, 1) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg)
  )

g_base +
  inset_element(g_bar, left = 0.5, right = 1.1, bottom = -0.2, top = 0.05) +
  inset_element(g_title, left = 0, right = 0.5, bottom = -0.21, top = 0) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave("scripts/2023/week-34-refugees/refugees.png", height = 9, width = 16)
