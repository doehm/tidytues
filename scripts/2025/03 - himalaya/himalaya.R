# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(magick)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

exped_tidy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/exped_tidy.csv') |>
  clean_names()
peaks_tidy <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-01-21/peaks_tidy.csv') |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "grey20"
bg <- "white"
sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
font_add_google("Poppins", "pop", regular.wt = 200)
font_add_google("Signika", "sig", regular.wt = 300, bold.wt = 600)
ft <- "pop"
ft1 <- "pop"
showtext_auto()


# 📷 image prep --------------------------------------------------------------

hikers <- list.files("images/hikers/original", full.names = TRUE)

walk(hikers, ~{
  image_read(.x) |>
    image_fill("none", fuzz = 10) |>
    image_write(.x)
})

walk(hikers, ~{
  image_read(.x) |>
    image_fill("none", fuzz = 10) |>
    image_fill("#9a8c98", fuzz = 10, point = "+65+50") |>
    image_write(str_replace(.x, "original", "alive"))
})

walk(hikers, ~{
  image_read(.x) |>
    image_fill("none", fuzz = 10) |>
    image_fill("grey20", fuzz = 10, point = "+65+50") |>
    image_write(str_replace(.x, "original", "summit"))
})

walk(hikers, ~{
  image_read(.x) |>
    image_fill("none", fuzz = 10) |>
    image_fill(sunset[4], fuzz = 10, point = "+65+50") |>
    image_write(str_replace(.x, "original", "death"))
})

# 🚙 functions ---------------------------------------------------------------

build_df <- function(k) {
  tibble(x = 1:df$totmembers[k]) |>
    mutate(
      peakid = df$peakid[k],
      cat = case_when(
        x <= df$mdeaths[k] ~ "death",
        x <= df$totmembers[k] - df$smtmembers[k] + df$mdeaths[k] ~ "alive",
        TRUE ~ "summit",
      ),
      x = (x-1) %% 3,
      y = cumsum(ifelse(x == 0, 1, 0))/61,
      i = sample(1:length(hikers), n(), replace = TRUE),
      img = glue("images/hikers/{cat}/hiker0{i}.png"),
      totmembers = df$totmembers[k]
    )
}

# 🤼 wrangle -----------------------------------------------------------------

df_base <- exped_tidy |>
  mutate(year = year(smtdate)) |>
  filter(year > 2020) |>
  group_by(peakid) |>
  summarise(
    totmembers = sum(totmembers),
    smtmembers = sum(smtmembers),
    mdeaths = sum(mdeaths)
  ) |>
  mutate(
    p_deaths = mdeaths/totmembers,
    p_summit = smtmembers/totmembers,
    peakid = fct_reorder(peakid, totmembers, .desc = TRUE)
  ) |>
  arrange(desc(totmembers)) |>
  head(10)

df <- df_base |>
  mutate_at(vars(-peakid), ~round(.x/10))

df_grid <- map_dfr(1:nrow(df), build_df) |>
  mutate(peakid = fct_reorder(peakid, totmembers, .desc = TRUE))

df_labels <- df_grid |>
  group_by(peakid) |>
  summarise(y = max(y)) |>
  left_join(df_base, join_by(peakid)) |>
  left_join(
    peaks_tidy |>
      select(peakid, pkname, heightm),
    join_by(peakid)
  ) |>
  mutate(
    peakid = fct_reorder(peakid, totmembers, .desc = TRUE),
    totmembers = scales::label_comma()(totmembers),
    heightm = scales::label_comma()(heightm),
    pct_summit = scales::label_percent(accuracy = 0.1)(p_summit),
    pct_deaths = scales::label_percent(accuracy = 0.1)(p_deaths),
    text = glue("{totmembers} climbers started, {smtmembers} ({pct_summit}) made the summit, {mdeaths} ({pct_deaths}) died")
  )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(txt, bg)
title <- "Himalayan\nExpeditions"

# 📊 plot --------------------------------------------------------------------

df_grid |>
  ggplot() +
  geom_from_path(aes(x, y, path = img), width = 0.02) +
  geom_text(aes(1.2, y = y+0.04, label = pkname), df_labels, family = ft, size = 56, fontface = "bold", colour = txt, hjust = 0) +
  geom_text(aes(0, y = y+0.04, label = text), df_labels, family = ft1, size = 18, colour = txt, hjust = 0, vjust = 1.5) +
  geom_text(aes(x = 0.5, y = 1.1, label = title), filter(df_labels, peakid == "DHA1"), hjust = 0.5, size = 96, lineheight = 0.3, family = ft, colour = txt, fontface = "bold") +
  facet_wrap(~peakid, ncol = 1) +
  ylim(0, 1.5) +
  labs(caption = caption) +
  coord_flip(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 50)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    panel.spacing = unit(2, "cm"),
    strip.text = element_blank()
  )

ggsave("scripts/2025/03 - himalaya/himalaya.png", height = 16, width = 22)

