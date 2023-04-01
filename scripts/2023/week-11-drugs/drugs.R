# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggsvg)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 11)
drugs <- dat$drugs

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "black"
bg <- "grey90"
accent <- txt

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Roboto Slab", "sono")
showtext_auto()

ft <- "sono"

# 🤼 wrangle -----------------------------------------------------------------

n_active_sub <- drugs |>
  count(common_name) |>
  arrange(desc(n)) |>
  slice_head(n = 100)

n_area <- drugs |>
  count(therapeutic_area) |>
  arrange(desc(n)) |>
  drop_na() |>
  slice_head(n = 100)

full_grid <- expand_grid(
  common_name = n_active_sub$common_name,
  therapeutic_area = n_area$therapeutic_area
)

df_count <-drugs |>
  semi_join(n_active_sub, by = "common_name") |>
  semi_join(n_area, by = "therapeutic_area") |>
  count(common_name, therapeutic_area)

df_base <- full_grid |>
  left_join(df_count, by = c("common_name", "therapeutic_area")) |>
  mutate(n = replace_na(n, 0)) |>
  arrange(desc(n), common_name, therapeutic_area) |>
  mutate(
    x = as.numeric(factor(common_name)),
    y = as.numeric(factor(therapeutic_area))
  ) |>
  filter(n > 0)

# for the final plot
df_depression <- drugs |>
  filter(str_detect(tolower(therapeutic_area), "depress|anxiet")) |>
  count(active_substance) |>
  mutate(
    active_substance = str_to_title(active_substance),
    active_substance = factor(active_substance),
    active_substance = fct_reorder(active_substance, n, max)
    ) |>
  arrange(active_substance)

df_depression <- map_dfr(df_depression$active_substance, ~{
  tibble(
    active_substance = .x,
    x = 1:df_depression$n[df_depression$active_substance == .x]
  )
})

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}European Medicines Agency")

cap_url <- "https://www.svgrepo.com/download/409421/capsule.svg"
svg_txt <- paste(readLines(cap_url), collapse = "\n")

subtitle <- "This dataset includes 20 medications for treating anxiety and depression.<br>
Pregabalin is the most common active substance."

# 📊 plot --------------------------------------------------------------------

df_depression |>
  ggplot() +
  geom_point_svg(aes(x, active_substance), svg = svg_txt, size = 18) +
  geom_text(aes(0.5, as.numeric(active_substance)-0.4, label = active_substance), family = ft, size = 16, colour = txt, lineheight = 0.3, hjust = 0) +
  xlim(0.5, 10) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Anxiety and Depression",
    subtitle = subtitle,
    caption = caption
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.35, colour = txt),
    plot.title = element_text(size = 128, face = "bold", hjust = 0.15),
    plot.subtitle = element_markdown(hjust = 0.2, halign = 0),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
    # axis.text.y = element_text(hjust = 1)
  )

ggsave("scripts/2023/week 11 drugs/drugs.png", height = 10, width = 10)
