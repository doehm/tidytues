# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(snakecase)
library(colorspace)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 40)
grants <- dat$grants
details <- dat$grant_opportunity_details

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- "#2f3c68"
pal <- c('#4a166a', '#ce5068', '#f0d35b', '#61aa6f', "#2f3c68")
pal24 <- colorRampPalette(pal)(24)

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
ft <- "bar"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_base <- details |>
  select(starts_with("category"), -category_explanation) |>
  summarise_all(~sum(.x)) |>
  pivot_longer(everything(), names_to = "category") |>
  mutate(category = str_remove(category, "category_")) |>
  mutate(pct = to_pct(value/2000, 3)) |>
  mutate(lab = toupper(to_title_case(category))) |>
  mutate(category = fct_reorder(factor(category), value, min)) |>
  arrange(desc(category))

df_tile <- map_dfr(1:nrow(df_base), ~{
  expand_grid(x = 1:200, y = 1:10) |>
    mutate(
      category = df_base$category[.x],
      fill = ifelse(row_number() <= df_base$value[.x], pal24[.x], NA)
    )
})

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(pal[1])

# 📊 plot --------------------------------------------------------------------

df_tile |>
  ggplot() +
  geom_tile(aes(x = x, y = y, fill = fill), width = 0.9, height = 0.9, colour = "grey90") +
  geom_text(aes(x = 199, y = 5, label = lab), df_base, hjust = 1, vjust = 0, colour = txt, size = 26, fontface = "bold", family = ft) +
  geom_text(aes(x = 199, y = 1, label = pct), df_base, hjust = 1, vjust = 0, colour = txt, size = 16, family = ft) +
  facet_wrap(~category, ncol = 1) +
  scale_fill_identity() +
  labs(
    title = "US Grants",
    subtitle = "Percentage of US grants within each category",
    caption = caption
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 160, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, margin = margin(b = 30)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 80, t = 50, r = 50, l = 50),
    legend.position = "none",
    strip.text = element_blank()
  )

ggsave("scripts/2023/week-40-US-grants/us-grants.png", height = 18, width = 14)

