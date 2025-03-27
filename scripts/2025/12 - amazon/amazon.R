# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

report_words_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-25/report_words_clean.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "grey50"
txt2 <- "grey10"
bg <- "white"
accent <- txt

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
autumn <- c('#1c2426', '#455852', '#9d8e71', '#d4935d', '#cc612b', '#a5371e', '#ab443b')

font_add_google("Raleway", "raleway", regular.wt = 200)
font_add_google("Merriweather", "mw")
ft <- "raleway"
ft <- "mw"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df <- report_words_clean |>
  slice_head(n = 20000) |>
  summarise(text = paste0(word, collapse = " "))

df_year <- report_words_clean |>
  count(year)

df_ai <- report_words_clean |>
  group_by(year) |>
  summarise(
    n = sum(word %in% c("genai", "ai", "artificial")),
    total = n()
  ) |>
  ungroup() |>
  mutate(rate = round(n/total*1e4, 1)) |>
  filter(n > 0)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(txt2, bg)

title <- "References to 'AI'\nin Amazon's\nannual reports"
subtitle <- "per 10,000 words. On average a report
has 22,000 words (excluding stop words)"

# 📊 plot --------------------------------------------------------------------

df |>
  ggplot() +
  geom_text(aes(2014, 10, label = str_wrap(text, 500)), family = ft, colour = txt, size = 4, lineheight = 0.3) +
  geom_col(aes(year, rate), df_ai, fill = "white", colour = txt2, width = 0.5, linewidth = 0.25) +
  geom_text(aes(year, rate + 0.5, label = rate), df_ai, family = ft, colour = txt2, size = 12, fontface = "bold", lineheight = 0.3) +
  annotate("text", x = 2010, y = 9, label = title, family = ft, size = 48, colour = txt2, lineheight = 0.3, fontface = "bold") +
  annotate("text", x = 2010, y = 6, label = subtitle, family = ft, size = 18, colour = txt2, lineheight = 0.3) +
  scale_x_continuous(breaks = 2005:2023, labels = 2005:2023, limits = c(2005, 2023.25)) +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(colour = txt2, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text.x = element_text(colour = txt2, size = 32),
    axis.line.x = element_line(),
    axis.ticks = element_line(),
    axis.ticks.length.x = unit(0.25, "cm")
  )

ggsave("scripts/2025/12 - amazon/amazon.png", height = 12, width = 12)
