# https://github.com/rfordatascience/tidytuesday

#

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggwordcloud)

# 💾 load data ---------------------------------------------------------------

df <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv') |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
perfume <- c("#EDE3E1", "#F3D9D7", "#F8CFCF", "#FCD7D8", "#FDE1E1", "#FCEBE9")
font_add_google("Poppins", "pop", regular.wt = 200)
ft <- "pop"
pal <- perfume
bg <- pal[3]
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

make_word_df <- function(df, x) {
  tibble(
    word = str_split(df[x], ", ") |>
      reduce(c),
    category = x
  )
}

clean_brands <- function(x) {

  id <- str_detect(x, "/")

  ifelse(
    id,
    str_extract(x, ".+/") |>
      str_remove_all("/") |>
      str_trim(),
    x
  )

}

categories <- c("main_accords", "top_notes", "middle_notes", "base_notes")

df_top_n <- df |>
  filter(rating_count > 100) |>
  arrange(desc(rating_value)) |>
  slice_head(n = 50)

df_word <- map_dfr(categories, ~make_word_df(df_top_n, .x)) |>
  filter(!is.na(word)) |>
  mutate(
    word = str_remove_all(word, '"'),
    word = str_remove_all(word, '\\n'),
    word = tolower(word)
    ) |>
  count(category, word) |>
  mutate(
    word = str_to_sentence(word),
    category = factor(category, levels = rev(c("main_accords", "base_notes", "middle_notes", "top_notes")))
    ) |>
  arrange(category, desc(n)) |>
  group_by(category) |>
  slice_head(n = 50)

# 🔡 text --------------------------------------------------------------------


# 📊 plot --------------------------------------------------------------------

df_word |>
  ggplot() +
  geom_text_wordcloud(aes(label = word, size = n), family = ft, shape = "triangle-forward") +
  scale_size_area(max_size = 60) +
  facet_wrap(~category, ncol = 1) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    # plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    strip.text = element_blank()
  )

# save --------------------------------------------------------------------

ggsave("scripts/2024/week-50-perfume/perfume.png", height = 16, width = 12)

# bar chart of brands -----------------------------------------------------

df |>
  count(brand) |>
  mutate(
    brand = clean_brands(brand),
    brand = fct_reorder(brand, n)
  ) |>
  arrange(desc(n)) |>
  slice_head(n = 30) |>
  ggplot() +
  geom_col(aes(brand, n), fill = txt) +
  scale_y_continuous(position = "right") +
  coord_flip() +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    # plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    strip.text = element_blank(),
    axis.text.y = element_text(hjust = 1),
    axis.line.x = element_line(),
    axis.ticks = element_line(),
    axis.ticks.length.x = unit(0.25, "cm"),
    axis.text = element_text()
  )

# save --------------------------------------------------------------------

ggsave("scripts/2024/week-50-perfume/brands.png", height = 12, width = 10)

