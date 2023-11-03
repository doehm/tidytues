# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(packcircles)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 44)
horror <- dat$horror_articles |>
  mutate(id = 1:n())

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "white"
bg <- "black"
accent <- "white"
pal <- c("true" = "#f0d35b", "false" = "#4a166a")

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Chivo", "chivo")
font_add_google("Butcherman", "butcher")
ft <- "chivo"
ft1 <- "butcher"

showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

three_words <- rep(NA, nrow(horror))

for(k in 1:nrow(horror)) {
  cat(k, "\n")
  three_words[k] <- create_chat_completion(
    model = "gpt-4",
    messages = list(
      list(
        "role" = "user",
        "content" = glue("Summarise the following text in 3 adjectives:

        {horror$claim[k]}")
      )
    )
  )$choices$message.content
}

write_rds(three_words, "scripts/2023/week-44-horror-articles/three-words.rds")

df_words_0 <- map_dfr(1:length(three_words), ~{
  words <- str_remove_all(three_words[.x], "\\.") |>
    str_split_1(",") |>
    str_trim() |>
    str_to_title()
  tibble(
    id = .x,
    words = words
    )
})

df_words <- df_words_0|>
  left_join(horror, by = "id") |>
  count(rating, words) |>
  filter(rating %in% c("true", "false")) |>
  group_by(words) |>
  mutate(N = sum(n)) |>
  filter(N > 2) |>
  ungroup() |>
  mutate(
    words = fct_reorder(words, N, max),
    alpha = min_max(N, 0.5, 1)
    ) |>
  mutate(id = 1:n())

df_lab <- df_words |>
  pivot_wider(id_cols = c(words, alpha), names_from = rating, values_from = n) |>
  mutate(
    true = replace_na(true, 0),
    false = replace_na(false, 0),
    lab = paste(false, "-", words, "-", true)
    )

df_claims <- horror |>
  filter(rating %in% c("true", "false")) |>
  left_join(df_words_0, by = "id") |>
  group_by(words) |>
  sample_n(1) |>
  ungroup() |>
  sample_n(9) |>
  mutate(
    x = seq(1, 5, length = 9),
    y = 1:9 %% 2
    )

df_3_words <- df_words_0 |>
  inner_join(
    df_claims |>
      select(id, x, y, rating),
    by = "id") |>
  group_by(id) |>
  mutate(x = x + seq(0, 0.35, length = 3))


# circles

df_base <- map(unique(df_words$rating), ~{

  df_x <- df_words |>
    filter(rating == .x)

  packing <- circleProgressiveLayout(df_x$n, sizetype='area') |>
    as_tibble()

  df_centers <- df_x |>
    bind_cols(packing) |>
    mutate(id = 1:n())

  df_poly <- circleLayoutVertices(packing, npoints = 50) |>
    left_join(
      df_centers |>
        select(id, n, rating, words),
      by = "id"
    ) |>
    as_tibble()

  list(
    centers = df_centers,
    poly = df_poly
  )

}) |>
  set_names(unique(df_words$rating))

df_centers <- map_dfr(df_base, "centers") |>
  mutate(
    group = paste(rating, id),
    alpha = min_max(n, 0.5, 1)
    )

df_titles <- df_centers |>
  group_by(rating) |>
  summarise(x = mean(x))

df_poly <- map_dfr(df_base, "poly") |>
  mutate(
    group = paste(rating, id),
    alpha = min_max(n, 0.3, 1)
    )


# experimenting with the article images - not used
k <- 12
horror$title[k]
horror$subtitle[k]
page <- read_html(horror$url[k])

img_el <- page |>
  html_elements("img")

id <- img_el |>
  html_attr("id") |>
  str_which("cover-main")

img_el[id] |>
  html_attr("src") |>
  image_read()

  # 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)
subtitle <- "Horror legends from Snopes.com
Each claim that is validated as either True or False was summarised by GPT-4 in 3 words. These are the words."

# 📊 plot --------------------------------------------------------------------

g_bar <- df_words |>
  mutate(n = ifelse(rating == "true", n, -n)) |>
  ggplot() +
  geom_chicklet(aes(words, n, fill = rating, alpha = alpha), colour = bg, radius = grid::unit(4, "pt"), width = 0.5) +
  geom_text(aes(words, 0, label = lab, alpha = alpha), df_lab, size = 8, family = ft, colour = txt, fontface = "italic", lineheight = 0.3, nudge_x = 0.5) +
  scale_alpha_identity() +
  scale_fill_manual(values = pal) +
  coord_flip(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    legend.position = "none"
  )

g_base <- ggplot() +
  geom_polygon(aes(x, y, group = group, fill = rating, alpha = alpha), df_poly, colour = "black") +
  geom_text(aes(x, y, size = n, group = group, label = paste0(words, "\n", n), alpha = alpha), df_centers, family = ft, colour = txt, fontface = "italic", lineheight = 0.3) +
  geom_text(aes(x, -9, label = str_to_title(rating), colour = rating), df_titles, size = 64, family = ft, fontface = "italic") +
  facet_wrap(~rating, nrow = 1) +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  scale_size_continuous(range = c(6,24)) +
  scale_alpha_identity() +
  theme_void() +
  theme(legend.position="none") +
  coord_equal() +
  labs(
    title = "HORROR ARTICLES",
    subtitle = subtitle,
    caption = caption
  ) +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = "grey70"),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 256, hjust = 0.5, family = ft1),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    panel.spacing = unit(4, "cm"),
    strip.text = element_blank()
  )

g_base +
  inset_element(g_bar, left = 0.43, right = 0.63, top = 0.9, bottom = 0.1) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave("scripts/2023/week-44-horror-articles/horror-articles.png", height = 14, width = 24)

