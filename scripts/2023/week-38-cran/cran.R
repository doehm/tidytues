# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(igraph)
library(ggnetwork)
library(ggforce)
library(ggbump)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 38)

cran <- dat$cran_20230905 |>
  clean_names()
authors <- dat$package_authors |>
  clean_names()
edges <- dat$cran_graph_edges |>
  clean_names()
nodes <- dat$cran_graph_nodes |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "beige"
bg <- "grey30"
accent <- "beige"

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Kanit", "kan")
ft <- "kan"
showtext_auto()


# functions ---------------------------------------------------------------

extract_imports <- function(packages, imports) {
  map_dfr(1:length(packages), ~{
    x <- str_split(imports[.x], ",")[[1]]
    x <- str_remove(x, "\\n")
    x <- str_remove(x, "\\(.+\\)")
    x <- str_trim(x)
    tibble(
      package = packages[.x],
      imports = x
    )
  }) |>
    drop_na()
}

times_together <- function(x, y) {
  k <- 0
  map2_dbl(x, y, ~{
    cat(k, .x, .y, "\n")
    imports |>
      filter(imports %in% c(.x, .y)) |>
      count(package) |>
      filter(n == 2) |>
      nrow()
  })
}

# 🤼 wrangle -----------------------------------------------------------------

df_base <- cran |>
  filter(str_sub(package, 1, 2) == "gg")

df <- extract_imports(df_base$package, df_base$imports) |>
  bind_rows(extract_imports(df_base$package, df_base$suggests)) |>
  bind_rows(extract_imports(df_base$package, df_base$depends)) |>
  group_by(imports) |>
  mutate(n = n()) |>
  ungroup() |>
  mutate(imports = fct_reorder(factor(imports), n, max)) |>
  filter(
    !is.na(imports),
    imports != ""
    )

imports <- extract_imports(df_base$package, df_base$imports)
uniq_imports <- unique(imports$imports)
uniq_imports <- uniq_imports[!is.na(uniq_imports)]

df_import_grid <- t(combn(uniq_imports, 2)) |>
  as_tibble() |>
  set_names(c("import1", "import2")) |>
  left_join(
    tibble(
      imports = uniq_imports,
      x = 1:length(uniq_imports)
    ),
    by = c("import1" = "imports")
  ) |>
  left_join(
    tibble(
      imports = uniq_imports,
      y = 1:length(uniq_imports)
    ),
    by = c("import2" = "imports")
  )

dfk <- df_import_grid |>
  mutate(n = times_together(import1, import2))

dfk_labs <- dfk |>
  group_by(x) |>
  slice_min(y)

dfk_labs_x <- dfk |>
  distinct(import1, x) |>
  mutate(y = -max(dfk$y)-3)

df_circles <- tibble(
  x0 = c(20, 220),
  y0 = -c(20, 75),
  r = c(30, 92)
  )

df_bump <- tibble(
  x = c(50, 220-92),
  y = df_circles$y0
)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "Data")

title <- "R Package Dependencies"
subtitle <- "Frequency of how many times a pair of packages are dependencies for a package beginning with 'gg' i.e. ggplot2 extensions, of which there are 194.
No surprises that the 'ggplot2' and 'dplyr' combo is right up there."

# 📊 plot --------------------------------------------------------------------

g_base <- dfk |>
  mutate(n = ifelse(n == 0, NA, n)) |>
  ggplot() +

  geom_circle(aes(x0 = x0, y0 = y0, r = r), df_circles, fill = lighten(bg, 0.1)) +

  geom_tile(aes(x, -y, fill = n), width = 1, height = 1, colour = "black", linewidth = 0.1) +
  geom_text(aes(x+3, y = -y, label = import2), dfk_labs, family = ft, colour = txt, size = 4, hjust = 0) +
  geom_text(aes(x, y = y, label = import1), dfk_labs_x, family = ft, colour = txt, size = 4, hjust = 1, angle = 90) +

  geom_bump(aes(x, y), df_bump, colour = txt) +
  geom_circle(aes(x0 = x0, y0 = y0, r = r), df_circles, fill = NA, colour = txt) +

  scale_fill_viridis_c(option = "rocket", direction = -1, na.value = bg, breaks = seq(10, 50, 10), labels = seq(10, 50, 10)) +
  scale_colour_viridis_c(option = "rocket", direction = -1, na.value = bg) +
  coord_fixed(clip = "off") +
  labs(
    caption = caption,
    fill = "",
    title = title,
    subtitle = subtitle
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 160, hjust = 0, margin = margin(l = 200, b = 30), face = "bold"),
    plot.subtitle = element_text(hjust = 0, margin = margin(l = 200), size = 48),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "bottom"
  )

g40 <- dfk |>
  mutate(n = ifelse(n == 0, NA, n)) |>
  filter(
    x <= 40,
    y <= 40
  ) |>
  ggplot() +
  geom_tile(aes(x, -y, fill = n), width = 1, height = 1, colour = "black", linewidth = 0.1) +
  geom_text(aes(x+1, y = -y, label = import2), filter(dfk_labs, x <= 40, y <= 40), family = ft, colour = txt, size = 10, hjust = 0) +
  geom_text(aes(x, y = -41, label = import1), filter(dfk_labs_x, x <= 40), family = ft, colour = txt, size = 10, hjust = 1, angle = 90) +
  scale_fill_viridis_c(option = "rocket", direction = -1, na.value = bg, breaks = seq(0, max(dfk$n), length = 6), labels = seq(0, max(dfk$n), length = 6)) +
  scale_colour_viridis_c(option = "rocket", direction = -1, na.value = bg) +
  coord_fixed(clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = NA, colour = NA),
    legend.position = "none"
  )

g_base +
  inset_element(g40, left = 0.525, right = 0.9, top = 0.875, bottom = 0.5) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave("scripts/2023/week-38-cran/cran.png", height = 20, width = 20)

