# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggbrick)

# 💾 load data ---------------------------------------------------------------

cdc_datasets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-11/cdc_datasets.csv')
fpi_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-11/fpi_codes.csv')
omb_codes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-11/omb_codes.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "white"
bg <- "black"
accent <- txt
pal <- eyedroppeR::pencil_case$d10$cat

font_add_google("Poppins", "pop", regular.wt = 200)
ft <- "pop"
showtext_auto()

# 🚙 functions ---------------------------------------------------------------

# 🤼 wrangle -----------------------------------------------------------------

df <- cdc_datasets |>
  left_join(fpi_codes, join_by(program_code == program_code_pod_format)) |>
  count(program_name, sort = TRUE) |>
  mutate(p = n/sum(n)) |>
  drop_na() |>
  mutate(
    program_name = str_wrap(program_name, 30),
    program_name = paste0(str_pad(as.numeric(fct_reorder(program_name, n)), pad = "0", width = 2), ". ", program_name)
  )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, bg)

# 📊 plot --------------------------------------------------------------------

df |>
  ggplot() +
  geom_brick(aes(0, n, fill = program_name), bricks_per_layer = 12, type = "random") +
  annotate("text", x = 0, y = 1260, label = "1,245", family = ft, colour = txt, size = 64, vjust = 0) +
  scale_fill_manual(values = pal) +
  labs(
    title = "Saved CDC Datasets",
    subtitle = str_wrap("The Trump administration has ordered agencies to purge their websites
    of any references to topics such as LGBTQ+ rights. An effort is underway to back up
    this publicly funded data before it is lost. This week's dataset contains metadata
    about CDC datasets backed up on archive.org.", 75),
    caption = caption,
    fill = "Program name"
  ) +
  coord_brick(bricks_per_layer = 12, clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0, margin = margin(b = 10)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.text = element_text(margin = margin(b = 5, l = 5, t = 5)),
    axis.line.x = element_line(colour = txt),
    legend.position = "right"
  )

ggsave("scripts/2025/06 - cdc-datasets/cdc-datasets.png", height = 20, width = 10)
