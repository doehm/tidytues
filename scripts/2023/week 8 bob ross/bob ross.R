# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(cropcircles)
library(ggpath)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 8)

bob_ross <- dat$bob_ross |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Mukta", "mukta")
font_add_google("Satisfy", "baby")
showtext_auto()

ft <- "mukta"

# 🤼 wrangle -----------------------------------------------------------------

luminocity <- map_dbl(bob_ross$img_src, ~{
  x <- image_read(.x) |>
    image_data()
  0.2126*mean(as.numeric(x[1,,])) + 0.7152*mean(as.numeric(x[2,,])) + 0.0722*mean(as.numeric(x[3,,]))
})

bob_ross <- bob_ross |>
  mutate(luminocity = luminocity) |>
  arrange(luminocity)

hex_imgs <- hex_crop(bob_ross$img_src, border_size = 16, border_colour = "black")

n_col <- 15
n_row <- 26
n <- n_col*n_row
df_grid <- expand_grid(
  y = 0:(26-1),
  x = 0:(15-1)
) |>
  mutate(
    x = ifelse(y %% 2 == 1, x + cos(pi/3), x),
    y = y*sin(pi/3),
    painting = hex_imgs[1:n]
  )

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{txt}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{txt}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytuesday{space2}{floppy}{space2}Bob Ross Paintings data")

# 📊 plot --------------------------------------------------------------------

df_grid |>
  ggplot() +
  geom_from_path(aes(x, y, path = painting), width = 0.05) +
  labs(
    caption = caption,
    title = "Bob Ross"
    ) +
  coord_fixed() +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 62, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.title = element_text(family = "baby", size = 300, margin = margin(b = 10), hjust = 0.5),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
)

ggsave("scripts/2023/week 8 bob ross/bob-ross.png", height = n_row, width = n_col+1)
