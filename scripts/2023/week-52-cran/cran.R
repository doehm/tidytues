# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggridges)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 52)
cran <- dat$cran_20221122

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "black"
bg <- "white"
accent <- txt

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Dosis", "dos")
ft <- "dos"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_base <- cran |>
  mutate(
    pos1 = as.numeric(str_extract(version, "[:digit:]+")),
    log_loc_R = log(loc_R),
    f_pos1 = factor(pos1)
  ) |>
  filter(
    pos1 < 10,
    log_loc_R >= 0
    )

df_base_sum <- df_base |>
  group_by(f_pos1) |>
  summarise(
    n_packages = n_distinct(package),
    n_loc_R = sum(loc_R, na.rm = TRUE),
    mu_loc_R = mean(loc_R, na.rm = TRUE)
  ) |>
  mutate(lab = glue("{scales::comma(n_packages)} packages\n{scales::comma(n_loc_R)} total lines of code\n{scales::comma(mu_loc_R)} avg. lines of code per package"))

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_density_ridges_gradient(aes(log_loc_R, f_pos1, fill = stat(x))) +
  geom_text(aes(-1, f_pos1, label = lab), df_base_sum, family = ft, colour = txt, size = 12, hjust = 0, lineheight = 0.3, vjust = -0.2) +
  scale_x_continuous(breaks = log(c(10, 1000, 10000, 100000)), labels = scales::comma(c(10, 1000, 10000, 100000))) +
  scale_fill_gradientn(colours = eyedroppeR::pencil_case$graffiti$div) +
  labs(
    title = "R Packages",
    subtitle = "The number of lines of code in the R folder by the package major version number. In general, the higher
the version number the more code.",
    caption = caption,
    x = "Lines of code (log scale)",
    y = "Major\nversion\nnumber"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 256, hjust = 0.5, margin = margin(b = 15)),
    plot.subtitle = element_markdown(margin = margin(b = 15), hjust = 0.5, size = 64),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text = element_text(),
    axis.title = element_text(margin = margin(t = 15, r = 15)),
    axis.ticks.x = element_line(),
    axis.ticks.length.x = unit(0.2, "cm"),
    axis.line.x = element_line(),
    legend.position = "none"
  )

ggsave("scripts/2023/week-52-cran/cran.png", height = 18, width = 18)


