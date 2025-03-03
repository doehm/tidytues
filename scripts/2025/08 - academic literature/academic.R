# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

article_dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/article_dat.csv')
model_dat <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-02-25/model_dat.csv')

# ✍️ fonts and palettes ------------------------------------------------------

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")

txt <- "white"
bg <- "grey10"
bg1 <- "grey5"
bg2 <- "grey20"
accent <- txt

sunset <- c("#355070", "#6d597a", "#b56576", "#e56b6f", "#eaac8b")
autumn <- c('#1c2426', '#455852', '#9d8e71', '#d4935d', '#cc612b', '#a5371e', '#ab443b')

font_add_google("Poppins", "pop", regular.wt = 200)
ft <- "pop"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df <- model_dat |>
  filter(
    lower != -99,
    upper != -99,
    measure == "OR"
    # compare %in% c("Hispanic", "Black", "Asian", "Other")
  )

df <- article_dat |>
  select(
    `Access to care` = access_to_care,
    `Disparities in received treatment` = treatment_received,
    `Disparities in health outcomes` = health_outcome,
    `Ovarian cancer` = cancer_ovarian,
    `Uterine cancer` = cancer_uterine,
    `Cervical cancer` = cancer_cervical,
    `Vulver cancer` = cancer_vulvar,
    `Other gynocological cancer` = other_gyn_onc,
    Endometriosis = endo,
    `Fibroids` = fibroids,
    `Other gynocologic surgery` = other_gyn_surg,
    Fertinility = fert,
    `Maternal morbidity and mortality` = matmorbmort,
    `Other pregnancy outcomes` = other_preg,
    `Physician diversity and/or training` = phys_div,
    COVID = covid
  ) |>
  pivot_longer(everything(), names_to = "var", values_to = "included") |>
  group_by(var) |>
  summarise(
    yes = mean(included, na.rm = TRUE),
    no = mean(included == 0, na.rm = TRUE)
  ) |>
  pivot_longer(-var, names_to = "included", values_to = "p")

df_lab <- df |>
  filter(included == "yes") |>
  mutate(pct = paste0(round(p*100), "%"))

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, bg)

# 📊 plot --------------------------------------------------------------------

df |>
  ggplot() +
  geom_col(aes(3, p, fill = included)) +
  geom_text(aes(0, 0, label = pct), df_lab, family = ft, colour = txt, size = 24, vjust = 1.6) +
  geom_text(aes(1, 0, label = str_wrap(var, 14)), df_lab, family = ft, colour = txt, size = 10, lineheight = 0.3) +
  geom_segment(aes(x = 2.4, xend = 3.6, y = p, yend = p), filter(df, included == "yes"), colour = sunset[4]) +
  facet_wrap(~var) +
  scale_fill_manual(values = c(no = bg1, yes = sunset[4])) +
  ylim(0, 1) +
  xlim(0, 4) +
  coord_polar("y") +
  labs(
    title = "Academic Literature on Racial and Ethnic Disparities in\nReproductive Medicine in the US",
    subtitle = "The percentage of articles that addressed the given factor",
    caption = caption
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = grid::radialGradient(c("grey25", "grey15"))),
    plot.title = element_text(size = 84, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    strip.text = element_blank(),
    legend.position = "none"
  )


ggsave("scripts/2025/08 - academic literature/academic.png", height = 12, width = 12)

