# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(brms)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 18)

species <- dat$species
survey <- dat$surveys

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "white"
bg <- "grey10"
pal <- c("#e77747", "#247996")
accent <- pal[1]

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
showtext_auto()

ft <- "bar"

# 🤼 wrangle -----------------------------------------------------------------

df <- survey |>
  mutate(
    species = tolower(species),
    species = replace_na(species, "na")
    ) |>
  filter(
    treatment != "removal",
    species != "DS"
    ) |>
  select(wgt, treatment, species) |>
  drop_na()

# fit bayes model
mod <- brm(wgt ~ treatment + species, data = df)

# predict
new_data <- df |>
  filter(
    treatment != "removal",
    species != "ds"
    ) |>
  distinct(species, treatment) |>
  arrange(species, treatment)

draws <- posterior_epred(mod, newdata = new_data)

# make base data set
df_base <- new_data |>
  bind_cols(as_tibble(t(draws))) |>
  pivot_longer(-c(1,2), names_to = "id", values_to = "draws") |>
  group_by(species, treatment) |>
  sample_n(200) |>
  left_join(
    species |>
      mutate(
        species = tolower(species),
        species = replace_na(species, "na")
        ) |>
      select(species, commonname),
    by = "species"
  )

df_sd <- df_base |>
  group_by(species) |>
  summarise(sd = sd(draws))

df_z <- df_base |>
  select(-id) |>
  group_by(species, treatment) |>
  summarise(mu = median(draws)) |>
  pivot_wider(names_from = treatment, values_from = mu) |>
  left_join(df_sd, by = "species") |>
  mutate(z = pnorm(abs(control-exclosure)/sd))

df_means <- df_base |>
  group_by(species) |>
  summarise(
    xmin = min(draws),
    xmax = max(draws)
  ) |>
  left_join(df_z, by = "species")


# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}Portal Projct")

subtitle <- "Long-term ecological research site studying the dynamics of desert rodents, plants, ants and weather
in Arizona. The alpha highlights the greatest difference in means across species. The control group
is typically lower. Distribution is the mean posterior."

title <- glue("Mean weight difference between <span style='color:{pal[1]}'>control</span> and <span style='color:{pal[2]}'>exclosure</span> groups")

# 📊 plot --------------------------------------------------------------------

# NOTE: Not sure if weight is actually in grams. Just taking a punt!

df_base |>
  left_join(df_means, by = "species") |>
  mutate(
    treatment = snakecase::to_sentence_case(treatment),
    commonname = str_wrap(commonname, 20)
    ) |>
  ggplot(aes(fill = treatment)) +
  geom_beeswarm(aes(treatment, draws, fill = treatment, colour = treatment, alpha = z-0.45), cex = 2) +
  facet_wrap(~commonname, scales = "free") +
  scale_fill_manual(values = pal) +
  scale_colour_manual(values = pal) +
  scale_alpha_identity() +
  coord_flip() +
  labs(
    title = title,
    subtitle = subtitle,
    caption = caption,
    fill = "Treatment",
    colour = "Treatment",
    y = "Weight (g)"
  ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_markdown(size = 84, margin = margin(b = 10)),
    plot.subtitle = element_text(margin = margin(b = 20)),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text.x = element_text(size = 24),
    axis.title.x = element_text(margin = margin(t = 10)),
    legend.position = "bottom",
    legend.margin = margin(t = 20),
    panel.spacing = unit(0.03, "npc"),
    strip.text = element_text(size = 32)
  )

ggsave("scripts/2023/week-18-portal/portal.png", height = 12, width = 12)

