# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggfx)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 29)
df <- dat$detectors |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "cyan3"
bg <- "black"
accent <- txt
pal <- c(Human = "grey70", AI = txt)

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Rajdhani", "raj")
showtext_auto()
ft <- "raj"

# 🤼 wrangle -----------------------------------------------------------------

df_base <- df |>
  group_by(model, pred_class) |>
  filter(native == "Yes" | is.na(native)) |>
  summarise(
    n = n(),
    mean = mean(pred_ai)
  ) |>
  group_by(model) |>
  mutate(
    p = n/sum(n),
    sqrt_p = sqrt(p)
  ) |>
  ungroup() |>
  mutate(
    y = as.numeric(as.factor(model)),
    xmin = ifelse(pred_class == "AI", -0.05-sqrt_p, 0.05),
    xmax = ifelse(pred_class == "AI", -0.05, 0.05+sqrt_p),
    ymax = y + sqrt_p
    )

df_detector <- df |>
  count(detector, pred_class, kind) |>
  filter(pred_class == "AI") |>
  group_by(detector) |>
  mutate(
    p = n/sum(n),
    sqrt_p = sqrt(p)
    )

df_text <- df_base |>
  filter(pred_class == "Human") |>
  mutate(
    pred_class = ifelse(model == "Human", "Human", "AI"),
    pct = paste0(round(p*100), "%"),
    text = glue("of the time {model} text is detected as Human{ifelse(model == 'Human', ' for native English writers', '')}")
  )

df_text_ai <- df_base |>
  filter(pred_class == "AI") |>
  mutate(
    pred_class = ifelse(model == "Human", "Human", "AI"),
    pct = paste0(round(p*100), "%"),
    pct = ifelse(p < 0.05, "", pct)
  )

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "GPT detectors R package")

text_lab <- tibble(
  x = c(-0.25, 0.5),
  y = 4.1,
  pred_class = c("AI", "Human")
)

# 📊 plot --------------------------------------------------------------------

a <- 0.05
df_base |>
  ggplot() +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = y, ymax = ymax, fill = pred_class)) +
  with_inner_glow(geom_rect(aes(xmin = xmin+a, xmax = xmax-a, ymin = y+a, ymax = ymax-a), filter(df_base, pred_class == "Human"), fill = bg), colour = pal[1], sigma = 30) +
  with_inner_glow(geom_rect(aes(xmin = xmin+a, xmax = xmax-a, ymin = y+a, ymax = ymax-a), filter(df_base, pred_class == "AI"), fill = bg), colour = pal[2], sigma = 30) +
  geom_text(aes(xmin + 0.5*(xmax-xmin), ymax-0.2, label = pct, colour = pred_class), df_text, family = ft, size = 48, fontface = "bold") +
  geom_text(aes(xmin + 0.5*(xmax-xmin), y+(ymax-y)*0.5, label = pct, colour = pred_class), df_text_ai, family = ft, size = 32, fontface = "bold") +
  geom_text(aes(xmin + 0.5*(xmax-xmin), ymax-0.35, label = str_wrap(text, 15), colour = pred_class), df_text, family = ft, size = 12, lineheight = 0.3, vjust = 1) +

  annotate("text", x = 0.2, y = 4.25, label = "Predicted class by the detector", family = ft, size = 16, colour = txt, fontface = "bold") +
  geom_text(aes(x, y, label = pred_class, colour = pred_class), text_lab, family = ft, size = 20, fontface = "bold") +

  geom_segment(aes(x = 0.5, xend = xmax+0.23, y = y-0.05, yend = y-0.05, colour = pred_class), df_text, linewidth = 0.5) +
  geom_text(aes(xmax+0.05, y, label = model, colour = pred_class), df_text, family = ft, size = 12, fontface = "bold", hjust = 0, vjust = 0) +

  scale_colour_manual(values = pal) +
  scale_fill_manual(values = pal) +
  coord_fixed() +
  labs(
    caption = caption,
    title = "GPT Detectors",
    subtitle = "GPT detectors are finding it increasingly more difficult to detect the difference between Human text
and text generated from an LLM. Detectors include Crossplag, GPTZero, HFOpenAI, OriginalityAI, Quil,
Sapling and ZeroGPT. Expect relentless misinformation."
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.title = element_text(size = 128, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(colour = pal[1]),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = pal[1], hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "none"
  )

ggsave("scripts/2023/week-29-gpt/gpt.png", height = 12, width = 7)

