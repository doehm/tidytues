# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 43)
df <- dat$patient_risk_profiles |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "#d5bdaf"
accent <- "grey20"


font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
ft <- "bar"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_sex <- df |>
  select(person_id, starts_with("sex")) |>
  mutate(sex = ifelse(sex_female == 1, "Female", "Male")) |>
  select(person_id, sex)

df_base <- df |>
  select(person_id, starts_with("age_group"), starts_with("predicted_risk")) |>
  pivot_longer(starts_with("age_group"), names_to = "age_group") |>
  filter(value == 1) |>
  select(-value) |>
  pivot_longer(starts_with("predicted_risk"), names_to = "risk") |>
  mutate(
    age_group = str_remove(age_group, "age_group_"),
    age_group = case_when(
      age_group == "0_4" ~ "00_04",
      age_group == "5_9" ~ "05-09",
      TRUE ~ age_group
    ),
    age_group = factor(age_group),
    risk = str_remove(risk, "predicted_risk_of_"),
    risk = str_replace_all(risk, "_", " "),
    risk = str_to_sentence(risk),
    risk = case_when(
      risk == "Acute pancreatitis with no chronic or hereditary or common causes of pancreatitis" ~ "Acute pancreatitis",
      risk == "Sudden vision loss with no eye pathology causes" ~ "Sudden vision loss",
      risk == "Sudden hearing loss no congenital anomaly or middle or inner ear conditions" ~ "Sudden hearing loss",
      risk == "Parkinsons disease inpatient or with 2nd diagnosis" ~ "Parkinsons disease",
      risk == "Treatment resistant depression trd" ~ "Treatment resistant depression",
      TRUE ~ risk
    ),
    risk = factor(risk)
    ) |>
  group_by(age_group, risk) |>
  summarise(mean = mean(value), .groups = "drop") |>
  mutate(
    risk_pct = to_pct(mean, 2)
    ) |>
  ungroup() |>
  mutate(
    x = as.numeric(age_group),
    y = -as.numeric(risk)
    )

df_seg <- df_base |>
  group_by(risk) |>
  filter(x == min(x) | x == max(x)) |>
  select(-mean, -risk_pct) |>
  pivot_wider(id_cols = risk, names_from = age_group, values_from = x) |>
  clean_names()

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

xlabs <- tribble(
  ~x, ~y, ~lab,
  1, 0, "0",
  10, 0, "Age",
  19, 0, "95"
)

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_ribbon(aes(x = x, ymin = y-3*mean, ymax = y+3*mean, group = risk), colour = accent, linewidth = 0.5) +
  geom_text(aes(0, y, label = risk), family = ft, size = 16, colour = txt, hjust = 1, vjust = 1, fontface = "italic", nudge_y = 0.25) +
  geom_segment(aes(x = 1, xend = 19, y = 0, yend = 0), colour = txt, linewidth = 0.1) +
  geom_richtext(aes(x, y, label = lab), xlabs, family = ft, size = 12, colour = txt, fontface = "italic", label.colour = NA, fill = bg) +
  coord_fixed(clip = "off") +
  labs(
    caption = caption,
    fill = "Patient risk",
    title = "Pateint Risk Profiles"
    ) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5, margin = margin(b = 30), face = "bold"),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 30)),
    plot.margin = margin(b = 70, t = 100, r = 100, l = 150)
  )

ggsave("scripts/2023/week-43-patient-risk-profiles/patient-risk-profiles.png", height = 12, width = 18)

