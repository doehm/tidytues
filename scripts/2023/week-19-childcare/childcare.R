# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(mapdata)
library(biscale)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 19)

costs <- dat$childcare_costs
counties <- dat$counties

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- txt
bi <- "BlueGold"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Fira Sans", "fira")
showtext_auto()
ft <- "fira"

# 🤼 wrangle -----------------------------------------------------------------

df <- costs |>
  filter(study_year == 2018) |>
  group_by(county_fips_code) |>
  summarise(
    poverty = mean(pr_f, na.rm = TRUE),
    cost = mean(mfcc_infant, na.rm = TRUE)
  ) |>
  drop_na() |>
  bi_class(x = poverty, y = cost, style = "quantile", dim = 3) |>
  left_join(counties, by = "county_fips_code") |>
  mutate(county = str_remove(county_name, " County"))

df_county <- map_data("county") |>
  as_tibble() |>
  mutate(county = str_to_title(subregion)) |>
  left_join(df, by = "county")

legend <- bi_legend(
  pal = bi,
  dim = 3,
  xlab = "Higher rate of poverty ",
  ylab = "Higher cost of services ",
  size = 32)

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}National Database of Childcare Prices")

subtitle <- str_wrap("Since 1998, the federal Administration for Children and Families (ACF) at the U.S.
Department of Health and Human Services has required states to conduct a study of
childcare market prices to evaluate adequacy of state reimbursement rates for the purpose of
demonstrating equal access to childcare for low-income families. States use the results of
market price surveys to inform rate-setting policy and to establish maximum reimbursement
rates for children served through childcare assistance programs. While market price surveys
may include multiple segments of the childcare market, at their core they are designed to
collect and report prices on providers that are operating in the regulated market for childcare.", 200)


# 📊 plot --------------------------------------------------------------------

g_base <- ggplot() +
  geom_polygon(aes(long, lat, group = group, fill = bi_class), df_county, colour = "black", size = 0.1) +
  bi_scale_fill(pal = bi, dim = 3) +
  coord_map() +
  labs(
    title = "Cost of Childcare Services and Poverty Rate across the US",
    subtitle = subtitle,
    caption = caption
  ) +
  theme_void() +
  theme(
    legend.position = "none",
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.title = element_text(size = 84, hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = 36),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 0, t = 0, r = 50, l = 50)
  )

g_base +
  inset_element(legend, left = -0.05, right = 0.25, bottom = -0.05, top = 0.25)

ggsave("scripts/2023/week-19-childcare/childcare.png", height = 12, width = 18)
