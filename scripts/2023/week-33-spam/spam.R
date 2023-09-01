# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(brms)
library(tidybayes)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 33)
spam <- dat$spam |>
  clean_names()

# ✍️ fonts and palettes ------------------------------------------------------

pal <- c('#2b2b2b', '#d9bb64', '#ddc070', '#c2aa68')
txt <- '#c2aa68'
bg <- '#2b2b2b'
accent <- '#c2aa68'
txt2 <- "grey80"

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
ft <- "bar"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

df_base <- spam |>
  mutate(
    spam = ifelse(yesno == "y", 1, 0),
    crl_tot = crl_tot/15000 # scaling so it samples faster
    ) |>
  select(-yesno)

mod <- brm(spam ~ ., data = df_base)

x <- summary(mod)
df_tbl <- x$fixed |>
  as_tibble() |>
  select(1:4) |>
  mutate_all(~round(.x, 3)) |>
  mutate(Parameter = rownames(x$fixed)) |>
  select(5, 1:4) |>
  mutate(
    y = n():1,
    desc = case_when(
      Parameter == "crl_tot" ~ "Total length of uninterrupted sequences of capitals",
      Parameter == "make" ~ "Occurrences of ‘make’, as a percent of total number of words",
      Parameter == "bang" ~ "Occurrences of ‘!’, as percent of total number of characters",
      Parameter == "money" ~ "Occurrences of ‘money’, as percent of total number of characters",
      Parameter == "Intercept" ~ "Intercept",
      Parameter == "dollar" ~ "Occurrences of the dollar sign, as percent of total number of characters",
      Parameter == "n000" ~ "Occurrences of the string ‘000’, as percent of total number of words"
    )
  )

beta <- prepare_predictions(mod)$dpars$mu$fe$b |>
  as_tibble() |>
  clean_names() |>
  pivot_longer(everything())

# this tweet makes me feel so gross!
spam_tweet_text <- "Y000U CAN MAKE S000000 MUCH MONEY!!!!!!!!!!!!!!! $$$$$$$ LET ME SH000W Y000U H000W Y000U CAN BE R!CH. CRYPT000!$!$!"

n_char <- str_length(spam_tweet_text)
bang <- length(str_extract_all(spam_tweet_text, "!")[[1]])/n_char
dollar <- length(str_extract_all(spam_tweet_text, "\\$")[[1]])/n_char
money <- 1/16
make <- 1/16
n000 <- 8/16
crl_tot <- 46/15000

df_tweet <- tibble(dollar = dollar, bang = bang, money = money, n000 = n000, make = make, crl_tot = crl_tot)

pred <- posterior_predict(mod, newdata = df_tweet)[,1]
pred <- exp(pred)/(1+exp(pred))

df_headers <- tibble(
  x = 0:5,
  y = 8,
  label = c(colnames(df_tbl)[1:5], "Description")
)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent, "Spam Email")
subtitle <-
"Posterior predictive distribution of the probability the below text is spam.
Bayesian regression model was fit using {brms}."

# 📊 plot --------------------------------------------------------------------

g_base <- tibble(pred = pred) |>
  ggplot() +
  geom_dots(aes(x = pred), dotsize = 1, stackratio = 1.1, colour = accent) +
  annotate("text", x = 0, y = 0.75, label = str_wrap(spam_tweet_text, 40),
           family = ft, colour = txt, lineheight = 0.3, size = 16, hjust = 0) +
  theme_void() +
  xlim(0, 1) +
  labs(
    caption = caption,
    x = "Probability",
    title = "Spam Detection Model",
    subtitle = subtitle
    ) +
  theme(
    text = element_text(family = ft, colour = txt, size = 48),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(l = 100, r = 100, t = 60, b = 50),
    plot.caption = element_markdown(hjust = 0.5, margin = margin(t = 250), colour = txt2),
    plot.title = element_text(hjust = 0.5, size = 128, margin = margin(b = 20), face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 48, colour = txt2, lineheight = 0.3),
    axis.text.x = element_text(),
    axis.title.x = element_text(margin = margin(t = 20))
  )

# don't look at this code. Bad bad bad.
g_tbl <- df_tbl |>
  ggplot() +
  geom_text(aes(x = 0, y = y, label = Parameter), family = ft, colour = txt, size = 12) +
  geom_text(aes(x = 1, y = y, label = Estimate), family = ft, colour = txt, size = 12) +
  geom_text(aes(x = 2, y = y, label = Est.Error), family = ft, colour = txt, size = 12) +
  geom_text(aes(x = 3, y = y, label = `l-95% CI`), family = ft, colour = txt, size = 12) +
  geom_text(aes(x = 4, y = y, label = `u-95% CI`), family = ft, colour = txt, size = 12) +
  geom_text(aes(x = 4.5, y = y, label = str_wrap(desc, 40)), family = ft, colour = txt, size = 9, lineheight = 0.25, hjust = 0) +
  geom_text(aes(x, y, label = label), df_headers, family = ft, colour = txt, size = 12) +
  annotate("segment", x = -0.5, xend = 5.5, y = 7.5, yend = 7.5, colour = txt) +
  annotate("segment", x = -0.5, xend = 5.5, y = 0.5, yend = 0.5, colour = txt) +
  annotate("segment", x = -0.5, xend = 5.5, y = 0.4, yend = 0.4, colour = txt) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, colour = txt, size = 48),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.margin = margin(l = 20, r = 20, t = 20, b = 20)
  )

g_base +
  inset_element(g_tbl, top = -0.15, bottom = -0.8, left = 0, right = 1) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave("scripts/2023/week-33-spam/spam.png", height = 12, width = 12)

