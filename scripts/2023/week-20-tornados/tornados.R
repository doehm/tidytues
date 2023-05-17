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

dat <- tidytuesdayR::tt_load(2023, week = 20)

tornados <- dat$tornados

# ✍️ fonts and palettes ------------------------------------------------------

txt <- "grey20"
bg <- "white"
accent <- "grey20"
pal <- c("#2F485C", "#3E5E77", "#597992", "#7B8EA6", "#93839E", "#8D6C89", "#7A5570", "#5C4155")
pal <- c('#2A4831', '#42662F', '#69812E', '#978F2C', '#BC7522', '#C44014', '#AC180A', '#720F08')
pal <- c('#0a080b', '#352b43', '#865d7b', '#98819d')
pal <- c(NA, colorRampPalette(pal)(12))
cat5 <- c('#42662F', '#69812E', '#978F2C', '#BC7522', '#C44014', '#AC180A')
cat5 <- c("#3E5E77", "#597992", "#7B8EA6", "#93839E", "#8D6C89", "#7A5570")

font_add("fa-brands", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-brands-400.ttf")
font_add("fa-solid", regular = "assets/fonts/fontawesome-free-6.2.0-web/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
showtext_auto()

ft <- "bar"

# 🤼 wrangle -----------------------------------------------------------------

df_state <- map_data("state")

df_base <- tornados |>
  filter(
    !st %in% c("HI", "AK", "VI"),
    slat > 25
    ) |>
  mutate(
    month = format(date, "%b"),
    month = factor(month, levels = c("Dec", "Mar", "Jun", "Sep", "Jan", "Apr", "Jul", "Oct", "Feb", "May", "Aug", "Nov"))
    # slat = round(slat,1),
    # slon = round(slon, 1)
    )

df_col <- tornados |>
  mutate(
    wd_cat = cut(wid, 6),
    wd_cat = factor(mag, levels = 5:0),
    month = mo %% 12
    ) |>
  dplyr::select(dy, month, wd_cat, mo) |>
  mutate(
    dm = paste0(str_pad(month, side = "left", width = 2, pad = 0), str_pad(dy, side = "left", width = 2, pad = 0)),
    mo = mo %% 12
  ) |>
  count(dm, wd_cat, mo) |>
  arrange(dm) |>
  drop_na()

df_labs <- df_base |>
  distinct(mo, month) |>
  mutate(mo = mo %% 12) |>
  left_join(
    df_col |>
      filter(str_sub(dm, 3, 4) == "15") |>
      distinct(mo, dm),
    by = "mo"
  )

df_base_labs <- df_base |>
  distinct(month) |>
  mutate(
    x = mean(range(df_state$long)),
    y = max(df_state$lat)+2
  )

df_seasons <- df_base_labs |>
  filter(month %in% c("Dec", "Mar", "Jun", "Sep")) |>
  mutate(season = c("Winter", "Spring", "Summer", "Autumn"))

# 🔡 text --------------------------------------------------------------------

mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
space2 <- glue("<span style='color:{bg}'>--</span>") # can't believe I'm doing this
caption <- glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}NOAA's National Weather Service Storm Prediction Center")

subtitle <- "Tornadoes are most prevalent in the United States during the spring and
early summer months, typically between March and July. This period corresponds to
the peak of tornado season when atmospheric conditions are favorable for their
formation. In the central and southern regions, such as Tornado Alley, tornadoes
are particularly common during late spring, specifically in the months of April,
May, and June. During these months, the collision of warm, moist air from the Gulf
of Mexico with cooler, drier air from the north creates a volatile environment,
leading to the formation of powerful thunderstorms and tornadoes. However, it's
important to note that tornadoes can occur at any time of the year and in various
regions across the United States, although their frequency and intensity tend to be
highest during the aforementioned months."

# 📊 plot --------------------------------------------------------------------

# 28

g_base <- df_base |>
  ggplot() +
  geom_polygon(aes(long, lat, group = group), df_state, colour = txt, fill = NA, linewidth = 0.1) +
  geom_density2d_filled(aes(slon, slat), alpha = 0.6, contour_var = "count") +
  geom_text(aes(x, y, label = month), df_base_labs, family = ft, size = 14, colour = txt) +
  geom_text(aes(x, y+3, label = season), df_seasons, family = ft, size = 20, fontface = "bold", colour = txt) +
  scale_fill_manual(values = pal, na.value = NA) +
  facet_wrap(~month, nrow = 3) +
  coord_map(clip = "off") +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    legend.position = "none",
    strip.text = element_blank()
  )

g_col <- df_col |>
  ggplot() +
  geom_col(aes(dm, n, fill = wd_cat), width = 0.5) +
  geom_text(aes(dm, -80, label = month), df_labs, family = ft, colour = txt, size = 12) +
  facet_wrap(~mo, nrow = 1, scales = "free_x") +
  scale_fill_manual(values = rev(cat5)) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank()
  )

g_base /
  g_col +
  plot_layout(ncol = 1, heights = c(4, 1)) +
  plot_annotation(
    title = "Tornado Patterns in the United States since the 1950s",
    subtitle = str_wrap(subtitle, 240),
    caption = caption,
    theme = theme(
      text = element_text(family = ft, size = 48, lineheight = 0.3, colour = txt),
      plot.title = element_text(size = 128, face = "bold"),
      plot.subtitle = element_text(size = 36),
      plot.background = element_rect(fill = bg, colour = bg),
      plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
      plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
      legend.position = "none"
    )
  )

ggsave("scripts/2023/week-20-tornados/tornados.png", height = 14, width = 20)

