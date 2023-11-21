# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(ggimage)
library(cropcircles)
library(colorspace)
library(eyedroppeR)

# 💾 load data ---------------------------------------------------------------

rladies_chapters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-21/rladies_chapters.csv')

# ✍️ fonts and palettes ------------------------------------------------------

extract_pal(12, "https://rladies.org/images/logo.png")
pal <- c('#2D1C2A', '#40263F', '#512A51', '#613161', '#713072', '#833C84', '#F0EFF1', '#D5D3D7', '#BBB8BF', '#A59FAA', '#918A96', '#795F79')

txt <- "grey20"
bg <- lighten("purple", 0.9)
accent <- pal[6]

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add_google("Barlow", "bar")
font_add_google("Josefin Sans", "jose")
ft <- "bar"
ft1 <- "jose"
showtext_auto()

# 🤼 wrangle -----------------------------------------------------------------

# logo
crop_hex(
  "https://secure.meetupstatic.com/photos/event/8/d/7/b/600_466896219.jpeg",
  "scripts/2023/week-47-rladies/rladies-logo.png",
  border_size = 8, border_colour = pal[6], bg_fill = "white")

df_base <- rladies_chapters |>
  count(year, location)

df_year <- rladies_chapters |>
  count(year)

tiles <- paste0("scripts/2023/week-47-rladies/", c("in-person.png", "online.png"))

df_legend <- tibble(
  x = 2010,
  y = c(450, 300),
  lab = c("In person\nattendance", "Online\nattendance"),
  tiles = crop_square(tiles, border_size = 3)
)

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

title <- "Ladies Chapter Events"
subtitle <- "R-Ladies Global is an inspiring story of community, empowerment,
and diversity in the field of data science. Founded by Gabriela de Queiroz, R-Ladies
began as a grassroots movement with a simple mission: to promote gender diversity in
the R programming community and provide a welcoming space for women and gender
minorities to learn, collaborate, and excel in data science. All events were held in
person until 2020 when attendance also become virtual. Assuming this was largely
influenced by COVID, but great it opened access for others."

# 📊 plot --------------------------------------------------------------------

df_base |>
  ggplot() +
  geom_col_pattern(
    aes(x = year, y = n, pattern_filename = location),
    pattern = 'image', pattern_type = 'tile', fill = 'white',
    colour = 'black', pattern_filter = 'box', pattern_scale = 0.5
  ) +
  geom_text(aes(year, n + 10, label = n), df_year, family = ft, colour = txt, size = 16, vjust = 0) +
  geom_image(aes(x, y, image = tiles), df_legend, size = 0.12) +
  geom_text(aes(2011, y, label = lab), df_legend, family = ft, colour = txt, size = 20, hjust = 0, lineheight = 0.3) +
  annotate("text", x = 2010.9, y = 1025, label = title, family = ft1, colour = pal[6], size = 56, hjust = 0, lineheight = 0.25, fontface = "bold", vjust = 1) +
  annotate("text", x = 2009, y = 850, label = str_wrap(subtitle, 80), family = ft, colour = txt, size = 16, hjust = 0, lineheight = 0.3, vjust = 1) +
  geom_image(aes(2009.7, 1000, image = "scripts/2023/week-47-rladies/rladies-logo.png"), size = 0.15) +
  scale_pattern_filename_discrete(choices = tiles) +
  scale_x_continuous(breaks = 2012:2023, labels = 2012:2023, limits = c(2008, NA)) +
  coord_cartesian(clip = "off") +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50),
    axis.text.x = element_text(),
    legend.position = "none"
  )

ggsave("scripts/2023/week-47-rladies/rladies.png", height = 12, width = 17)

