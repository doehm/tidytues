# https://github.com/rfordatascience/tidytuesday

library(tidyverse)
library(showtext)
library(patchwork)
library(janitor)
library(glue)
library(ggtext)
library(openai)
library(magick)
library(snakecase)
library(cropcircles)
library(ggpath)
library(colorspace)
library(geofacet)

# you need to put your key in here
Sys.setenv(OPENAI_API_KEY = "xxxx")

msg <- list(
  list(
    "role" = "user",
    "content" = "show me an image of a perfect monstera indoor plant"
  )
)

# 💾 load data ---------------------------------------------------------------

dat <- tidytuesdayR::tt_load(2023, week = 41)
places <- dat$haunted_places |>
  mutate(id = 1:n()) |>
  select(id, everything())

# ✍️ fonts and palettes ------------------------------------------------------

blue_pal <- c('#0A1530', '#152E64', '#1A458D', '#1C5EB0', '#1D7DCE', '#3699E2', '#63B0EE', '#98C8F7')
gold_pal <- colorRampPalette(c("grey", "gold3"))(8)
txt <- "grey90"
bg <- darken(blue_pal[1], 0.9)
accent <- gold_pal[2]

font_add("fa-brands", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-brands-400.ttf")
font_add("fa-solid", regular = "../../Assets/Fonts/fontawesome/webfonts/fa-solid-900.ttf")
font_add("metal-gothic", regular = "assets/fonts/metal-gothic/MetalGothicDemoVersionRegular-WyB9A.ttf")
font_add_google("Barlow", "bar")
ft <- "bar"
ft_title <- "metal-gothic"
showtext_auto()

# 🪅 functions ------------------------------------------------------------

# just to speed up the manual process of going to midjourney and saving down
# the image from the url
save_mj_image <- function(url) {
  path <- "scripts/2023/week-41-haunted-places/ghosts/"
  file_nums <- str_remove(list.files(path), ".png") |>
    as.numeric()
  num <- str_pad(max(file_nums) + 1, width = 3, pad = 0, side = "left")
  new_path <- paste0(path, num, ".png")
  image_read(url) |>
    image_write(new_path)
}

# helper for quickly reading a description
read_description <- function(id) {
  x <- places$description[id]
  cat(str_wrap(x, 120))
}

# creates the prompt for summarising the description
prompt <- function(id) {
  desc <- places$description[id]
  prompt <- glue("the following text describes a haunted place.
  Provide a summary description of what the ghost, entity or environment looks like in 50 words or less.

  Here is the text:

  {desc}")
  list(
    list(
      "role" = "user",
      "content" = prompt
    )
  )
}

# call to gpt-4 to summarise the description
description_summary <- function(id) {
  text <- places$description[id]
  gpt_output <- create_chat_completion(
    model = "gpt-4",
    messages = list(
      list(
        "role" = "user",
        "content" = glue("Summarise the following text in 100 words or less:

        {text}")
        )
      )
  )
  gpt_output$choices$message.content
}

# use gpt-4 to describe the haunting entity
ghost_description <- function(id) {
  gpt_output <- create_chat_completion(
    model = "gpt-4",
    messages = prompt(id)
    )
  gpt_output$choices$message.content
}

# use gpt to create a prompt to generate an image from the ghost description
# this could be improved by training the LLM first with a set of midjourney
# prompts
ghost_image_prompt <- function(desc, location) {
  prompt <- list(
    list(
      "role" = "user",
      "content" = glue("create a prompt for a hyper realistic image of what the entity
      looks like and the enviornment in 20 words or less from the following description:

      {desc}
      ")
    )
  )
  out <- create_chat_completion(
    model = "gpt-4",
    messages = prompt
  )
  img_prompt <- str_replace_all(out$choices$message.content, "\\.", ",")
  img_prompt <- paste0(img_prompt, ", dark, smokey, grainy")
  glue("{img_prompt} at the {location}")
}

# run all the bits together and add to the sample data frame
# if dalle == FALSE it will just produce the prompt for using
# in midjourney
run_gpt <- function(df, ids, dalle = FALSE) {
  n <- nrow(df)
  desc_summary <- rep(NA, n)
  ghost_summary <- rep(NA, n)
  image_prompt <- rep(NA, n)
  img <- rep(NA, n)
  for(k in 1:n){
    id_k <- df$id[k]
    desc_summary[k] <- description_summary(id_k)
    ghost_summary[k] <- ghost_description(id_k)
    image_prompt[k] <- ghost_image_prompt(ghost_summary[k], df$location[k]) |>
      str_remove_all('"')
    if(dalle) {
      cat(glue("\n\n{id_k} Title: {places$location[id_k]} | Prompt: {image_prompt[k]}\n\n"))
      image_prompt[k] <- str_remove_all(image_prompt[k], "decapitation|decapitated|noose")
      img[k] <- glue("scripts/2023/week-41-haunted-places/DALL-E/{id_k}.png")

      tryCatch(
        {
          img_k <- create_image(image_prompt[k])
          image_read(img_k$data$url) |>
            image_write(img[k])
        },
        error = function(e) print(glue("{k} / id {id_k} failed"))
      )

    }
  }
  out <- df |>
    bind_cols(tibble(desc_summary = desc_summary)) |>
    bind_cols(tibble(ghost_summary = ghost_summary)) |>
    bind_cols(tibble(image_prompt = image_prompt))

  if(dalle) {
    out <- out |>
      bind_cols(tibble(img = img))
  }

  out
}

# other helpers

# split the title into words
split_title <- function(x) {
  x_ls <- str_split(str_remove_all(x, "[:punct:]+"), "[:space:]+")
  map_dfr(1:length(x), ~{
    tibble(
      id = .x,
      type = x_ls[[.x]]
    )
  })
}

# sample the places for the main image
sample_places <- function(df, n, seed = 9369) {
  set.seed(seed)
  df |>
    sample_n(n) |>
    ungroup()
}

# find the number of words in a string
n_words <- function(x) {
  n_ls <- str_split(x, "[:space:]")
  map_dbl(n_ls, length)
}

# helper to quickly read the description summary and the image prompt
# to see if it's ok or complete rubbish
read_summary_and_prompt <- function(id) {
  cat("\n", df_base$desc_summary[id], "\n\n")
  cat(df_base$image_prompt[id], "\n\n")
}

# 🤼 wrangle -----------------------------------------------------------------

# get the location type
df_type <- split_title(places$location) |>
  mutate(
    type = tolower(str_remove_all(type, '"')),
    type = case_when(
      type == "st" ~ "street",
      type == "rd" ~ "road",
      type == "ave" ~ "avenue",
      type == "graveyard" ~ "cemetery",
      type == "grave" ~ "cemetery",
      TRUE ~ type
    )
    ) |>
  mutate(type = str_to_title(type)) |>
  group_by(id) |>
  slice_tail(n = 1) |>
  ungroup()

# summarise the location type
df_n_type <- df_type |>
  count(type) |>
  arrange(desc(n)) |>
  slice_head(n = 20)

# summarise state
df_state <- places |>
  count(code = state)

# sample the top types
places_to_sample <- df_n_type |>
  slice_head(n = 3)

# create the sample data frame
df_sample <- places |>
  left_join(
    df_type |>
      semi_join(df_n_type, by = "type") |>
      group_by(id) |>
      slice_head(n = 1),
    by = "id"
  ) |>
  drop_na() |>
  mutate(desc_length = n_words(description)) |>
  filter(desc_length >= 30) |>
  semi_join(places_to_sample, by = "type") |>
  group_by(type) |>
  sample_places(5)

# call GPT
df_base <- df_sample |>
  run_gpt(dalle = TRUE)

# writing to store gpt output
if(FALSE) {
  write_csv(df_base, file = "scripts/2023/week-41-haunted-places/sample.csv")
  df_base <- read_csv("scripts/2023/week-41-haunted-places/sample.csv")
}

# doing all the midjourney image creation
# this could be done with DALL-E e.g. create_image(df_base$image_prompt[1])
# but DALL-E is not as good as midjourney... nowhere near.

# map images - a hacky way
df_images <- tribble(
  ~id, ~img,
  2066, "006.png",
  7203, "007.png",
  4445, "008.png",
  9883, "009.png",
  5933, "010.png",
  9158, "011.png",
  4850, "012.png",
  8538, "013.png",
  5750, "014.png",
  1955, "015.png",
  4942, "016.png",
  2904, "017.png",
  8724, "018.png",
  4914, "019.png",
  5842, "020.png"
) |>
  mutate(img = paste0("scripts/2023/week-41-haunted-places/ghosts/", img))

# if using DALL-E don't merge on df_images
# a bit clunky
df_final <- df_base |>
  # left_join(df_images, by = "id") |>
  mutate(
    type = factor(type),
    x = as.numeric(type),
    desc_summary = str_remove(desc_summary, "The text describes |The text refers to "),
    desc_summary = str_to_sentence(desc_summary)
  ) |>
  group_by(type) |>
  mutate(
    y = 1:n()
  )

# circle crop
df_final <- df_final |>
  mutate(img_circle = circle_crop(img, border_size = 12, border_colour = accent))

df_base$img <- glue("scripts/2023/week-41-haunted-places/DALL-E/{df_base$id}.png")
for(k in 8:15) {
  prmpt <- df_base$image_prompt[k]
  prmpt <- str_remove_all(prmpt, "decapitation|decapitated|noose|headless")
  x <- create_image(prmpt)
  image_read(x$data$url) |>
    image_write(df_base$img[k])
}

# 🔡 text --------------------------------------------------------------------

caption <- make_caption(accent)

title <- "HAUNTED PLACES\nIN THE US"
subtitle <- "The United States has 10,992 documented haunted places all across
the country, patricularly in Schools, Cemeteries and Old Roads. Here are a selection
of haunted places from the top 3 most common types of haunted places."

# 📊 plot --------------------------------------------------------------------

x1 <- 0.16
x2 <- 0.2
y1 <- 0.45
y2 <- 0.25

g_base <- df_final |>
  ggplot() +
  geom_from_path(aes(x = x, y = y, path = img_circle), width = 0.085) +
  geom_text(aes(x = x + x1, y = y + y1, label = location), family = ft_title, size = 26, vjust = 1, hjust = 0, colour = accent) +
  geom_text(aes(x = x + x2, y = y + y2, label = str_wrap(desc_summary, 55)), family = ft, size = 10, vjust = 1, hjust = 0, colour = txt, lineheight = 0.3) +
  annotate("text", x = -0.2, y = 5.45, label = title, size = 72, colour = accent, family = ft_title, lineheight = 0.3, hjust = 0, vjust = 1) +
  annotate("text", x = -0.2, y = 4.55, label = str_wrap(subtitle, 55), size = 16, colour = accent, family = ft, lineheight = 0.3, hjust = 0, vjust = 1) +
  xlim(-0.2, 3.6) +
  ylim(0.8, 5+y1) +
  coord_cartesian(clip = "off") +
  labs(caption = caption) +
  theme_void() +
  theme(
    text = element_text(family = ft, size = 32, lineheight = 0.3, colour = txt),
    plot.background = element_rect(fill = bg, colour = bg),
    plot.title = element_text(size = 128, hjust = 0.5),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = txt, hjust = 0.5, margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 50, r = 50, l = 50)
  )

g_map <- df_state |>
  ggplot() +
  geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = n), colour = accent) +
  geom_text(aes(x = 0.5, y = 0.6, label = n), colour = txt, family = ft, size = 12, fontface = "bold") +
  geom_text(aes(x = 0.5, y = 0.35, label = str_wrap(code, 10)), colour = txt, family = ft, size = 4, lineheight = 0.3, vjust = 1) +
  facet_geo(~code) +
  scale_fill_gradientn(colours = colorRampPalette(c(bg, accent))(12)) +
  theme_void() +
  theme(
    legend.position = "none",
    strip.text = element_blank()
  )

g_type <- df_n_type |>
  mutate(type = fct_reorder(type, n)) |>
  ggplot() +
  geom_col(aes(x = type, y = n, fill = n), colour = accent, width = 0.8) +
  geom_text(aes(x = type, y = n + 25, label = n), family = ft, colour = accent, size = 12, hjust = 0) +
  scale_fill_gradientn(colours = colorRampPalette(c(bg, accent))(12)) +
  coord_flip(clip = "off") +
  theme_void() +
  theme(
    text = element_text(colour = accent, size = 32, lineheight = 0.3, family = ft),
    legend.position = "none",
    axis.text.y = element_text(hjust = 1, margin = margin(r = 2))
  )

g_base +
  inset_element(g_type, left = 0.03, right = 0.25, top = 0.3, bottom = 0) +
  inset_element(g_map, left = 0.045, right = 0.25, top = 0.65, bottom = 0.35) +
  plot_annotation(
    theme = theme(
      plot.background = element_rect(fill = bg, colour = bg)
    )
  )

ggsave("scripts/2023/week-41-haunted-places/haunted-places-dalle-og.png", height = 12, width = 24)
