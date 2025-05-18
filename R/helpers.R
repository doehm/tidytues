
#' Write readme text for the readme page
#'
#' @param week Week
#' @param year Year
#'
#' @return
#' @export
#'
#' @examples
make_readme_text <- function(week, title, year = 2025) {

  # make image small
  make_image_small(week)

  # make readme text
  week <- str_pad(week, width = 2, pad = 0)
  dir <- list.files(glue("scripts/{year}"), pattern = week, full.names = TRUE)

  r_file <- list.files(dir, pattern = ".R")
  png_file <- str_replace(r_file, ".R", ".png")
  pngs_file <- str_replace(r_file, ".R", "-s.png")

  # post template
  cat(glue("
    #TidyTuesday week {week}:\n



    #Rstats #dataviz #ggplot2
  \n"))

  cat(glue("
Code: 🔗 https://github.com/doehm/tidytues/blob/main/{dir}/{r_file}
Gallery: 🔗 https://github.com/doehm/tidytues
  "))

  # print readme text
  glue("## [Week {week}: {title}](https://github.com/doehm/tidytues/blob/main/{dir}/{r_file})

  <a href='https://github.com/doehm/tidytues/blob/main/{dir}/{png_file}'>
    <img src='{dir}/{pngs_file}'/></a>")
}


#' Tweet template
#'
#' @param name Name for this week
#' @param week Week
#'
#' @return
#' @export
#'
#' @examples
make_tweet <- function(name, week) {
  glue("#TidyTuesday week {week}: {name}
  🔗 http://github.com/doehm/tidytues
  #Rstats #dataviz #r4ds #ggplot2")
}

#' Min max
#'
#' @param x Numeric vector
#' @param a Min
#' @param b Max
#'
#' @return
#' @export
#'
#' @examples
min_max <- function(x, a, b) {
  (b - a) * (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) + a
}


#' Caption text
#'
#' @param accent Colour of icon
#' @param data Name of data
#'
#' @return
#' @export
make_caption <- function(accent, bg, data) {

  if(length(accent) != 3) {
    accent <- rep(accent[1], 3)
  }

  github <- glue("<span style='font-family:fa-brands; color:{accent[1]}'>&#xf09b;</span>")
  bluesky <- glue("<span style='font-family:fa-brands; color:{accent[2]}'>&#xe671;</span>")
  linkedin <- glue("<span style='font-family:fa-brands; color:{accent[3]}'>&#xf08c;</span>")

  twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
  threads <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xe618;</span>")
  mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
  floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")

  space <- glue("<span style='color:{bg};font-size:1px'>''</span>")
  space2 <- glue("<span style='color:{bg}'>-</span>") # can't believe I'm doing this

  glue("
       {github} {space} doehm/tidytues {space2}
       {bluesky} {space} @danoehm.bsky.social {space2}
       {linkedin} {space} Dan Oehm
       ")
}

#' To percent
#'
#' @param x Numberic value
#'
#' @return
#' @export
to_pct <- function(x, digit) {
  paste0(round(x, digit)*100, "%")
}


#' Make image small
#'
#' Makew the image small for the readme page
#'
#' @param week Week
#' @param year Year
#'
#' @return
#' @export
make_image_small <- function(week, year = 2025) {
  week <- str_pad(week, width = 2, pad = "0")
  dir <- list.files("scripts/2025/", pattern = as.character(week), full.names = TRUE)
  files <- list.files(dir, pattern = ".png", full.names = TRUE)
  files <- files[!str_detect(files, "-s.png")]
  new_file <- str_replace(files, ".png", "-s.png")
  walk2(files, new_file, ~{
    image_read(.x) |>
      image_resize("x1080") |>
      image_write(.y)
  })
}


#' Helper for richtext
#'
#' @param text Text
#' @param col Colour
#'
#' @return
#' @export
ct <- function(text, col) {
  glue("<span style='color:{col};'>{text}</span>")
}


#' Breathing space
#'
#' To help tweakteyt positioning so there is no teyt overlap on
#' the y ayis
#'
#' @param y Numeric vector for plotting on the y
#' @param diff Minimum distance between points
#' @param eps Tolerance. To ensure the algorithm stops when it's close enough
#'
#' @return Numeric vector
#' @export
#'
#' @importFrom purrr map_dbl
#' @importFrom lubridate ymd wday
#' @importFrom glue glue
#' @importFrom stringr str_pad
#' @import dplyr
#'
#' @examples
breathing_space_on_y <- function(y, diff, eps = 0.01) {
  y0 <- sort(y, index.return = TRUE)
  y_sorted <- y0$x
  y_index <- map_dbl(1:length(y), ~which(y0$ix == .x))
  d_lag <- y_sorted - lag(y_sorted)
  d_lead <- lead(y_sorted) - y_sorted
  K <- 2:length(y)

  while(min(d_lag, na.rm = TRUE) < diff*(1-eps)) {
    for(k in K) {
      if(d_lag[k] < diff) {
        y_sorted[k-1] <- y_sorted[k-1]-(diff-d_lag[k])/2
        y_sorted[k] <- y_sorted[k]+(diff-d_lag[k])/2
      }
    }
    d_lag <- y_sorted - lag(y_sorted)
  }
  y_sorted[y_index]
}



#' Breathing space on `x`
#'
#' If labels are too close and overlap, breathing space will stagger them to
#' avoid overlap
#'
#' @param x Input vector
#' @param d Minimum distance where two labels can be the same height
#' @param y0 Initial `y` position
#' @param dy Distance to stagger i.e. move up or down
#'
#' @return
#' @export
#'
#' @examples
breathing_space_on_x <- function(x, d, y0, dy) {
  n <- length(x)
  dd <- c(0, diff(x))
  y_spaced <- rep(0, n)
  for(k in 1:n) {
    if(y_spaced[k] == 0) {
      dk <- cumsum(dd[(k+1):(k+min(n, k+5))])
      id <- which(dk < d)
      if(length(id > 0)) {
        y_spaced[(k+1):(k+length(id))] <- id
      }
    }
  }
  y_new <- y0 - seq(0, 10*dy, dy)
  y_new[y_spaced+1]
}


#' Hours, minutes and seconds
#'
#' Converts a number vector of seconds into hh:mm:ss format
#'
#' @param seconds Numeric vector of seconds
#'
#' @return Character vector
#' @export
#'
#' @examples to_hms(12345)
to_hm <- function(seconds, fmt = "hm") {
  hrs <- floor(seconds/3600)
  mins <- floor(seconds/60) - hrs*60
  secs <- seconds - mins*60 - hrs*3600
  mins <- str_pad(mins, width = 2, pad = 0)
  secs <- str_pad(secs, width = 2, pad = 0)
  ifelse(hrs == 0, paste0(mins, "m"), paste0(hrs, "h ", mins, "m"))
}

#' Title
#'
#' @param ss GS4 id
#' @param sheet Sheet name
#'
#' @return
#' @export
read_shapes <- function(ss, sheet) {
  read_sheet(ss = ss, sheet = sheet) |>
    mutate(y = 1:n()) |>
    pivot_longer(starts_with("x"), names_to = "x", values_to = "pos") |>
    filter(pos > 0) |>
    mutate(x = as.numeric(str_remove(x, "x"))) |>
    arrange(pos) |>
    mutate(
      x = min_max(x, 0, 1),
      y = min_max(-y, 0, 1)
    )
}
