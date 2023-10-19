
#' Write readme text for the readme page
#'
#' @param week Week
#' @param year Year
#'
#' @return
#' @export
#'
#' @examples
readme_text <- function(week, year = 2023) {

  # make image small
  make_image_small(week)

  # make readme text
  dir <- list.files("scripts/2023", pattern = as.character(week), full.names = TRUE)
  lab <- str_extract(dir, "(?<=[:digit:]{1,2}-).+")
  title <- str_to_title(lab) |>
    str_replace_all("-", " ")

  # print tweet
  cat(
    glue("#TidyTuesday week {week}: {title}
    🔗 http://github.com/doehm/tidytues
    #Rstats #dataviz #r4ds #ggplot2\n\n\n")
  )

  # print readme text
  glue("## [Week {week}: {title}](https://github.com/doehm/tidytues/blob/main/scripts/{year}/week-{week}-{lab}/{lab}.R)

  <a href='https://github.com/doehm/tidytues/blob/main/scripts/{year}/week-{week}-{lab}/{lab}.png'>
    <img src='scripts/{year}/week-{week}-{lab}/{lab}-s.png'/></a>")
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
make_caption <- function(accent, data) {

  if(length(accent) != 4) {
    accent <- rep(accent[1], 4)
  }

  github <- glue("<span style='font-family:fa-brands; color:{accent[1]}'>&#xf09b;</span>")
  twitter <- glue("<span style='font-family:fa-brands; color:{accent[2]}'>&#xf099;</span>")
  threads <- glue("<span style='font-family:fa-brands; color:{accent[3]}'>&#xe618;</span>")
  mastodon <- glue("<span style='font-family:fa-brands; color:{accent[4]}'>&#xf4f6;</span>")
  floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
  space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
  space2 <- glue("<span style='color:{bg}'>-</span>") # can't believe I'm doing this
  glue("{github}{space2}doehm/tidytues{space2}{twitter}{space2}@danoehm{space2}{threads}{space2}@danoehm{space2}{mastodon}{space2}@danoehm@{space}fosstodon.org")
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
make_image_small <- function(week, year = 2023) {
  dir <- list.files("scripts/2023", pattern = as.character(week), full.names = TRUE)
  lab <- str_extract(dir, "(?<=[:digit:]{1,2}-).+")
  img <- paste0(dir, "/", lab, ".png")
  new_file <- paste0(dir, "/", lab, "-s.png")
  image_read(img) |>
    image_resize("x1080") |>
    image_write(new_file)
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
