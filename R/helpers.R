
#' Title
#'
#' @param week Week
#' @param name Name
#' @param year Year
#'
#' @return
#' @export
#'
#' @examples
readme_text <- function(week, name, year = 2023) {
  glue("## [Week {week}: {str_to_title(name)}](https://github.com/doehm/tidytues/blob/main/scripts/{year}/week-{week}-{name}/{name}.R)

  <a href='https://github.com/doehm/tidytues/blob/main/scripts/{year}/week-{week}-{name}/{name}.R'>
    <img src='scripts/{year}/week-{week}-{name}/{name}.png'/></a>")
}


#' Title
#'
#' @param name
#' @param week
#'
#' @return
#' @export
#'
#' @examples
make_tweet <- function(name, week) {
  glue("#TidyTuesday week {week}: {str_to_title(name)}
  🔗 http://github.com/doehm/tidytues
  #Rstats #dataviz #r4ds #ggplot2")
}

#' Title
#'
#' @param x
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
min_max <- function(x, a, b) {
  (b - a) * (x - min(x)) / (max(x) - min(x)) + a
}


#' Title
#'
#' @param accent
#' @param data
#'
#' @return
#' @export
make_caption <- function(accent, data) {
  mastodon <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf4f6;</span>")
  twitter <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf099;</span>")
  github <- glue("<span style='font-family:fa-brands; color:{accent}'>&#xf09b;</span>")
  floppy <- glue("<span style='font-family:fa-solid; color:{accent}'>&#xf0c7;</span>")
  space <- glue("<span style='color:{bg};font-size:1px'>'</span>")
  space2 <- glue("<span style='color:{bg}'>-</span>") # can't believe I'm doing this
  glue("{mastodon}{space2}@danoehm@{space}fosstodon.org{space2}{twitter}{space2}@danoehm{space2}{github}{space2}doehm/tidytues{space2}{floppy}{space2}{data}")
}
