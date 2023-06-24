
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
