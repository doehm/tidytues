
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
