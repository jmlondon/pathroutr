#' extend a line to include given start and end points
#'
#' @param l line
#' @param start_pt start point
#' @param end_pt end point
#'
#' @return line
#' @export
#'
prt_extend_line <- function(l, start_pt, end_pt) {
  p1 <- sf::st_line_sample(l, sample = 0) %>% sf::st_cast('POINT')
  p2 <- sf::st_line_sample(l, sample = 1) %>% sf::st_cast('POINT')
  l1 <- c(start_pt, p1) %>% sf::st_combine() %>% sf::st_cast('LINESTRING')
  l2 <- c(p2, end_pt) %>% sf::st_combine() %>% sf::st_cast('LINESTRING')
  l <- c(l1,l,l2) %>% sf::st_cast('POINT') %>%
    st_combine() %>%
    st_cast('LINESTRING')
}
