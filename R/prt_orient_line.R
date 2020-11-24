#' Correctly orient points from line based on a given nearest start point
#'
#' @param x start point
#' @param l_pts points from line
#'
#' @return l_pts
#' @export
#'
prt_orient_line <- function(x,l_pts) {
  l_pts <- l_pts %>% sf::st_cast('POINT')
  p1 <- l_pts[1]
  p2 <- l_pts[length(l_pts)]

  p1_dist <- sf::st_distance(x, p1)
  p2_dist <- sf::st_distance(x, p2)

  if (which.min(c(p1_dist, p2_dist)) != 1) {
    l_pts <- rev(l_pts)
    message('point order reversed')
    return(l_pts)
  } else {
    return(l_pts)
  }

}
