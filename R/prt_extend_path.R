#' Extend a path to include given start and end points
#'
#' @param p sfnetwork representing a single path
#' @param start_pt start point
#' @param end_pt end point
#'
#' @return linestring
#' @export
#'
prt_extend_path<- function(p, start_pt, end_pt) {
  if(length(st_geometry(p)) == 0) {
    l <- c(start_pt,end_pt) %>% st_combine() %>% st_cast('LINESTRING')
    return(l)
  }
  p_pts <- p %>% st_geometry() %>% st_cast('POINT')
  p_start <- p_pts[st_nearest_feature(start_pt,p_pts)]
  p_end <- p_pts[st_nearest_feature(end_pt,p_pts)]

  l <- st_geometry(p) %>% st_combine() %>% st_line_merge()
  l1 <- c(start_pt,p_start) %>% st_combine() %>% st_cast('LINESTRING')
  l2 <- c(end_pt,p_end) %>% st_combine() %>% st_cast('LINESTRING')

  l <- c(l1,l,l2) %>% st_combine() %>% st_line_merge()

  # check if line needs to be reversed
  start2start <- sf::st_distance(start_pt, lwgeom::st_startpoint(l))
  start2end <- sf::st_distance(start_pt, lwgeom::st_endpoint(l))
  if(start2start > start2end) {
    l <- sf::st_reverse(l)
  }

  return(l)
}
