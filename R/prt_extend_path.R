#' Extend a path to include given start and end points
#'
#' @param p sfnetwork representing a single path
#' @param start_pt start point
#' @param end_pt end point
#'
#' @return linestring
#' @export
#'
prt_extend_path<- function(l_geom, start_pt, end_pt) {
  # start_pt <- sf::st_sfc(start_pt, crs = sf::st_crs(l_geom))
  # end_pt <- sf::st_sfc(end_pt, crs = sf::st_crs(l_geom))
  if(length(l_geom) == 0) {
    l <- c(start_pt,end_pt) %>% st_combine() %>% st_cast('LINESTRING')
    return(l)
  }
  l <- l_geom %>% st_combine() %>% st_line_merge()
  l_start <- lwgeom::st_startpoint(l)
  l_end <- lwgeom::st_endpoint(l)
  # check if line needs to be reversed
  start2start <- sf::st_distance(start_pt, l_start)
  start2end <- sf::st_distance(start_pt, l_end)
  if(start2start > start2end) {
    l <- sf::st_reverse(l)
  }
  l_start <- lwgeom::st_startpoint(l)
  l_end <- lwgeom::st_endpoint(l)
  l1 <- c(start_pt,l_start) %>% st_combine() %>% st_cast('LINESTRING')
  l2 <- c(end_pt, l_end) %>% st_combine() %>% st_cast('LINESTRING')

  l <- c(l1,l,l2) %>% st_combine() %>% st_line_merge()

  return(l)
}
