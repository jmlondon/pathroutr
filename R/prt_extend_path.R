#' Extend a path to include given start and end points
#'
#' @param l_geom geometry passed from inside prt_shortpath()
#' @param start_pt start point
#' @param end_pt end point
#'
#' @return linestring
#' @export
#'
prt_extend_path<- function(l_geom, start_pt, end_pt) {

  if(length(l_geom) == 0) {
    l <- c(start_pt,end_pt) %>% st_combine() %>% st_cast('LINESTRING')
    return(l)
  }
  l <- st_sf(l_geom) %>% summarise(do_union=FALSE) %>%
    st_line_merge() %>% st_geometry()
  l_start <- lwgeom::st_startpoint(l)
  l_end <- lwgeom::st_endpoint(l)

  # check if line needs to be reversed

  if(nabor::knn(sf::st_coordinates(c(start_pt,end_pt)),
                sf::st_coordinates(l_start),
                k = 1)$nn.idx[,1] == 2) {
    l <- sf::st_reverse(l)
    l_start <- lwgeom::st_startpoint(l)
    l_end <- lwgeom::st_endpoint(l)
  }

  l1 <- c(start_pt,l_start) %>% st_combine() %>% st_cast('LINESTRING')
  l2 <- c(end_pt, l_end) %>% st_combine() %>% st_cast('LINESTRING')

  l <- c(l1,l,l2) %>% st_combine() %>% st_line_merge()

  return(l)
}
