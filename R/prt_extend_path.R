#' Extend a path to include given start and end points
#'
#' This function extends the calculated shortest path to start and end with
#' the provided *start_pt* and *end_pt* geometries. This ensures that the
#' final sample of new points along the re-routed path include the portion
#' of the line from the *start_pt*/*end_pt* to the nearest network node.
#'
#' @param l_geom geometry passed from inside prt_shortpath()
#' @param start_pt start point
#' @param end_pt end point
#'
#' @return linestring
#' @export
#'
prt_extend_path<- function(l_geom, start_pt, end_pt) {

  if(nrow(l_geom) == 0) {
    l <- c(start_pt,end_pt) %>% sf::st_combine() %>% sf::st_cast('LINESTRING')
    return(l)
  }
  l <- sf::st_sf(l_geom) %>% dplyr::summarise(do_union=FALSE) %>%
    sf::st_line_merge() %>% sf::st_geometry()

  get_endpoints <- function(l) {
    l_coord <- sf::st_coordinates(l)
    l_crs <- sf::st_crs(l)

    list(
      start = l_coord[1, 1:2] |> sf::st_point() |> sf::st_sfc(crs = l_crs),
      end = l_coord[nrow(l_coord), 1:2] |>
        sf::st_point() |>
        sf::st_sfc(crs = l_crs)
    )
  }

  endpoints <- get_endpoints(l)

  # check if line needs to be reversed

  if(nabor::knn(sf::st_coordinates(c(start_pt,end_pt)),
      sf::st_coordinates(endpoints$start),
      k = 1)$nn.idx[,1] == 2) {
    l <- sf::st_reverse(l)
    endpoints <- get_endpoints(l)
  }

  l1 <- c(start_pt, endpoints$start) %>% sf::st_combine() %>% sf::st_cast('LINESTRING')
  l2 <- c(end_pt, endpoints$end) %>% sf::st_combine() %>% sf::st_cast('LINESTRING')

  l <- c(l1,l,l2) %>% sf::st_combine() %>% sf::st_line_merge()

  return(l)
}