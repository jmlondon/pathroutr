#' Update track points with fixed geometry
#'
#' Original geometry is updated in place and (currently) no record of those points
#' that were updated is provided.
#'
#' @param rrt_pts output from `prt_reroute()` or tibble with *rrt_idx* and *geometry* columns
#' @param trkpts original trkpts Simple Features Collection
#'
#' @return trkpts with updated geometry
#' @export
#'
prt_update_points <- function(rrt_pts, trkpts) {
  trkpts <- trkpts %>% sf::st_cast('POINT') %>%
    sf::st_sf() %>% tibble::rowid_to_column("fid")

  res <- trkpts %>% tibble::as_tibble()  %>%
    dplyr::rows_update(rrt_pts, by = "fid") %>%
    sf::st_sf(sf_column_name = "geometry")

  return(res)
}
