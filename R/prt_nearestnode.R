
#' Find the nearest node for start and end points in segs_tbl
#'
#' @param segs_tbl output from `get_barrier_segments()`
#' @param vis_graph SpatialLinesNetwork output from `prt_visgraph()`
#'
#' @return data frame with updated columns for nearest start and end nodes
#' @export

prt_nearestnode <- function(segs_tbl, vis_graph) {
  segs_tbl <- segs_tbl %>% ungroup() %>%
    dplyr::mutate(
      start_node = nabor::knn(st_coordinates(stplanr::sln2points(vis_graph)),
                              sf::st_coordinates(.$start_pt),
                              k = 1)$nn.idx[,1],
      end_node = nabor::knn(st_coordinates(stplanr::sln2points(vis_graph)),
                              sf::st_coordinates(.$end_pt),
                              k = 1)$nn.idx[,1],
    )
  return(segs_tbl)
}
