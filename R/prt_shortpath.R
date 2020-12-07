#' Calculate the shortest path through a visibility network between two points
#'
#' @param segs_tbl tbl from `prt_nearestnode()`
#' @param vis_graph SpatialLinesNetwork from prt_visgraph()
#'
#' @return data frame with added geometry column for shortest path LINESTRING that
#' connects the *start_pt* and *end_pt* coordinates
#' @export
#'
prt_shortpath <- function(segs_tbl, vis_graph) {

  res_tbl <- stplanr::sum_network_routes(sln = vis_graph,
                                         start = segs_tbl$start_node,
                                         end = segs_tbl$end_node)

  segs_tbl <- cbind(segs_tbl, res_tbl) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(geometry = prt_extend_line(geometry, start_pt, end_pt)) %>%
    dplyr::select(sid:geometry)

  return(segs_tbl)
}
