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
  suppressWarnings({
  vis_graph <- vis_graph %>%
    sfnetworks::st_network_blend(c(segs_tbl$start_pt,segs_tbl$end_pt))
  })

  edge_tbl <- segs_tbl %>%
    dplyr::rowwise() %>%
    dplyr::mutate(edge_paths = sfnetworks::st_network_paths(vis_graph,
                                             from = start_pt,
                                             to = end_pt) %>% pull(edge_paths),
                  geometry = sfnetworks::activate(vis_graph, "edges") %>%
                    slice(unlist(edge_paths)) %>%
                    prt_extend_path(start_pt, end_pt)
    )

  segs_tbl <- cbind(segs_tbl, edge_tbl %>% select(geometry))

  return(segs_tbl)
}
