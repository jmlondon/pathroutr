#' Calculate the shortest path through a visibility network between two points
#'
#' @param segs_tbl tbl from `get_barrier_segments()`
#' @param vis_graph sfnetwork from prt_visgraph()
#'
#' @return segs_tbl data frame with added geometry column for shortest path LINESTRING that
#' connects the *start_pt* and *end_pt* coordinates
#' @export
#'
prt_shortpath <- function(segs_tbl, vis_graph) {
  suppressWarnings({
  vis_graph <- vis_graph %>%
    sfnetworks::st_network_blend(c(segs_tbl$start_pt,segs_tbl$end_pt)) %>%
    sfnetworks::activate("edges")
  })

  segs_tbl <- prt_nearestnode(segs_tbl,vis_graph)

  route_edges <- lapply(1:length(segs_tbl$start_node), function(i) {
    unlist(igraph::shortest_paths(graph = vis_graph,
                                  from = segs_tbl$start_node[i],
                                  to = segs_tbl$end_node[i],
                                  weights = NULL,
                                  output = "epath")$epath)
  })

  edge_geom <- lapply(route_edges, function(e) {
    g <- vis_graph %>% as_tibble(spatial = FALSE)
    g[e,"geometry"]
  })

  path_geom <- sapply(1:length(edge_geom), function(i) {
    prt_extend_path(edge_geom[[i]], segs_tbl$start_pt[i], segs_tbl$end_pt[i])
  })
  segs_tbl <- segs_tbl %>%
    mutate(geometry = st_sfc(path_geom, crs = sf::st_crs(vis_graph)))

  return(segs_tbl)

}
