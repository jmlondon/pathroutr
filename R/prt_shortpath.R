#' Calculate the shortest path through a visibility network between two points
#'
#' Given a *segs_tbl* tibble created by `get_barrier_segments()` and a
#' *vis_graph* created by `prt_visgraph()`, this function adds a geometry
#' column to *segs_tbl* that is the shortest path from the POINT geometries
#' provided in the *start_pt* / *end_pt* columns of *segs_tbl*.
#'
#' The `blend = TRUE` argument will blend all of the *start_pt* / *end_pt*
#' geometries in *segs_tbl* into the *vis_graph* network via the
#' `sfnetworks::st_network_blend()` function. This process creates new nodes
#' within *vis_graph* that are positioned at the perpendicular intersection
#' between each point and the nearest edge. With `blend = FALSE` the nearest
#' existing node is used. In highly complex coastlines, the use of `blend = FALSE`
#' could result in re-routed paths that still intersect land or do not
#' accurately represent the intended result. For less complex situations and
#' when computational speed is important, `blend = FALSE` may be appropriate.
#'
#' This function is typically called directly by `prt_reroute` and users are
#' discouraged from using this function directly.
#'
#' @param segs_tbl tibble from `get_barrier_segments()`
#' @param vis_graph sfnetwork from prt_visgraph()
#' @param blend boolean whether to blend start/end points into network
#'
#' @return segs_tbl data frame with added geometry column for shortest path LINESTRING that
#' connects the *start_pt* and *end_pt* coordinates
#' @export
#'
prt_shortpath <- function(segs_tbl, vis_graph, blend = TRUE) {
  if(blend) {
  suppressWarnings({
  vis_graph <- vis_graph %>%
    sfnetworks::st_network_blend(c(segs_tbl$start_pt,segs_tbl$end_pt))
  })
  }
  vis_graph <- sfnetworks::activate(vis_graph,"edges")

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
