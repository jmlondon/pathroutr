#' Calculate the shortest path through a visibility network between two points
#'
#' @param segs_tbl tbl from get_barrier_segments()
#' @param vis_graph SpatialLinesNetwork from prt_visgraph()
#'
#' @return simple feature 'LINESTRING'
#' @export
#'
prt_shortpath <- function(segs_tbl, vis_graph) {

  shortpath <- function(sln, start, end) {
    stplanr::sum_network_routes(sln, start, end)
  }

  safe_shortpath <- purrr::safely(shortpath)

  res_tbl <- vector(mode = "list", length = nrow(segs_tbl))
  for (i in 1:nrow(segs_tbl)) {
    res_tbl[[i]] <-
      safe_shortpath(vis_graph, segs_tbl$start_node[i], segs_tbl$end_node[i])

    if(segs_tbl$start_node[i] == segs_tbl$end_node[i]) {
      node_pt <- stplanr::sln2points(vis_graph)[i,]
      g <- rbind(node_pt, node_pt) %>% st_combine() %>% st_cast('LINESTRING')
      l_sf <- tibble(geometry = g, ID = NA, sum_length = NA, pathfound = FALSE) %>%
        st_sf(sf_column_name = "geometry")
      res_tbl[[i]]$result <- l_sf
    }

    }

  res_tbl <- res_tbl %>% purrr::map_dfr("result")

  segs_tbl <- cbind(segs_tbl, res_tbl) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(geometry = prt_extend_line(geometry, start_pt, end_pt)) %>%
    dplyr::select(sid:geometry)

  return(segs_tbl)
}
