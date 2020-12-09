#' Re-route track points around barrier feature
#'
#' This is a convenience wrapper, and the suggested function, for re-routing a *trkpts*
#' series of ordered POINT features around a *barrier* polygon via *vis_graph* built with
#' the `prt_visgraph()` function. The output can be used as a starting point for a custom
#' process to replace the original geometry. Or, provide the output tibble directly to
#' `prt_update_points()` along with *trkpts* for simply updating in place.
#'
#' @param trkpts Simple Feature points ('sf', 'sfc_POINT'/'sfc_MULTIPOINT') that represent
#' track points. Order is accepted as is and the bounding box of trkpts should be within
#' the bounding box of the barrier polygon.
#' @param barrier Simple Feature polygon ('sf', 'sfc_POLYGON'/'sfc_MULTIPOLYGON')
#' representing the barrier feature. Should be the same barrier as supplied to the
#' `prt_visgraph()` function.
#' @param vis_graph SpatialLinesNetwork from prt_visgraph()
#'
#' @return a two-column tibble with column *fid* representing the row index in trkpts
#' to be replaced by the new geometry in *geometry* column.
#' @export
#'
prt_reroute <- function(trkpts, barrier, vis_graph) {
  stopifnot(
    "barrier must be a simple feature collection with geometry type 'POLYGON' or 'MULTIPOLYGON" =
      inherits(barrier %>% st_geometry(), 'sfc_POLYGON') |
      inherits(barrier %>% st_geometry(), 'sfc_MULTIPOLYGON')
  )
  stopifnot(
    "trkpts must be a simple feature collection with geometry type 'POINT' or 'MULTIPOINT" =
      inherits(trkpts %>% st_geometry(), 'sfc_POINT') |
      inherits(trkpts %>% st_geometry(), 'sfc_MULTIPOINT')
  )
  stopifnot(
    "trkpts and barrier must have the same CRS" =
      sf::st_crs(trkpts) == sf::st_crs(barrier)
  )

  trkpts <- sf::st_cast(trkpts, 'POINT')
  test_intersect <- sf::st_intersects(trkpts, barrier) %>%
    purrr::map_lgl( ~ length(.x) > 0) %>% sum()

  if (test_intersect == 0) {
    message("trkpts and barrier do NOT intersect; returning empty tibble")
    e <- tibble::tibble("rrt_idx" = integer(), "geometry" = sf::st_sfc(), .rows = 1)
    return(e)
  }

  segs_tbl <- get_barrier_segments(trkpts, barrier) %>%
    prt_nearestnode(vis_graph) %>%
    prt_shortpath(vis_graph)

  pts_list <- vector(mode = "list", length = nrow(segs_tbl))

  for (i in 1:nrow(segs_tbl)) {
    idx <- seq(segs_tbl$start_idx[i] + 1,segs_tbl$end_idx[i] - 1)
    pts <- sf::st_line_sample(segs_tbl$geometry[i],
                                  n = segs_tbl$n_pts[i],
                                  type = "regular") %>%
      sf::st_cast('POINT')
    pts_list[[i]] <- sf::st_sf(fid = idx, geometry = pts )
  }

  res <- do.call(rbind, pts_list) %>%
    tibble::as_tibble()

  return(res)
}
