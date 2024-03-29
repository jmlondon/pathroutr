#' Re-route track points around barrier feature
#'
#' This is a convenience wrapper, and the suggested function, for re-routing a *trkpts*
#' series of ordered POINT features around a *barrier* polygon via *vis_graph* built with
#' the `prt_visgraph()` function. The output can be used as a starting point for a custom
#' process to replace the original geometry. Or, provide the output tibble directly to
#' `prt_update_points()` along with *trkpts* for simply updating in place.
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
#' @param trkpts Simple Feature points ('sf', 'sfc_POINT'/'sfc_MULTIPOINT') that represent
#' track points. Order is accepted as is and the bounding box of trkpts should be within
#' the bounding box of the barrier polygon.
#' @param barrier Simple Feature polygon ('sf', 'sfc_POLYGON'/'sfc_MULTIPOLYGON')
#' representing the barrier feature. Should be the same barrier as supplied to the
#' `prt_visgraph()` function.
#' @param vis_graph sfnetwork from prt_visgraph()
#' @param blend boolean whether to blend start/end points into network
#'
#' @return a two-column tibble with column *fid* representing the row index in trkpts
#' to be replaced by the new geometry in *geometry* column. If trkpts and barrier do not
#' spatially intersect and empty tibble is returned.
#' @export
#'
prt_reroute <- function(trkpts, barrier, vis_graph, blend=TRUE) {
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
    e <- tibble::tibble("fid" = integer(), "geometry" = sf::st_sfc(), .rows = 0)
    return(e)
  }

  segs_tbl <- get_barrier_segments(trkpts, barrier) %>%
    prt_shortpath(vis_graph,blend=blend)

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
