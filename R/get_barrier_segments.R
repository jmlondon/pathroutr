#' Identify track points that intersect with a barrier polygon
#'
#' This function identifies the segments of consecutive points that intersect with the
#' barrier polygon feature. The result is a data frame of segment records that identify
#' portions of the track that will need to be re-routed. The result from this function
#' can be directly passed into the `prt_nearestnode()`.
#'
#' @param trkpts Simple Feature points ('sf', 'sfc_POINT'/'sfc_MULTIPOINT') that represent
#' track points. Order is accepted as is and the bounding box of trkpts should be within
#' the bounding box of the barrier polygon.
#' @param barrier Simple Feature polygon ('sf', 'sfc_POLYGON'/'sfc_MULTIPOLYGON')
#' representing the barrier feature. Should be the same barrier as supplied to the
#' `prt_visgraph()` function.
#'
#' @return data.frame representing segments of consecutive points that intersect with
#' barrier feature. the *start_pt* and *end_pt* geometry columns represent the bookend
#' points for each segment that do not intersect with the barrier feature. The *n_pts*
#' column is the number of points to be re-routed.
#' @export

get_barrier_segments = function(trkpts, barrier) {
  stopifnot("barrier must be a simple feature collection with geometry type 'POLYGON' or 'MULTIPOLYGON" =
              inherits(barrier %>% st_geometry(), 'sfc_POLYGON') |
              inherits(barrier %>% st_geometry(), 'sfc_MULTIPOLYGON')
  )
  stopifnot("trkpts must be a simple feature collection with geometry type 'POINT' or 'MULTIPOINT" =
              inherits(trkpts %>% st_geometry(), 'sfc_POINT') |
              inherits(trkpts %>% st_geometry(), 'sfc_MULTIPOINT')
  )

  trkpts <- sf::st_cast(trkpts, 'POINT')
  barrier_intersect <- sf::st_intersects(trkpts, barrier) %>%
    purrr::map_lgl(~ length(.x) > 0)

  stopifnot("first point in trkpts cannot intersect with barrier; suggest prt_trim()" =
              min(which(barrier_intersect == TRUE)) > 1)
  stopifnot("last point in trkpts cannot intersect with barrier; suggest prt_trim()" =
              max(which(barrier_intersect == 0)) == length(barrier_intersect)
  )

  in.segment <- (barrier_intersect == TRUE)

  if (sum(in.segment) == 0) {
    barrier_segments <- tibble::tibble(
      'sid' = integer(),
      'start_idx' = numeric(),
      'end_idx' = numeric(),
      'n_pts' = numeric(),
      'start_pt' = sf::st_sfc(),
      'end_pt' = sf::st_sfc()
    )
    return(barrier_segments)
  }

  start_idx <- which(c(FALSE, in.segment) == TRUE &
                       dplyr::lag(c(FALSE, in.segment) == FALSE)) - 2
  end_idx <- which(c(in.segment, FALSE) == TRUE &
                     dplyr::lead(c(in.segment, FALSE) == FALSE)) + 1


  barrier_segments <- tibble::tibble(sid = 1:length(start_idx),
                                 start_idx, end_idx) %>%
    dplyr::mutate(n_pts = end_idx-start_idx-1) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(start_pt = trkpts[start_idx, ] %>% sf::st_geometry(),
                  end_pt = trkpts[end_idx, ] %>% sf::st_geometry()) %>%
    dplyr::ungroup()

  return(barrier_segments)
}
