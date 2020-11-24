#' Identify Track Points That Intersect With A Barrier
#'
#' @param trkpts simple feature points that represent track
#' @param barrier simple feature polygon for barrier
#'
#' @return data.frame of on-barrier points by segment
#' @export

get_barrier_segments = function(trkpts, barrier) {
  # TODO: add check for starting or ending track_pts on land
  trkpts <- sf::st_cast(trkpts, 'POINT')
  barrier_intersect <- sf::st_intersects(trkpts, barrier) %>%
    purrr::map_lgl(~ length(.x) > 0)

  in.segment <- (barrier_intersect == TRUE)

  start_idx <- which(c(FALSE, in.segment) == TRUE &
                       dplyr::lag(c(FALSE, in.segment) == FALSE)) - 2
  end_idx <- which(c(in.segment, FALSE) == TRUE &
                     dplyr::lead(c(in.segment, FALSE) == FALSE)) + 1
  barrier_segments <- data.frame(sid = 1:length(start_idx),
                                 start_idx, end_idx) %>%
    dplyr::mutate(n_pts = end_idx-start_idx-1) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(start_pt = trkpts[start_idx, ] %>% sf::st_geometry(),
                  end_pt = trkpts[end_idx, ] %>% sf::st_geometry()) %>%
    dplyr::ungroup()

  return(barrier_segments)
}
