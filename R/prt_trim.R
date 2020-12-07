#' Trim tracks to start and end outside barrier
#'
#' @param trkpts sf POINT
#' @param barrier sf POLYGON
#'
#' @return
#' @export
#'
prt_trim <- function(trkpts, barrier) {
  stopifnot("trkpts must be a simple feature collection with geometry type 'POINT' or 'MULTIPOINT" =
              inherits(trkpts %>% st_geometry(), 'sfc_POINT') |
              inherits(trkpts %>% st_geometry(), 'sfc_MULTIPOINT')
  )
  trkpts <- sf::st_cast(trkpts, 'POINT')
  barrier_intersect <- sf::st_intersects(trkpts, barrier) %>%
    purrr::map_lgl(~ length(.x) > 0)

  head_start <- 1
  tail_end <- length(barrier_intersect)

  if (min(which(barrier_intersect == TRUE)) == 1) {
    warning(paste0("Path starts within vector mask, first ",
                   min(which(barrier_intersect == 0)) - 1,
                   " observations removed"))
    head_start = min(which(barrier_intersect == FALSE))
  }
  if (max(which(barrier_intersect == 0)) < length(barrier_intersect)) {
    warning(paste("Path ends within vector mask, last ",
                  length(barrier_intersect) - max(which(barrier_intersect == 0)),
                  " observations removed"))
    tail_end <- max(which(barrier_intersect == 0))
  }

  trkpts <- trkpts[head_start:tail_end,]

  return(trkpts)
}
