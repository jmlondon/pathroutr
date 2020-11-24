#' Spatial predicates
#'
#' These are custom spatial predicate functions that are negated versions
#' of the spatial predicates `st_within()`, `st-crosses()`, and `st_intersects`
#'
#' @param x,y simple features.
#' @name spatial_predicates
NULL
#> NULL

#' @rdname spatial_predicates
not_crosses <- function(x,y) {
  !sf::st_crosses(x,y)
}

#' @rdname spatial_predicates
not_within <- function(x,y) {
  !sf::st_within(x,y)
}

#' @rdname spatial_predicates
not_intersects <- function(x,y) {
  !sf::st_intersects(x,y)
}
