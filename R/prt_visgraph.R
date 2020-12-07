#' Create a visibility graph
#'
#' @param barrier simple feature 'POLYGON' or 'MULTIPOLYGON' that can be cast into 'POLYGON'
#' @param buffer integer specifying buffer distance for barrier
#' @param centroids logical whether to include centroids in the mesh
#' @param centroid_limit integer minimum size (m^2) for adding centroid to triangles
#' @param aug_points simple feature 'POINT' or 'MULTIPOINT'; Not implemented yet
#'
#' @return SpatialLinesNetwork
#' @export
#'
prt_visgraph <- function(barrier,
                         buffer = 0,
                         centroids = FALSE,
                         centroid_limit = 10e+06,
                         aug_points = NULL) {

  stopifnot("barrier must be a simple feature collection with geometry type 'POLYGON' or 'MULTIPOLYGON" =
              inherits(barrier %>% st_geometry(), 'sfc_POLYGON') |
              inherits(barrier %>% st_geometry(), 'sfc_MULTIPOLYGON')
            )

  barrier <- sf::st_union(
    sf::st_cast(barrier, 'POLYGON')
  )

  if (buffer > 0) {
    buf_poly <- barrier %>% sf::st_buffer(buffer)
  } else {
    buf_poly <- barrier
  }

  if (!is.null(aug_points)) {
    stopifnot("aug_points must be a simple feature collection with geometry type 'POINT'" =
                inherits(aug_points %>% st_geometry(), 'sfc_POINT')
    )
    augment <- TRUE
  } else {
    augment <- FALSE
  }

  if (centroids & !augment) {
    centroid_limit <- units::set_units(centroid_limit, "m^2")
    init_dt <- sf::st_collection_extract(
      sf::st_triangulate(
        sf::st_cast(buf_poly, 'MULTIPOINT')
      ),
      'POLYGON')

    ctr_pts <- sf::st_centroid(
      init_dt[sf::st_area(init_dt) > centroid_limit]
    )

    edges <- c(
      buf_poly %>%
        sf::st_cast('MULTIPOINT') %>%
        sf::st_cast('POINT'),
      ctr_pts
    ) %>%
      sf::st_union() %>%
      sf::st_triangulate(bOnlyEdges = TRUE) %>%
      sf::st_cast('LINESTRING') %>%
      sf::st_sf()
    crosses <- do.call(c, st_intersects(st_buffer(barrier,-1), edges))

    edges <- edges[-crosses,]
  }

  if (centroids & augment) {
    centroid_limit <- units::set_units(centroid_limit, "m^2")
    init_dt <- sf::st_collection_extract(
      sf::st_triangulate(
        sf::st_cast(buf_poly, 'MULTIPOINT')
      ),
      'POLYGON')

    ctr_pts <- sf::st_centroid(
      init_dt[sf::st_area(init_dt) > centroid_limit]
    )

    edges <- c(
      buf_poly %>%
        sf::st_cast('MULTIPOINT') %>%
        sf::st_cast('POINT'),
      ctr_pts, aug_pts
    ) %>%
      sf::st_union() %>%
      sf::st_triangulate(bOnlyEdges = TRUE) %>%
      sf::st_cast('LINESTRING') %>%
      sf::st_sf()
    crosses <- do.call(c, st_intersects(st_buffer(barrier,-1), edges))

    edges <- edges[-crosses,]
  }
  if (!centroids & augment) {
    centroid_limit <- units::set_units(centroid_limit, "m^2")
    init_dt <- sf::st_collection_extract(
      sf::st_triangulate(
        sf::st_cast(buf_poly, 'MULTIPOINT')
      ),
      'POLYGON')

    edges <- c(
      buf_poly %>%
        sf::st_cast('MULTIPOINT') %>%
        sf::st_cast('POINT'),
      aug_pts
    ) %>%
      sf::st_union() %>%
      sf::st_triangulate(bOnlyEdges = TRUE) %>%
      sf::st_cast('LINESTRING') %>%
      sf::st_sf()
    crosses <- do.call(c, st_intersects(st_buffer(barrier,-1), edges))

    edges <- edges[-crosses,]
  }

  if (!centroids & !augment) {
    edges <- buf_poly %>%
      sf::st_cast('MULTIPOINT') %>%
      sf::st_union() %>%
      sf::st_triangulate(bOnlyEdges = TRUE) %>%
      sf::st_cast('LINESTRING') %>%
      sf::st_sf()

    crosses <- do.call(c, st_intersects(st_buffer(barrier,-1), edges))

    edges <- edges[-crosses,]
  }

  sln <- stplanr::SpatialLinesNetwork(edges)

  return(sln)
}
