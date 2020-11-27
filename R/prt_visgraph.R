#' Create a visibility graph
#'
#' @param barrier simple feature 'POLYGON' or 'MULTIPOLYGON' that can be cast into 'POLYGON'
#' @param buffer integer specifying buffer distance for barrier
#' @param centroids logical whether to include centroids in the mesh
#' @param centroid_limit integer minimum size (m^2) for adding centroid to triangles
#' @param aug_points simple feature 'POINT' or 'MULTIPOINT'
#'
#' @return SpatialLinesNetwork
#' @export
#'
prt_visgraph <- function(barrier,
                         buffer = 0,
                         centroids = TRUE,
                         centroid_limit = 10e+06,
                         aug_points = NULL) {
  barrier <- sf::st_union(
    sf::st_cast(barrier, 'POLYGON')
  )

  if (buffer > 0) {
    buf_poly <- barrier %>% sf::st_buffer(buffer)
  } else {
    buf_poly <- barrier
  }

  if (centroids) {
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
      sf::st_sf() %>%
      sf::st_filter(sf::st_buffer(barrier,-1), .predicate = not_intersects)
  } else {
    edges <- buf_poly %>%
      sf::st_cast('MULTIPOINT') %>%
      sf::st_union() %>%
      sf::st_triangulate(bOnlyEdges = TRUE) %>%
      sf::st_cast('LINESTRING') %>%
      sf::st_sf() %>%
      sf::st_filter(sf::st_buffer(barrier,-1), .predicate = not_intersects)
  }

  sln <- stplanr::SpatialLinesNetwork(edges)

  return(sln)
}
