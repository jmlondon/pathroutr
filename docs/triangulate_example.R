library(sf)
library(crawl)
library(ptolemy)
library(tidyverse)
library(sfdct)
library(tictoc)

# data("harborSeal_sf")
# area_crop <- harborSeal_sf %>%
#   st_transform(3338)

## alternative crop coordinates
## UL: 59.5, -155
## UR: 59.5, -153
## LR: 58, -153
## LL: 58, -155
##
area_crop <- sf::st_polygon(list(matrix(
  c(-155, -153, -153, -155, -155, 59.5, 59.5, 58, 58, 59.5),
  ncol = 2
))) %>% st_sfc(crs = 4326) %>%
  st_transform(3338)

ak <- ptolemy::alaska(resolution = "i")

land_barrier <- sf::st_read('~/gis/pathroutr/simple_land_barrier.gpkg') %>%
  st_transform(3338)

land_barrier <- land_barrier %>%
  sf::st_cast('POLYGON') %>%
  sf::st_union()

library(ggspatial)

ggplot() +
  ggspatial::layer_spatial(land_barrier)

not_crosses  = function(x, y) !st_crosses(x, y)

tic()
delaunay_routes <- sf::st_cast(sf::st_buffer(land_barrier, 1), 'MULTIPOINT') %>%
  st_as_sf() %>%
  sf::st_triangulate() %>%
  sf::st_collection_extract('POLYGON') %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_filter(land_barrier, .predicate = not_crosses)
toc()

ggplot() +
  ggspatial::annotation_spatial(land_barrier) +
  ggspatial::layer_spatial(delaunay_routes)

library(sfnetworks)

n <- sfnetworks::as_sfnetwork(delaunay_routes)

n %>% to_spatial_subgraph(land_barrier,.predicate = not_intersects, subset_by = "edges")

n <- n %>%
  activate("edges") %>%
  mutate(
    length = edge_length())

poi <- sf::st_read('~/gis/pathroutr/points_of_interest.gpkg') %>%
  st_transform(3338) %>% sample_n(2)

start <- poi[1,]
end <- poi[2,]

ggplot() +
  ggspatial::annotation_spatial(land_barrier) +
  ggspatial::layer_spatial(delaunay_routes) +
  ggspatial::layer_spatial(start, color = "green", size = 3) +
  ggspatial::layer_spatial(end, color = "red", size = 3)

st_shortest_paths(n, start, end, weights = "length")$vpath
st_shortest_paths(n, start, end)

## try using anglr package

library(anglr)

barrier_mesh <- sf::st_cast(sf::st_buffer(land_barrier, 1),'MULTIPOINT') %>%
  st_as_sf() %>%
  anglr::DEL0() %>%
  anglr::as.mesh3d()

# now what?
