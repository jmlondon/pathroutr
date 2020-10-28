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

ak <- ptolemy::alaska(resolution = "h")

land_barrier <- ak %>%
  st_crop(area_crop) %>%
  sf::st_collection_extract('POLYGON') %>%
  sf::st_cast('POLYGON') %>%
  sf::st_union()

library(ggspatial)

ggplot() +
  ggspatial::layer_spatial(land_barrier)

not_intersects  = function(x, y) !st_intersects(x, y)

tic()
delaunay_routes <- sf::st_cast(sf::st_buffer(land_barrier, 1),'MULTIPOINT') %>%
  st_as_sf() %>%
  sfdct::ct_triangulate(D=TRUE) %>%
  sf::st_collection_extract() %>%
  sf::st_cast("LINESTRING") %>%
  sf::st_filter(land_barrier, .predicate = not_intersects)
toc()

ggplot() +
  ggspatial::annotation_spatial(land_barrier) +
  ggspatial::layer_spatial(delaunay_routes)

library(sfnetworks)

n <- sfnetworks::as_sfnetwork(delaunay_routes)

n <- n %>%
  activate("edges") %>%
  mutate(
    length = edge_length())

library(mapview)
n %>% activate("edges") %>% st_as_sf() %>% mapview()

start <- st_point(c(-154.11346, 59.05751)) %>%
  st_sfc(crs = 4326) %>%
  st_transform(3338)
end <- st_point(c(-153.55042, 58.63836)) %>%
  st_sfc(crs = 4326) %>%
  st_transform(3338)

ggplot() +
  ggspatial::annotation_spatial(land_barrier) +
  ggspatial::layer_spatial(delaunay_routes) +
  ggspatial::layer_spatial(start) +
  ggspatial::layer_spatial(end)

st_shortest_paths(n, start, end, weights = "length")$vpath
