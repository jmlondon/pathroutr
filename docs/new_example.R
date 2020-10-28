library(sf)
library(crawl)
library(ptolemy)
library(tidyverse)

library(sfnetworks)

data("harborSeal_sf")
harborSeal_sf <- harborSeal_sf %>%
  st_transform(3338)

ak <- ptolemy::alaska()

land_barrier <- ak %>%
  st_crop(harborSeal_sf %>% st_transform(3338)) %>%
  sf::st_collection_extract('POLYGON') %>%
  sf::st_cast('POLYGON') %>%
  sf::st_union()

library(ggspatial)

ggplot() +
  ggspatial::annotation_spatial(land_barrier) +
  ggspatial::layer_spatial(harborSeal_sf)

land_barrier_buffer <- land_barrier %>%
  sf::st_buffer(3000) %>%
  sf::st_union()

ggplot() +
  ggspatial::annotation_spatial(land_barrier) +
  ggspatial::layer_spatial(land_barrier_buffer,fill=NA) +
  ggspatial::layer_spatial(harborSeal_sf)

coastal_corridor <- land_barrier_buffer %>%
  sf::st_difference(land_barrier)

coastal_points <- coastal_corridor %>%
  sf::st_make_grid(2000, what = "centers") %>%
  sf::st_intersection(coastal_corridor)

xx <- tidyr::expand_grid(p1 = st_geometry(coastal_points),
                         p2=st_geometry(coastal_points))

is_equal <- function(p1,p2) {
  sf::st_equals(p1,p2) %>% lengths > 0
}

# library(furrr)
# plan(multisession, workers = 6)

xx2 <- xx2 %>%
  dplyr::mutate( redundant = purrr::map2_lgl(p1,p2,is_equal)) %>%
  dplyr::filter(!redundant) %>%
  dplyr::select(-redundant)

