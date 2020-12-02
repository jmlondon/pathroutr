## code to prepare `akcoast` dataset goes here
library(sf)
library(here)
akcoast_qry <- "https://arcgis.dnr.alaska.gov/arcgis/rest/services/OpenData/Physical_AlaskaCoast/MapServer/2/query?where=1%3D1&outFields=*&geometry=-159.240%2C55.112%2C-152.422%2C59.413&geometryType=esriGeometryEnvelope&inSR=4326&spatialRel=esriSpatialRelIntersects&outSR=3338&f=json"

akcoast <- sf::read_sf(akcoast_qry)
akcoast <- sf::st_make_valid(akcoast)

save(akcoast, file = here::here("data/akcoast.rda"), compress = "xz", precheck = FALSE)
