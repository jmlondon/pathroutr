library(sf)
library(tidygraph)
library(igraph)
library(dplyr)
library(tibble)
library(ggplot2)
library(units)
library(tmap)
library(osmdata)
library(rgrass7)
library(link2GI)
library(nabor)

muenster <- opq(bbox =  c(7.61, 51.954, 7.636, 51.968)) %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf() %>%
  osm_poly2line()

muenster_center <- muenster$osm_lines %>%
  select(highway)

ggplot(data = muenster_center) + geom_sf()

linkGRASS7(muenster_center,c("/Applications/GRASS-7.4.4.app/Contents/Resources",
                             "GRASS 7.4.4",
                             "NSIS"))
writeVECT(
  SDF = muenster_center,
  vname = 'muenster_center',
  v.in.ogr_flags = 'overwrite'
)

execGRASS(
  cmd = 'v.clean',
  input = 'muenster_center',
  output = 'muenster_cleaned',
  tool = 'break',
  flags = c('overwrite', 'c')
)

# Read back into R
use_sf()
muenster_center <- readVECT('muenster_cleaned') %>%
  rename(geometry = geom) %>%
  select(-cat)

edges <- muenster_center %>%
  mutate(edgeID = c(1:n()))
edges

nodes <- edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))
nodes

nodes <- nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>%
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)
nodes

source_nodes <- nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges = edges %>%
  mutate(from = source_nodes, to = target_nodes)

edges

nodes <- nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))

nodes

graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

graph

graph <- graph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))

graph

distances <- distances(
  graph = graph,
  weights = graph %>% activate(edges) %>% pull(length)
)

distances[1:5, 1:5]

from_node <- graph %>%
  activate(nodes) %>%
  filter(nodeID == 3044) %>%
  pull(nodeID)

to_node <- graph %>%
  activate(nodes) %>%
  filter(nodeID == 3282) %>%
  pull(nodeID)

path <- shortest_paths(
  graph = graph,
  from = from_node,
  to = to_node,
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)

path_graph <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()

path_graph

ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey', size = 0.5) +
  geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = path_graph %>% activate(nodes) %>% filter(nodeID %in% c(from_node, to_node)) %>% as_tibble() %>% st_as_sf(), size = 2)

muenster_station <- st_point(c(7.6349, 51.9566)) %>%
  st_sfc(crs = 4326)

muenster_cathedral <- st_point(c(7.626, 51.962)) %>%
  st_sfc(crs = 4326)

# Coordinates of the origin and destination node, as matrix
coords_o <- muenster_station %>%
  st_coordinates() %>%
  matrix(ncol = 2)

coords_d <- muenster_cathedral %>%
  st_coordinates() %>%
  matrix(ncol = 2)

# Coordinates of all nodes in the network
nodes <- graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf()

coords <- nodes %>%
  st_coordinates()

# Calculate nearest points on the network.
node_index_o <- knn(data = coords, query = coords_o, k = 1)
node_index_d <- knn(data = coords, query = coords_d, k = 1)
node_o <- nodes[node_index_o$nn.idx, ]
node_d <- nodes[node_index_d$nn.idx, ]

path <- shortest_paths(
  graph = graph,
  from = node_o$nodeID, # new origin
  to = node_d$nodeID,   # new destination
  output = 'both',
  weights = graph %>% activate(edges) %>% pull(length)
)

path_graph <- graph %>%
  subgraph.edges(eids = path$epath %>% unlist()) %>%
  as_tbl_graph()

ggplot() +
  geom_sf(data = graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey') +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), col = 'darkgrey', size = 0.5) +
  geom_sf(data = path_graph %>% activate(edges) %>% as_tibble() %>% st_as_sf(), lwd = 1, col = 'firebrick') +
  geom_sf(data = muenster_station, size = 2) +
  geom_sf(data = muenster_cathedral, size = 2)  +
  geom_sf_label(data = muenster_station, aes(label = 'station'), nudge_x = 0.004) +
  geom_sf_label(data = muenster_cathedral, aes(label = 'cathedral'), nudge_x = 0.005)





