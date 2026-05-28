# Create a visibility graph

Create a visibility graph

## Usage

``` r
prt_visgraph(
  barrier,
  buffer = 0,
  centroids = FALSE,
  centroid_limit = 1e+07,
  aug_points = NULL
)
```

## Arguments

- barrier:

  simple feature 'POLYGON' or 'MULTIPOLYGON' that can be cast into
  'POLYGON'

- buffer:

  integer specifying buffer distance for barrier

- centroids:

  logical whether to include centroids in the mesh

- centroid_limit:

  integer minimum size (m^2) for adding centroid to triangles

- aug_points:

  simple feature 'POINT' or 'MULTIPOINT' as additional nodes

## Value

sfnetwork
