# Find the nearest node for start and end points in segs_tbl

Find the nearest node for start and end points in segs_tbl

## Usage

``` r
prt_nearestnode(segs_tbl, vis_graph)
```

## Arguments

- segs_tbl:

  output from [`get_barrier_segments()`](get_barrier_segments.md)

- vis_graph:

  sfnetwork output from [`prt_visgraph()`](prt_visgraph.md)

## Value

segs_tbl tibble with updated columns for nearest start and end nodes
