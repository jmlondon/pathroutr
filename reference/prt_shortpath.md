# Calculate the shortest path through a visibility network between two points

Given a *segs_tbl* tibble created by
[`get_barrier_segments()`](get_barrier_segments.md) and a *vis_graph*
created by [`prt_visgraph()`](prt_visgraph.md), this function adds a
geometry column to *segs_tbl* that is the shortest path from the POINT
geometries provided in the *start_pt* / *end_pt* columns of *segs_tbl*.

## Usage

``` r
prt_shortpath(segs_tbl, vis_graph, blend = TRUE)
```

## Arguments

- segs_tbl:

  tibble from [`get_barrier_segments()`](get_barrier_segments.md)

- vis_graph:

  sfnetwork from prt_visgraph()

- blend:

  boolean whether to blend start/end points into network

## Value

segs_tbl data frame with added geometry column for shortest path
LINESTRING that connects the *start_pt* and *end_pt* coordinates

## Details

The `blend = TRUE` argument will blend all of the *start_pt* / *end_pt*
geometries in *segs_tbl* into the *vis_graph* network via the
[`sfnetworks::st_network_blend()`](https://luukvdmeer.github.io/sfnetworks/reference/st_network_blend.html)
function. This process creates new nodes within *vis_graph* that are
positioned at the perpendicular intersection between each point and the
nearest edge. With `blend = FALSE` the nearest existing node is used. In
highly complex coastlines, the use of `blend = FALSE` could result in
re-routed paths that still intersect land or do not accurately represent
the intended result. For less complex situations and when computational
speed is important, `blend = FALSE` may be appropriate.

This function is typically called directly by `prt_reroute` and users
are discouraged from using this function directly.
