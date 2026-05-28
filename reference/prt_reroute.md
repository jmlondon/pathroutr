# Re-route track points around barrier feature

This is a convenience wrapper, and the suggested function, for
re-routing a *trkpts* series of ordered POINT features around a
*barrier* polygon via *vis_graph* built with the
[`prt_visgraph()`](prt_visgraph.md) function. The output can be used as
a starting point for a custom process to replace the original geometry.
Or, provide the output tibble directly to
[`prt_update_points()`](prt_update_points.md) along with *trkpts* for
simply updating in place.

## Usage

``` r
prt_reroute(trkpts, barrier, vis_graph, blend = TRUE)
```

## Arguments

- trkpts:

  Simple Feature points ('sf', 'sfc_POINT'/'sfc_MULTIPOINT') that
  represent track points. Order is accepted as is and the bounding box
  of trkpts should be within the bounding box of the barrier polygon.

- barrier:

  Simple Feature polygon ('sf', 'sfc_POLYGON'/'sfc_MULTIPOLYGON')
  representing the barrier feature. Should be the same barrier as
  supplied to the [`prt_visgraph()`](prt_visgraph.md) function.

- vis_graph:

  sfnetwork from prt_visgraph()

- blend:

  boolean whether to blend start/end points into network

## Value

a two-column tibble with column *fid* representing the row index in
trkpts to be replaced by the new geometry in *geometry* column. If
trkpts and barrier do not spatially intersect and empty tibble is
returned.

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
