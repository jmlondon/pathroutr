# Identify track points that intersect with a barrier polygon

This function identifies the segments of consecutive points that
intersect with the barrier polygon feature. The result is a tibble of
segment records that identify portions of the track that will need to be
re-routed.

## Usage

``` r
get_barrier_segments(trkpts, barrier)
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

## Value

tibble representing segments of consecutive points that intersect with
the barrier feature. the *start_pt* and *end_pt* geometry columns
represent the bookend points for each segment that do not intersect with
the barrier feature. The *n_pts* column is the number of points to be
re-routed.
