# Update track points with fixed geometry

Original geometry is updated in place and (currently) no record of those
points that were updated is provided.

## Usage

``` r
prt_update_points(rrt_pts, trkpts)
```

## Arguments

- rrt_pts:

  output from [`prt_reroute()`](prt_reroute.md) or tibble with *rrt_idx*
  and *geometry* columns

- trkpts:

  original trkpts Simple Features Collection

## Value

trkpts with updated geometry
