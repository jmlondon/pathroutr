# Extend a path to include given start and end points

This function extends the calculated shortest path to start and end with
the provided *start_pt* and *end_pt* geometries. This ensures that the
final sample of new points along the re-routed path include the portion
of the line from the *start_pt*/*end_pt* to the nearest network node.

## Usage

``` r
prt_extend_path(l_geom, start_pt, end_pt)
```

## Arguments

- l_geom:

  geometry passed from inside prt_shortpath()

- start_pt:

  start point

- end_pt:

  end point

## Value

linestring
