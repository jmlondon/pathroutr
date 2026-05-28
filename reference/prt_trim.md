# Trim tracks to start and end outside barrier

Trim tracks to start and end outside barrier

## Usage

``` r
prt_trim(trkpts, barrier)
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
