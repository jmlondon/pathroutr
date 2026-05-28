# Spatial predicates

These are custom spatial predicate functions that are negated versions
of the spatial predicates `st_within()`, `st-crosses()`, and
`st_intersects`

## Usage

``` r
not_crosses(x, y)

not_within(x, y)

not_intersects(x, y)
```

## Arguments

- x, y:

  simple features.
