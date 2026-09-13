# AOI membership probabilities from uncertainty draws

AOI membership probabilities from uncertainty draws

## Usage

``` r
aoi_membership_probability(draws, aois)
```

## Arguments

- draws:

  Output of \`propagate_calibration_uncertainty()\` or compatible table.

- aois:

  Rectangular AOI table with aoi/x_min/x_max/y_min/y_max.

## Value

A tabular R object containing aOI membership probabilities from
uncertainty draws; rows represent analysis units and columns contain the
returned quantities.
