# Uncertainty ellipse implied by an empirical calibration-error model

Uncertainty ellipse implied by an empirical calibration-error model

## Usage

``` r
gaze_uncertainty_ellipse(model, level = 0.95, center = NULL)
```

## Arguments

- model:

  Calibration error model.

- level:

  Probability level.

- center:

  Optional center; defaults to model mean error.

## Value

A data frame containing uncertainty ellipse implied by an empirical
calibration-error model. Rows represent the analysis units and columns
contain the identifiers, estimates, or diagnostics defined by the
function.
