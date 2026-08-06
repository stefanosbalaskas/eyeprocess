# Construct a functional basis for pupil trajectories

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
functional_pupil_basis(x, df = 6L, basis = c("natural_spline", "bspline"),
  degree = 3L, boundary_knots = NULL, knots = NULL)
```

## Arguments

- x:

  Prepared functional pupil data or numeric time vector.

- df:

  Degrees of freedom.

- basis:

  Natural spline or B-spline.

- degree:

  B-spline polynomial degree; ignored for natural splines.

- boundary_knots:

  Optional boundary knots.

- knots:

  Optional internal knots.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
