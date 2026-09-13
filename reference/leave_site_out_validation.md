# Leave site out validation

Leave site out validation

## Usage

``` r
leave_site_out_validation(data, site, fitter, predictor, scorer)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- site:

  Site identifier or site facet.

- fitter:

  Model-fitting function.

- predictor:

  Prediction function.

- scorer:

  Function that scores predictions.

## Value

An object of class "eye_leave_site_out_validation", "data.frame", stored
as a data frame, containing leave site out validation and associated
metadata needed to interpret the result.
