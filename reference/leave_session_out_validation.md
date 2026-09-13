# Leave session out validation

Leave session out validation

## Usage

``` r
leave_session_out_validation(data, session, fitter, predictor, scorer)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- session:

  Session identifier or session facet.

- fitter:

  Model-fitting function.

- predictor:

  Prediction function.

- scorer:

  Function that scores predictions.

## Value

An object of class "eye_leave_session_out_validation", "data.frame",
stored as a data frame, containing leave session out validation and
associated metadata needed to interpret the result.
