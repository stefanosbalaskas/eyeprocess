# Leave item out validation

Leave item out validation

## Usage

``` r
leave_item_out_validation(data, item, fitter, predictor, scorer)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- item:

  Item identifier, name, or item column.

- fitter:

  Model-fitting function.

- predictor:

  Prediction function.

- scorer:

  Function that scores predictions.

## Value

An object of class "eye_leave_item_out_validation", "data.frame", stored
as a data frame, containing leave item out validation and associated
metadata needed to interpret the result.
