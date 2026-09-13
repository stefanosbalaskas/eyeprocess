# Leave device out validation

Leave device out validation

## Usage

``` r
leave_device_out_validation(data, device, fitter, predictor, scorer)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- device:

  Device identifier or device facet.

- fitter:

  Model-fitting function.

- predictor:

  Prediction function.

- scorer:

  Function that scores predictions.

## Value

An object of class "eye_leave_device_out_validation", "data.frame",
stored as a data frame, containing leave device out validation and
associated metadata needed to interpret the result.
