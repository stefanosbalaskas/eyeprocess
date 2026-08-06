# Predict destination-state probabilities

S3 method supporting a research-scale eyeprocess object.

## Usage

``` r
# S3 method for class 'eye_multinomial_transition'
predict(object, newdata = NULL,
  type = c("probability", "class", "link"), ...)
```

## Arguments

- object:

  Multinomial transition model.

- newdata:

  Optional prepared transition data.

- type:

  Probability, class, or linear predictor.

- ...:

  Unused.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
