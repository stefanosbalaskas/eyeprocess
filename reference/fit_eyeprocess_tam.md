# Fit a TAM model without substituting another estimator

Fit a TAM model without substituting another estimator

## Usage

``` r
fit_eyeprocess_tam(
  resp,
  model = c("rasch", "2pl", "gpcm"),
  ...,
  engine = "TAM"
)
```

## Arguments

- resp:

  Response matrix supplied to TAM.

- model:

  Model specification passed to the selected external engine.

- ...:

  Additional arguments passed to the selected method or external engine.

- engine:

  Requested estimation or analysis engine.
