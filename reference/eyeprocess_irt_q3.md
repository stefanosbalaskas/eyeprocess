# Compute Yen-style Q3 residual correlations

Compute Yen-style Q3 residual correlations

## Usage

``` r
eyeprocess_irt_q3(responses, probabilities, use = "pairwise.complete.obs")
```

## Arguments

- responses:

  Response matrix or response data.

- probabilities:

  Probability matrix or vector, with dimensions appropriate to the
  model.

- use:

  Missing-data handling mode passed to the residual correlation
  calculation.

## Value

An R object containing yen-style Q3 residual correlations. The concrete
class and structure follow the selected method, engine, or input object
and are preserved as documented by that workflow.
