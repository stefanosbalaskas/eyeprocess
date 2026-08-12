# Create an auditable adaptive-testing trace

Create an auditable adaptive-testing trace

## Usage

``` r
eyeprocess_irt_adaptive_trace(
  item_id,
  theta_before,
  theta_after,
  se_after,
  information,
  response = NA_real_
)
```

## Arguments

- item_id:

  Item identifier or vector of item identifiers.

- theta_before:

  Latent-trait estimate before item administration.

- theta_after:

  Latent-trait estimate after item administration.

- se_after:

  Conditional standard error after item administration.

- information:

  Item or test information value or vector.

- response:

  Observed item response or response variable.
