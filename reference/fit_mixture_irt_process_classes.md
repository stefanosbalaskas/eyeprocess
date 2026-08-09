# Fit a true mirt mixture-IRT response model

This is distinct from the existing two-stage process-class reference
model: the response distribution itself is calibrated using mirt's
mixture density. Process features may then be compared with the fitted
response classes only when a defensible class-membership extraction is
available.

## Usage

``` r
fit_mixture_irt_process_classes(
  response_matrix,
  n_classes = 2L,
  model = 1,
  itemtype = "2PL",
  SE = FALSE
)
```

## Arguments

- response_matrix:

  Person x item response matrix.

- n_classes:

  Number of mixture classes. The currently verified internal route
  supports two classes (\`mixture-2\`).

- model:

  mirt model specification, default one dimension.

- itemtype:

  Item type.

- SE:

  Request standard errors.
