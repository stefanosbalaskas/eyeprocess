# Mean-mean IRT linking coefficients

Mean-mean IRT linking coefficients

## Usage

``` r
eyeprocess_irt_mean_mean_link(reference, focal, anchors = NULL)
```

## Arguments

- reference:

  Reference-form or reference-group item parameters.

- focal:

  Focal-form or focal-group item parameters.

- anchors:

  Anchor-item identifiers.

## Value

An object of class "eye_irt_link", stored as a named list, with
components "A", "B", "method", "anchors", "objective". It contains
mean-mean IRT linking coefficients and associated metadata or
diagnostics needed to interpret the result.
