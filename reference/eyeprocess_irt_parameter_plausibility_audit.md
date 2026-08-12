# Audit basic plausibility of dichotomous item parameters

Audit basic plausibility of dichotomous item parameters

## Usage

``` r
eyeprocess_irt_parameter_plausibility_audit(
  items,
  discrimination = c(0.2, 4),
  difficulty = c(-6, 6),
  lower_asymptote = c(0, 0.5),
  upper_asymptote = c(0.5, 1)
)
```

## Arguments

- items:

  Item-parameter data frame or item collection.

- discrimination:

  Discrimination vector or matrix.

- difficulty:

  Item difficulty or location parameter.

- lower_asymptote:

  Lower-asymptote parameter values.

- upper_asymptote:

  Upper-asymptote parameter values.
