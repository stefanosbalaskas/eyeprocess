# Equate IRT scales using anchor item parameters

Implements mean-sigma, mean-mean, Stocking-Lord, and Haebara linking for
dichotomous 2PL-style item parameters. New-form parameters are
transformed onto the reference scale using theta_ref = A \* theta_new +
B.

## Usage

``` r
equate_irt_scales(
  reference,
  new,
  method = c("stocking-lord", "haebara", "mean-sigma", "mean-mean"),
  theta_grid = seq(-4, 4, length.out = 81)
)
```

## Arguments

- reference:

  Reference-scale parameters or data.

- new:

  New-scale parameters or data.

- method:

  Method used for estimation, linking, or comparison.

- theta_grid:

  Grid of latent-trait values used for evaluation.
