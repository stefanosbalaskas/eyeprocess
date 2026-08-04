# Fit a diffusion IRT adapter

Fit a diffusion IRT adapter

## Usage

``` r
fit_diffirt_adapter(x, model = c("D", "Q"), ...)
```

## Arguments

- x:

  An \`eye_dataset\`.

- model:

  Diffusion IRT model, \`"D"\` or \`"Q"\`.

- ...:

  Passed to \`diffIRT::diffIRT()\`.

## Value

An \`eyeprocess_model\`.
