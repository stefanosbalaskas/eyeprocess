# Fit a G-DINA cognitive-diagnosis model without fallback substitution

Fit a G-DINA cognitive-diagnosis model without fallback substitution

## Usage

``` r
fit_eyeprocess_gdina(dat, Q, model = "GDINA", ..., engine = "GDINA")
```

## Arguments

- dat:

  Response data supplied to the GDINA engine.

- Q:

  Binary item-by-attribute Q-matrix.

- model:

  Model specification passed to the selected external engine.

- ...:

  Additional arguments passed to the selected method or external engine.

- engine:

  Requested estimation or analysis engine.

## Value

An object of class "eye_external_irt_fit", stored as a named list, with
components "status", "engine", "fit", "call". It contains a G-DINA
cognitive-diagnosis model without fallback substitution and associated
metadata or diagnostics needed to interpret the result.
