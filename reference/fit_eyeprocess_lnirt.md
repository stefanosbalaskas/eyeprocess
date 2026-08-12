# Fit a joint response/response-time LNIRT model without fallback substitution

Fit a joint response/response-time LNIRT model without fallback
substitution

## Usage

``` r
fit_eyeprocess_lnirt(Y, RT, quadratic = FALSE, ..., engine = "LNIRT")
```

## Arguments

- Y:

  Item-response matrix supplied to LNIRT.

- RT:

  Response-time matrix supplied to LNIRT.

- quadratic:

  Whether the LNIRT quadratic option is requested.

- ...:

  Additional arguments passed to the selected method or external engine.

- engine:

  Requested estimation or analysis engine.
