# Build an integrated IRT diagnostic dashboard object

Build an integrated IRT diagnostic dashboard object

## Usage

``` r
eyeprocess_irt_fit_dashboard(
  item_fit = NULL,
  person_fit = NULL,
  q3 = NULL,
  parameter_audit = NULL,
  identification = NULL
)
```

## Arguments

- item_fit:

  Item-fit diagnostic object or table.

- person_fit:

  Person-fit diagnostic object or table.

- q3:

  Q3 residual-correlation matrix or summary.

- parameter_audit:

  Item-parameter plausibility audit.

- identification:

  Identification specification or identification audit.

## Value

An object of class "eye_irt_fit_dashboard", stored as a named list, with
components "components", "present", "n_components", "interpretation". It
contains an integrated IRT diagnostic dashboard object and associated
metadata or diagnostics needed to interpret the result.
