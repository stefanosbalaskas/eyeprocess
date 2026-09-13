# Audit process-dependent item discrimination

Residualises a process variable against person/item baselines, then
tests whether the theta-response slope changes with that residual
process value. This is a transparent diagnostic inspired by 2026
evidence on conditional response-time/discrimination dependencies, not
an exact reproduction of the published meta-analytic model.

## Usage

``` r
process_dependent_discrimination_audit(
  data,
  response,
  theta,
  process,
  person,
  item,
  nonlinear = TRUE
)
```

## Arguments

- data:

  Long-format response data.

- response:

  Binary response column.

- theta:

  Person latent-score column.

- process:

  RT/gaze/process measure.

- person, item:

  Identifier columns.

- nonlinear:

  If TRUE and mgcv is installed, additionally estimate a smooth
  theta-by-process diagnostic surface.

## Value

An object of class "eye_process_dependent_discrimination", stored as a
named list, with components "process_model", "response_model",
"interaction", "smooth_model", "residual_process", "status", "caveat".
It contains process-dependent item discrimination and associated
metadata or diagnostics needed to interpret the result.
