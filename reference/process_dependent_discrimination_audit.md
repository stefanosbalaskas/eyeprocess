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
