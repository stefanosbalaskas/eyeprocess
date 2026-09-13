# Audit multivariate process/data-quality anomalies

Computes a regularized Mahalanobis distance over selected person-level
process metrics. Flags indicate review needs only; they are not
cheating, identity, diagnosis, or intent classifications.

## Usage

``` r
audit_process_anomalies(
  data,
  person = "person_id",
  metrics = NULL,
  alpha = 0.975,
  aggregate = TRUE,
  ridge = 1e-06
)
```

## Arguments

- data:

  Data frame.

- person:

  Person identifier column.

- metrics:

  Numeric process metrics. If omitted, usable numeric columns are
  selected.

- alpha:

  Chi-square review quantile.

- aggregate:

  If TRUE, aggregate metrics to person level before auditing.

- ridge:

  Diagonal covariance regularization.

## Value

An object of class "eye_process_anomaly_audit", stored as a named list,
with components "table", "metrics", "alpha", "threshold", "center",
"covariance", "caveat". It contains multivariate process/data-quality
anomalies and associated metadata or diagnostics needed to interpret the
result.
