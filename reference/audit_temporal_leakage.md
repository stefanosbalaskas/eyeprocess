# Audit temporal leakage in a feature provenance table

Leakage here means information becoming available after the declared
outcome boundary. It is a data/analysis property and is not an
allegation of misconduct.

## Usage

``` r
audit_temporal_leakage(provenance, allow_equal = TRUE, tolerance = 0)
```

## Arguments

- provenance:

  Output of \[process_feature_time_provenance()\] or compatible table.

- allow_equal:

  Whether features available exactly at outcome time are allowed.

- tolerance:

  Numeric tolerance in the provenance time unit.
