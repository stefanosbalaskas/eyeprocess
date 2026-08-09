# Process pre-flight and anomaly governance

## Scope

This workflow places a data-quality gate before biometric/process
modelling. It is designed to protect calibration, DIF, scoring,
process-IRT, and deployment analyses from poor signal quality. It does
not classify motivation, misconduct, diagnosis, or ability.

## Pre-flight specification

``` r

spec <- process_preflight_spec(
  min_gaze_validity = 0.80,
  min_pupil_validity = 0.70,
  max_gaze_missingness = 0.25,
  max_pupil_missingness = 0.30,
  min_valid_trial_fraction = 0.70
)

audit <- audit_biometric_preflight(
  trial_data,
  by = c("person_id", "recording_id"),
  spec = spec
)

preflight_decisions(audit)
preflight_failures(audit)
preflight_exclusion_manifest(audit)
plot(audit, type = "heatmap")
plot(audit, type = "decision_counts")
```

No rows are removed automatically.
[`apply_preflight_decision()`](https://stefanosbalaskas.github.io/eyeprocess/reference/apply_preflight_decision.md)
performs filtering only when explicitly requested and records what
decision levels were retained.

## Multivariate anomaly review

``` r

anomaly <- audit_process_anomalies(
  person_process_data,
  person = "person_id",
  metrics = c("rt_ms", "dwell_ms", "pupil_peak", "valid_gaze_prop")
)

process_anomaly_distance(anomaly)
plot(anomaly)
```

The Mahalanobis distance is a review statistic. A large distance can
reflect calibration problems, glasses, lighting, tracker loss, atypical
viewing, or other benign causes.
