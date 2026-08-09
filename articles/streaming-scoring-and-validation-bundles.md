# Streaming scoring and validation evidence bundles

## Partial and streaming scoring

``` r

partial <- score_partial_response_pattern(
  calibrated_mirt_model,
  response_pattern = c(1, 0, 1, NA, NA, NA),
  method = "MAP"
)

stream <- score_response_stream(
  calibrated_mirt_model,
  response_pattern = c(1, 0, 1, 1, 0, 1),
  method = "MAP"
)
streaming_score_history(stream)
plot(stream)
```

Streaming scoring is an operational building block. High-stakes
deployment still requires calibrated item banks, latency/stopping
validation, privacy governance, and score-use rules.

## Unified evidence bundles

``` r

bundle <- collect_validation_evidence(
  model_spec = spec,
  recovery = recovery_summary,
  coverage = coverage_audit,
  convergence = convergence_audit,
  ppc = ppc,
  stress_tests = stress_tests,
  external_validation = external_results,
  process_ablation = ablation,
  negative_controls = negative_controls,
  preflight = preflight,
  drift = drift,
  model_name = "joint_process_model"
)

validation_bundle_manifest(bundle)
cat(validation_report(bundle), sep = "\n")
plot(bundle)
export_validation_bundle(bundle, "validation-export")
```
