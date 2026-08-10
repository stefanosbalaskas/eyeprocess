# Negative controls, placebo windows, and temporal leakage

Predictive and process-feature workflows can accidentally use
information that is unavailable at the intended decision boundary. The
temporal provenance layer makes availability explicit.

``` r

p <- process_feature_time_provenance(c("dwell_pre","rt_final"), c(400,1200), outcome_at=c(1000,1000))
audit_temporal_leakage(p)
```

Negative controls deliberately break a declared process–outcome relation
and rerun the same analysis.

``` r

nc <- run_process_negative_controls(data, outcome="y", analysis_fun=analysis_fun, replications=200)
summarise_process_negative_controls(nc)
process_null_benchmark(observed_effect, nc)
plot(nc)
```

A leakage flag denotes temporal/information contamination, not
misconduct. Null-like negative controls are useful diagnostics but do
not prove model validity.
