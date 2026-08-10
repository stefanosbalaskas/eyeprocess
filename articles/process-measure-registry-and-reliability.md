# Process-measure registry, repeatability, and reliability

The process-measure registry separates an observed feature from the
construct claims sometimes attached to it. Each registry entry records
channel, unit, aggregation level, interpretation, guardrail, and status.

``` r

process_measure_registry()
process_measure_card("pupil_peak")
process_measure_guardrails()
```

Repeatability can then be evaluated independently of construct meaning.

``` r

r <- read.csv(system.file("extdata","reliability_demo.csv", package="eyeprocess"))
process_icc(r, "person_id", "session", "dwell_time")
profile <- process_reliability_profile(r, "person_id", "session", "dwell_time")
plot(profile, type="bland_altman")
```

High repeatability does not by itself establish validity, diagnostic
meaning, or person-level interpretability.
