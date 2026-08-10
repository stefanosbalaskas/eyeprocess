# Computational benchmarking and synthetic stress testing

Scientific validity and computational feasibility are separate
questions.
[`eye_benchmark_design()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eye_benchmark_design.md)
measures runtime/scaling under declared dataset sizes, while synthetic
corruption plans probe robustness to missingness, pupil dropout,
calibration offsets, timestamp jitter, AOI label noise, device shifts,
and trial imbalance.

``` r

plans <- list(
 synthetic_corruption_plan(missingness=.05),
 synthetic_corruption_plan(missingness=.20, sampling_jitter_sd=2),
 synthetic_corruption_plan(pupil_dropout=.30, gaze_offset_x=.02)
)
st <- stress_test_process_pipeline(data, plans, analysis_fun)
stress_test_summary(st)
plot(st, severity="missingness", metric="effect")
```

Stress tests describe sensitivity to the perturbations actually
supplied. They do not replace validation on independent empirical data.
