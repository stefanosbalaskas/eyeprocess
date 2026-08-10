# Empirical validation programmes in eyeprocess

eyeprocess 0.9 treats validation as an explicit research object rather
than an implicit property of an estimator.
[`process_validation_design()`](https://stefanosbalaskas.github.io/eyeprocess/reference/process_validation_design.md)
declares the measurement regimes to study;
[`run_process_validation()`](https://stefanosbalaskas.github.io/eyeprocess/reference/run_process_validation.md)
records recovery, interval coverage, convergence, warnings, and
failures; and
[`freeze_validation_reference()`](https://stefanosbalaskas.github.io/eyeprocess/reference/freeze_validation_reference.md)
supports regression-style evidence freezing.

``` r

d <- process_validation_design(n_persons=c(50,150), n_trials=c(10,30), missingness=c(0,.15), sampling_rate_hz=c(60,120), replications=100)
x <- run_process_validation(d)
validation_recovery_table(x)
validation_coverage_table(x)
validation_failure_profile(x)
plot(x, type="recovery")
```

The built-in simulator is a software-validation fixture with known
truth. It is not a substantive psychological theory. Recovery under a
supplied data-generating process does not establish external validity.
