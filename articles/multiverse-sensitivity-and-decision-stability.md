# Sensitivity, specification curves, and decision stability

Reasonable preprocessing and modeling choices can change numerical
results. eyeprocess therefore represents a defensible specification set
explicitly and evaluates each branch under the same extraction contract.

``` r

grid <- process_sensitivity_grid(fixation_min_ms=c(60,80,100), pupil_interpolation=c("none","linear","spline"), max_invalid_trial=c(.2,.3,.4))
x <- run_process_sensitivity(data, grid, analysis_fun, extract_fun)
summarise_process_sensitivity(x, p_value="p_value")
decision_stability(x, p_value="p_value")
sensitivity_decision_leverage(x)
plot(x, lower="lower", upper="upper")
```

A stable declared multiverse does not validate specifications that were
never included. Stability thresholds are reporting conventions, not
universal validity cutoffs.
