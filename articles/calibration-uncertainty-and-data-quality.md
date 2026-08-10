# Calibration uncertainty and eye-tracking data quality

Version 0.9 makes measurement quality visible in the analysis object.
Calibration/validation error, successive-sample precision, effective
sampling frequency, irregular sampling, and data loss can be summarized
rather than hidden in preprocessing.

``` r

cal <- read.csv(system.file("extdata","calibration_targets_demo.csv", package="eyeprocess"))
m <- calibration_error_model(cal)
gaze_uncertainty_ellipse(m)
plot(m)

g <- read.csv(system.file("extdata","gaze_quality_demo.csv", package="eyeprocess"))
q <- gaze_data_quality_profile(g, valid="valid", by="person_id")
data_quality_reporting_table(q)
```

[`propagate_calibration_uncertainty()`](https://stefanosbalaskas.github.io/eyeprocess/reference/propagate_calibration_uncertainty.md)
and
[`probabilistic_aoi_assignment()`](https://stefanosbalaskas.github.io/eyeprocess/reference/probabilistic_aoi_assignment.md)
propagate empirical calibration error into AOI membership. These
probabilities concern spatial membership under the error model; they are
not probabilities of psychological attention.
