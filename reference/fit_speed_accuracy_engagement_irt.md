# Speed-accuracy-engagement IRT convenience wrapper

Speed-accuracy-engagement IRT convenience wrapper

## Usage

``` r
fit_speed_accuracy_engagement_irt(data, ..., engine = c("reference", "brms"))
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- ...:

  Additional arguments passed to the selected model, engine, or method.

- engine:

  Estimation engine.

## Value

An object of class "eye_joint_gaze_rt_irt", stored as a named list, with
components "engine", "response_model", "rt_model", "gaze_model",
"person_scores", "item_scores", "person_covariance", "item_covariance",
"data_n", "gaze_family", "columns", "status", and additional components.
It contains speed-accuracy-engagement IRT convenience wrapper and
associated metadata or diagnostics needed to interpret the result.
