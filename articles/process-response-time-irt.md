# Response-time and process-aware IRT

Eye-tracking, pupil, response-time, and sequence channels are
**measurement channels**. Their association with latent response models
does not automatically identify cognitive strategy, effort, diagnosis,
or mental state.

``` r

spec <- eyeprocess_joint_process_irt_spec(
  response_family = "2pl", time_model = "lognormal",
  process_channels = c("dwell", "pupil", "transitions"),
  missingness = "ignorable"
)
spec
#> eyeprocess joint process-IRT specification
#>   response family : 2pl 
#>   response time   : lognormal 
#>   process channels: dwell, pupil, transitions 
#>   status          : experimental
```

[`eyeprocess_process_irt_data_bundle()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess_process_irt_data_bundle.md)
preserves sparse person-item data and declared process channels.
Descriptive functions summarize response-time structure, speed-accuracy
association, item/person process profiles, missingness patterns, and
alignment without turning process measures into latent-state labels.

When exact joint response/response-time estimation is requested,
[`fit_eyeprocess_lnirt()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_eyeprocess_lnirt.md)
delegates to LNIRT if available and otherwise returns a gated result
with `fit = NULL`.

Primary package source: <https://cran.r-project.org/package=LNIRT>.
