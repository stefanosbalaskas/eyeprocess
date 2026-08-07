# Cognitive-episode change-point detection

Cognitive-episode change-point detection. These functions form the
eyeprocess 0.6.0.9000 measurement-intelligence programme and use
dependency-free reference implementations with explicit evidence limits.

## Usage

``` r
detect_process_changepoints(x, channels = c("gaze_velocity", "aoi", "pupil", "eda"),
  time_col = NULL, window = 10, threshold_quantile = 0.9, min_segment = 5)
segment_process_episodes(x, ...)
label_process_episodes(x, rules = NULL, model = NULL)
compare_episode_structure(x, group)
plot_process_episodes(x, ...)
plot_changepoint_ribbons(x, ...)
plot_episode_waterfall(x, ...)
plot_episode_transition_graph(x, ...)
plot_episode_duration_distribution(x, ...)
```

## Arguments

- x:

  Input object or data structure appropriate for the selected analysis.

- channels:

  Argument controlling \`channels\`; see the function usage and returned
  audit metadata.

- time_col:

  Argument controlling \`time_col\`; see the function usage and returned
  audit metadata.

- window:

  Argument controlling \`window\`; see the function usage and returned
  audit metadata.

- threshold_quantile:

  Argument controlling \`threshold_quantile\`; see the function usage
  and returned audit metadata.

- min_segment:

  Argument controlling \`min_segment\`; see the function usage and
  returned audit metadata.

- ...:

  Additional arguments passed to the underlying method or plotting
  function.

- rules:

  Argument controlling \`rules\`; see the function usage and returned
  audit metadata.

- model:

  Argument controlling \`model\`; see the function usage and returned
  audit metadata.

- group:

  Argument controlling \`group\`; see the function usage and returned
  audit metadata.

## Details

The APIs return auditable S3 objects. Plot wrappers call registered
base-graphics methods. Experimental or approximate engines are labelled
in object status fields and should be validated before confirmatory or
operational use.

## Value

An eyeprocess result object, data frame, model object, plot, or audit
table as documented by the individual function.

## See also

[`plot_diagnostics()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md),
[`plot_evidence()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md),
and
[`plot_sensitivity()`](https://stefanosbalaskas.github.io/eyeprocess/reference/030-measurement-intelligence-utils.md).
