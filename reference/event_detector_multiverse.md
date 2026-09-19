# Event-detector multiverse and inference robustness

Build explicit event-detector specifications, run detector branches
independently, compare event catalogues, propagate detector choice
through AOI features and an identical statistical model, and report
scientific inference stability.

## Usage

``` r
define_event_detector_spec(
  detector_id, algorithm, velocity_threshold = NULL,
  dispersion_threshold = NULL, minimum_duration_ms = NULL,
  maximum_gap_ms = NULL, merge_rule = "none", sampling_rate = NULL,
  smoothing = NULL, filter = NULL, coordinate_unit = "degrees",
  implementation = NULL, implementation_version = NULL,
  parameters = list(), callback = NULL
)
validate_event_detector_spec(spec)
create_detector_multiverse(
  specs = NULL, base_spec = NULL, parameter_grid = NULL,
  id_template = "%s_%03d", label = "event_detector_multiverse"
)
detect_events_with_spec(x, spec)
import_external_detector_events(events, spec, dataset = NULL)
run_detector_multiverse(x, multiverse, continue_on_error = TRUE)
match_detected_events(
  reference, candidate, event_type = "fixation",
  onset_tolerance_ms = 75, minimum_overlap = 0.10
)
compare_event_catalogues(
  reference, candidate, event_type = "fixation",
  onset_tolerance_ms = 75, minimum_overlap = 0.10
)
estimate_detector_agreement(
  x, event_type = "fixation",
  onset_tolerance_ms = 75, minimum_overlap = 0.10
)
summarise_detector_events(x)
summarise_detector_disagreement(x, event_type = "fixation")
propagate_detector_to_aoi(x, overlap = "error", continue_on_error = TRUE)
propagate_detector_to_features(x, continue_on_error = TRUE)
run_detector_inference_multiverse(
  x, model_spec, model_callback = NULL, minimum_valid_fraction = NULL
)
assess_detector_inference_stability(
  x, term, substantive_threshold = NULL,
  direction = c("above", "below", "absolute")
)
summarise_detector_robustness(
  x, inference = NULL, term = NULL,
  substantive_threshold = NULL, direction = "above"
)
plot_detector_event_timeline(x, trial_id = NULL, event_type = "fixation", ...)
plot_detector_agreement(x, metric = "mean_event_overlap", ...)
plot_detector_feature_distributions(
  x, feature = "dwell_time_ms", aoi_id = NULL, ...
)
plot_detector_coefficient_stability(x, term, ...)
plot_detector_multiverse(
  x, inference = NULL, term = NULL,
  feature = "dwell_time_ms", aoi_id = NULL
)
report_detector_multiverse(
  x, inference = NULL, term = NULL,
  substantive_threshold = NULL, path = NULL
)
simulate_detector_multiverse_data(
  n_participants = 12L, sampling_rate = 60,
  trial_duration_s = 2, seed = 20260918L
)
```

## Arguments

- detector_id:

  Unique detector-branch identifier.

- algorithm:

  One of `"ivt"`, `"idt"`, `"adaptive_velocity"`, `"remodnav"`,
  `"external"`, or `"vendor"`.

- velocity_threshold:

  Explicit velocity threshold in the declared coordinate unit per
  second.

- dispersion_threshold:

  Explicit dispersion threshold in the declared coordinate unit.

- minimum_duration_ms:

  Minimum event duration in milliseconds.

- maximum_gap_ms:

  Maximum gap used by algorithms that support gap handling.

- merge_rule:

  Recorded merge rule. No undeclared merging is applied.

- sampling_rate:

  Declared sample rate in Hz.

- smoothing,filter:

  Recorded preprocessing choices.

- coordinate_unit:

  `"degrees"`, `"pixels"`, or `"normalized"`.

- implementation,implementation_version:

  Detector implementation identity and version.

- parameters:

  Named algorithm-specific parameter list.

- callback:

  Explicit external detector or model callback.

- spec:

  An event-detector specification.

- specs:

  A list of explicit detector specifications.

- base_spec:

  Base specification for an explicit parameter grid.

- parameter_grid:

  Named list of parameter values. Values are study-specific
  alternatives, not universal defaults.

- id_template:

  [`sprintf()`](https://rdrr.io/r/base/sprintf.html) template for
  generated branch identifiers.

- label:

  Multiverse label.

- x:

  An `eye_dataset`, detector multiverse result, or detector inference
  result as required.

- multiverse:

  An event-detector multiverse created by
  `create_detector_multiverse()`.

- events,reference,candidate:

  Canonical or canonicalizable event data frames.

- dataset:

  Optional source `eye_dataset` used to attach provenance to imported
  events.

- continue_on_error:

  Whether to retain a failed branch and continue other specifications.

- event_type:

  Event type to compare, usually `"fixation"`.

- onset_tolerance_ms:

  Allowed onset difference for near-boundary matches.

- minimum_overlap:

  Minimum temporal intersection-over-union for event matching.

- overlap:

  AOI-overlap rule. The default `"error"` prevents silent ambiguity
  resolution.

- model_spec:

  Named list declaring the model engine, formula, outcome, and optional
  AOI/model settings.

- model_callback:

  Specialist model callback returning the documented tidy coefficient
  contract.

- minimum_valid_fraction:

  Explicit minimum gaze-validity fraction before modelling.

- term:

  Model coefficient term to assess.

- substantive_threshold:

  Optional analyst-declared effect threshold.

- direction:

  How the substantive threshold is interpreted.

- inference:

  Optional detector inference result.

- trial_id:

  Optional trial filter.

- metric:

  Agreement metric to plot.

- feature:

  Propagated feature to plot.

- aoi_id:

  Optional AOI filter.

- path:

  Optional Markdown report path.

- n_participants:

  Synthetic-example participant count.

- trial_duration_s:

  Synthetic trial duration in seconds.

- seed:

  Synthetic-data random seed.

- ...:

  Additional base-graphics arguments.

## Details

The multiverse is intended to answer whether event detection materially
changes the quantities and scientific inference used by a study. It is
not a mechanism for selecting the branch with a preferred p-value.

Zero-event trials remain represented during feature propagation. Trials
with no valid gaze remain missing rather than being converted to zero.
AOI ambiguity is an error unless an analyst explicitly selects another
overlap rule. Model engines are explicit; formula engines use `na.fail`;
and non-converged model branches are retained diagnostically but
excluded from coefficient-stability summaries.

`algorithm = "remodnav"` calls an installed REMoDNaV executable and
never substitutes an internal approximation. Pixel coordinates require
an explicit `parameters$px2deg`. The built-in adaptive-velocity
implementation is reported as a robust-MAD reference detector and must
not be described as a named external algorithm.

## Value

Specification functions return detector specification or multiverse
objects. Execution/propagation functions return governed multiverse
result objects. `run_detector_inference_multiverse()` returns an
inference result containing coefficients, failures, warnings, model
metadata, a feature fingerprint, and `input_audit`. The audit has one
row per planned detector and records `input_rows`, `aoi_selected_rows`,
`quality_excluded_rows`, `outcome_missing_rows`, `model_rows_used`, and
branch `status`. Comparison and stability functions return data frames
or named summary lists. For `assess_detector_inference_stability()`,
`specifications` is the number of planned detector specifications,
`term_available_specifications` is the number that returned the
requested coefficient, and `model_failure_specifications` is the number
with recorded model-stage failures. The convergence-rate denominator is
the full declared multiverse, so failed, missing-term, and non-converged
branches cannot disappear from robustness accounting. Plotting functions
draw base-R diagnostics and invisibly return plotted data.
`report_detector_multiverse()` returns Markdown text.
`simulate_detector_multiverse_data()` returns a synthetic `eye_dataset`.

## See also

[`detect_fixations_ivt`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-preprocessing.md),
[`detect_fixations_idt`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-preprocessing.md),
[`assign_aois`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-trials-aoi.md),
[`preprocessing_multiverse`](https://stefanosbalaskas.github.io/eyeprocess/reference/preprocessing_multiverse.md)

## Examples

``` r
d <- simulate_detector_multiverse_data(n_participants = 4, seed = 1)

s1 <- define_event_detector_spec(
  "ivt25", "ivt",
  velocity_threshold = 25,
  minimum_duration_ms = 60,
  maximum_gap_ms = 75,
  sampling_rate = 60
)
s2 <- define_event_detector_spec(
  "ivt35", "ivt",
  velocity_threshold = 35,
  minimum_duration_ms = 60,
  maximum_gap_ms = 75,
  sampling_rate = 60
)

mv <- create_detector_multiverse(list(s1, s2))
out <- run_detector_multiverse(d, mv)
out <- propagate_detector_to_aoi(out)
out <- propagate_detector_to_features(out)
summarise_detector_events(out)
#>   detector_id number_of_events number_of_fixations number_of_saccades
#> 1       ivt25               43                  43                  0
#> 2       ivt35               34                  34                  0
#>   mean_fixation_duration_ms median_fixation_duration_ms
#> 1                  327.1318                    233.3333
#> 2                  430.8824                    416.6667
#>   total_fixation_duration_ms
#> 1                   14066.67
#> 2                   14650.00
```
