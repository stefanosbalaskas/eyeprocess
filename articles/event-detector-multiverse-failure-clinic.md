# Failure clinic: detector multiverse audits

## Purpose

This clinic demonstrates that a detector-sensitivity workflow can remain
scientifically valid when individual branches fail. The goal is not to
force every branch to produce an estimate; it is to keep detection
failures, model-input attrition, invalid callback output, and
unavailable model rows explicit.

The executable source is installed at
`examples/event-detector-multiverse-failure-clinic.R`.

## Deliberate detector failure

The script defines an I-VT branch and an external detector callback that
deliberately stops.
`run_detector_multiverse(..., continue_on_error = TRUE)` records the
external branch as failed while preserving the successful I-VT branch.

``` r

external_fail <- define_event_detector_spec(
  "external_fail",
  "external",
  implementation = "deliberate_failure_fixture",
  callback = function(data, spec) {
    stop("deliberate external detector failure")
  }
)

detected <- run_detector_multiverse(
  d,
  list(ivt30, external_fail),
  continue_on_error = TRUE
)

detected$status
detected$failures
```

The failed branch is not replaced by another detector.

## Model-input audit

The clinic then creates one prespecified quality exclusion and one
non-finite dwell outcome. The resulting `input_audit` separates:

- all propagated detector rows;
- rows selected for the target AOI;
- rows excluded by the declared quality threshold;
- rows with non-finite outcomes;
- rows supplied to the estimator;
- final branch status.

``` r

inference <- run_detector_inference_multiverse(
  features,
  list(
    engine = "callback",
    formula = dwell_time_ms ~ condition_id,
    outcome = "dwell_time_ms",
    aoi_id = "disclosure"
  ),
  model_callback = tidy_callback,
  minimum_valid_fraction = .5
)

inference$input_audit
inference$warnings
```

Missing outcomes remain missing; they are never recoded as zero.

## Invalid callback output

A second callback deliberately returns the same coefficient term twice.
That branch fails rather than contributing two coefficient rows to the
stability summary.

``` r

duplicate_callback <- function(data, model_spec) {
  row <- tidy_callback(data, model_spec)
  rbind(row, row)
}

invalid <- run_detector_inference_multiverse(
  features,
  model_spec,
  model_callback = duplicate_callback
)

invalid$failures
```

## Interpretation

A failed branch is not automatically evidence that the detector is poor,
and a successful branch is not evidence that its events are valid.
Failures show whether the declared analysis was executable under each
specification. Scientific interpretation still depends on the detector
rationale, data quality, event agreement, AOIs, downstream model, and
any external validation.

## Reporting example

> Two detector specifications were planned. The internal I-VT branch
> completed, whereas the deliberately failing external branch was
> retained as a detector-stage failure and was not replaced. During
> model propagation, the input audit separately recorded target-AOI
> selection, one prespecified quality exclusion, one non-finite outcome,
> and the resulting model N. An invalid duplicate-term callback was
> rejected and recorded as a model-stage failure.

## Related material

See
[`vignette("event-detector-multiverse")`](https://stefanosbalaskas.github.io/eyeprocess/articles/event-detector-multiverse.md)
for the full workflow,
[`vignette("event-detector-multiverse-visual-reporting")`](https://stefanosbalaskas.github.io/eyeprocess/articles/event-detector-multiverse-visual-reporting.md)
for the plot-first diagnostic sequence, and the installed failure-clinic
script for an executable synthetic example.
