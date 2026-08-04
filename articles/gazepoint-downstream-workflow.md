# Complete Gazepoint Downstream Workflow

## Purpose

[`run_gazepoint_workflow()`](https://stefanosbalaskas.github.io/eyeprocess/reference/run_gazepoint_workflow.md)
executes the full research-data workflow from a real Gazepoint Analysis
folder:

1.  canonical `eye_dataset` import;
2.  file-pair, timebase, coordinate, sampling-rate, and signal-quality
    audits;
3.  contiguous media-run reconstruction as person-by-item-by-trial
    intervals;
4.  vendor-fixation and AOI summaries;
5.  short-gap pupil interpolation, optional filtering, and blink
    detection;
6.  valid-only biometric summaries while preserving native values;
7.  gaze, pupil, biometric, AOI, and QC plots;
8.  one-row-per-person-item-trial process tables;
9.  response templates and IRT-ready long/matrix structures;
10. canonical exports, provenance, source fingerprints, and reproducible
    reports.

The workflow does not manufacture response scores. When no observed
responses are supplied, the result is marked
`process_ready_response_pending`.

## Minimal workflow

``` r

library(eyeprocess)

source_dir <- "C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess-validation-corpus/cases/gazepoint-analysis-v7.2.0-demo"
output_dir <- "C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess-downstream-output"

result <- run_gazepoint_workflow(
  source_dir,
  output_dir = output_dir,
  overwrite = TRUE
)

result
validate_gazepoint_workflow(result)
```

## Explicit specification

Pupil baseline correction is deliberately disabled by default. The first
samples after media onset are not automatically equivalent to a
pre-stimulus baseline.

``` r

spec <- gazepoint_workflow_spec(
  expected_sampling_rate = 60,
  minimum_valid_gaze = 0.80,
  minimum_valid_pupil = 0.70,
  pupil_interpolation = "linear",
  pupil_max_gap_ms = 150,
  pupil_filter = "median",
  pupil_window = 5,
  pupil_baseline = "none",
  create_plots = TRUE,
  create_html_report = TRUE,
  retain_raw = TRUE
)
```

## Item labels and conditions

By default, `item_id` equals Gazepoint `MEDIA_ID`. A study-specific
mapping can supply meaningful item and condition labels.

``` r

item_map <- data.frame(
  stimulus_id = c("0", "1"),
  item_id = c("item_control", "item_treatment"),
  condition_id = c("control", "treatment")
)

result <- run_gazepoint_workflow(
  source_dir,
  output_dir,
  item_map = item_map,
  spec = spec,
  overwrite = TRUE
)
```

## Adding observed responses

Responses may be supplied now or joined later using the generated
`irt/response-template.csv` file.

``` r

responses <- data.frame(
  participant_id = c("User 3", "User 3"),
  item_id = c("item_control", "item_treatment"),
  response = c("yes", "no"),
  score = c(1, 1),
  response_time = c(6.1, 7.4)
)

result <- run_gazepoint_workflow(
  source_dir,
  output_dir,
  responses = responses,
  item_map = item_map,
  spec = spec,
  overwrite = TRUE
)
```

The workflow creates response and response-time matrices only when the
relevant observations are available. It does not fit IRT automatically;
model adequacy, sample size, item count, dimensionality, and
process-covariate assumptions must be evaluated first.

## Output structure

``` text
eyeprocess-downstream-output/
├── canonical-dataset/
├── qc/
├── tables/
├── irt/
├── plots/
│   ├── summary/
│   ├── gaze/
│   ├── fixations/
│   ├── pupil/
│   └── biometrics/
├── gazepoint-workflow-report.md
├── gazepoint-workflow-report.html
├── workflow-result.rds
├── workflow-spec.rds
├── source-fingerprint.csv
├── session-info.txt
└── rerun-workflow.R
```

## Interpretation boundaries

Fixations are not automatically attention; dwell time is not
automatically difficulty; pupil dilation is not automatically cognitive
load; and GSR or heart rate does not identify a specific emotion. The
report preserves these interpretive safeguards alongside the analysis
outputs.
