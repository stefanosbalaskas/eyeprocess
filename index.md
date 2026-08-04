# eyeprocess

`eyeprocess` is a vendor-neutral R framework for transforming
heterogeneous eye-tracking, pupillometry, behavioural, and biometric
exports into validated, analysis-ready process data. It provides
first-class support for Gazepoint Analysis and Gazepoint Biometrics
exports, dedicated adapters for common eye-tracking ecosystems, and
downstream psychometric modelling.

## Design commitments

- Harmonize semantics, not merely column names.
- Retain native timestamps and source files.
- Record every timebase and coordinate transformation.
- Keep vendor-produced and package-derived ocular events
  distinguishable.
- Never resample, interpolate, clip, or exclude observations silently.
- Treat gaze, pupil, and physiology as observations—not automatic
  psychological constructs.
- Keep Gazepoint support deep while the canonical representation remains
  vendor-neutral.

## Supported inputs

| Ecosystem | Initial interface | Support level |
|----|----|----|
| Gazepoint Analysis | [`read_gazepoint()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-gazepoint.md), [`read_gazepoint_folder()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-gazepoint.md) | First class |
| Gazepoint Biometrics | [`read_gazepoint_biometrics()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-gazepoint.md) | First class |
| Generic CSV/TSV | [`read_eye_generic()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-generic.md) | Universal mapping |
| Tobii Pro Lab | [`read_tobii()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-vendors.md) | Dedicated |
| Pupil Labs Neon | [`read_pupil_neon()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-vendors.md) | Dedicated |
| Pupil Labs Core | [`read_pupil_core()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-vendors.md) | Dedicated |
| EyeLink ASC | [`read_eyelink_asc()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-vendors.md) | Dedicated |
| EyeLink Data Viewer | [`read_eyelink_report()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-vendors.md) | Explicit mapping |
| EyeLink EDF | [`read_eyelink_edf()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-vendors.md) | Local EDF2ASC bridge |
| SMI BeGaze ASCII | [`read_smi()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-vendors.md) | Legacy dedicated |
| Custom adapters | [`register_eye_adapter()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-adapters.md) | Extensible |

## Empirical export validation

Version 0.2.0.9002 adds a formal evidence framework for validating real
export files rather than treating adapter availability as proof of
production compatibility. It provides:

- [`inspect_eye_source()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md)
  for file structure, fields, delimiters, hashes, and format-detection
  evidence;
- [`validate_eye_source()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md)
  for adapter, canonical-schema, timebase, coordinate, provenance,
  quality, and round-trip checks;
- [`init_validation_corpus()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md),
  [`validation_manifest()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md),
  and
  [`validate_eye_corpus()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md)
  for versioned multi-vendor compatibility corpora;
- [`schema_coverage()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md)
  and
  [`source_preservation_audit()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md)
  for loss-aware harmonization evidence;
- [`anonymize_eye_dataset()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md)
  and
  [`create_validation_bundle()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md)
  for reviewable, non-raw compatibility bundles.

Support levels are reported separately as declared, fixture-tested, or
empirically validated. Real vendor exports remain necessary before
production compatibility claims are made.

## Development validation status

Version 0.1.0.9003 is the validated baseline: on Windows 11 with R 4.6.1
it installed successfully, passed the complete unit-test suite,
completed `R CMD check` with **0 errors, 0 warnings, and 0 notes**,
passed
[`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html),
and passed runtime smoke tests.

Version 0.2.0.9002 established the validated real-structure Gazepoint
Analysis 7.2.0 adapter baseline. Version 0.3.0.9000 adds the complete
downstream workflow and requires a fresh runtime validation after
installation. The regression corpus is derived from six paired
de-identified Gazepoint sample/fixation exports and four Data Summary
reports. Production compatibility remains version-specific and must be
confirmed against additional independent exports before a general
compatibility claim is made.

The original joint-process and dynamic models are explicitly
experimental. They must undergo parameter-recovery, calibration,
coverage, misspecification, and empirical-reproduction studies before
confirmatory use.

See
[`IMPLEMENTATION_STATUS.md`](https://stefanosbalaskas.github.io/eyeprocess/IMPLEMENTATION_STATUS.md),
[`FUNCTION_REFERENCE.md`](https://stefanosbalaskas.github.io/eyeprocess/FUNCTION_REFERENCE.md),
and
[`STATIC_AUDIT.txt`](https://stefanosbalaskas.github.io/eyeprocess/STATIC_AUDIT.txt).

## Gazepoint Analysis 7.2.0 real-export workflow

The enhanced Gazepoint adapter recognizes current export names and
report structures:

``` r

x <- read_gazepoint_folder(
  "C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess-validation-corpus/cases/gazepoint-analysis-v7.2.0-demo"
)

gp_pair_exports(
  "C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess-validation-corpus/cases/gazepoint-analysis-v7.2.0-demo"
)

validate_eye_dataset(x)
schema_coverage_summary(x)
source_preservation_audit(x, require_raw = TRUE)
```

For these exports, `TIMETICK(f=10000000)` is the monotonic recording
clock. The `TIME(...)` field is retained as media-relative time because
it restarts when the media changes. Gazepoint fixation identifiers are
namespaced by media for the same reason. Multi-section Data Summary
reports are imported with
[`read_gazepoint_summary()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-import-gazepoint.md)
and converted to AOI definitions and participant-AOI features.

## Installation from the local source tree

``` r

install.packages(c(
  "ggplot2", "testthat", "knitr", "rmarkdown", "jsonlite"
))

install.packages(
  "C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess",
  repos = NULL,
  type = "source"
)
```

For development:

``` r

install.packages(c("devtools", "roxygen2", "pkgdown"))
devtools::load_all("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess")
devtools::test("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess")
devtools::check("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess")
```

## Gazepoint workflow

``` r

library(eyeprocess)

x <- read_gazepoint_folder(
  "data/P001",
  include = c("gaze", "fixations", "events", "biometrics")
)

x
validate_eye_dataset(x)
audit_signal_quality(x)
audit_timebase(x)

x <- build_trials(x, start_events = "TRIAL_START", end_events = "TRIAL_END")
x <- register_aois(
  x,
  new_aoi("prompt", x = 0, y = 0, width = 0.50, height = 1),
  new_aoi("options", x = 0.50, y = 0, width = 0.50, height = 1)
)
x <- assign_aois(x)
x <- derive_all_features(x)

plot_signal_quality(x)
plot_scanpath(x, trial_id = x$intervals$trial_id[1])
plot_pupil_timeseries(x, trial_id = x$intervals$trial_id[1])
```

## Generic export

``` r

mapping <- eye_mapping(
  participant = "subject",
  recording = "recording",
  timestamp = "timestamp_us",
  x = "gaze_x",
  y = "gaze_y",
  pupil_left = "pupil_left_mm",
  pupil_right = "pupil_right_mm",
  trial = "trial_id",
  stimulus = "stimulus"
)

x <- read_eye_generic(
  "data/export.csv",
  mapping = mapping,
  time_unit = "microseconds",
  coordinate_space = "display_pixels_top_left",
  screen_width = 1920,
  screen_height = 1080
)
```

## Psychometric workflow

``` r

x <- derive_all_features(x)

fit <- fit_explanatory_irt(
  x,
  score ~ dwell_time + first_fixation_latency + pupil_auc,
  engine = "lme4"
)

fit_rt <- fit_accuracy_rt(x, engine = "LNIRT")
```

Optional engines are deliberately not installed as mandatory
dependencies. Install only the modelling engines required for a study:

``` r

install.packages(c("mirt", "TAM", "LNIRT", "lme4"))
```

## Simulation

``` r

sim <- simulate_eye_dataset(
  n_person = 80,
  n_item = 20,
  seed = 42
)

sim <- derive_all_features(sim)
plot_eye_trace(sim, trial_id = sim$intervals$trial_id[1])
plot_transition_matrix(sim)
```

## Responsible interpretation

Run:

``` r

interpretive_warnings()
analysis_readiness(x)
provenance_manifest(x)
```

The package does not equate fixation with attention, dwell time with
difficulty, pupil dilation with cognitive load, rapid response with
guessing, or a data-derived process factor with a named psychological
construct.

## Complete Gazepoint downstream workflow

The integrated workflow converts a real Gazepoint Analysis folder into a
canonical dataset and every major downstream research artifact:

``` r

library(eyeprocess)

result <- run_gazepoint_workflow(
  "C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess-validation-corpus/cases/gazepoint-analysis-v7.2.0-demo",
  output_dir = "C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess-downstream-output",
  overwrite = TRUE
)

result
validate_gazepoint_workflow(result)
```

The output includes QC evidence, media/trial reconstruction,
vendor-fixation and AOI summaries, processed pupil and valid-only
biometric features, gaze and physiological plots, a
person-by-item-by-trial process table, IRT-ready response templates,
canonical exports, provenance, source fingerprints, a rerun script, and
Markdown/HTML reports. The workflow does not fabricate responses or fit
an IRT model automatically when observed response data are unavailable.
