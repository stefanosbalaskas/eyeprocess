# eyeprocess

[![DOI](https://zenodo.org/badge/1322747590.svg)](https://doi.org/10.5281/zenodo.21844472)

<!-- badges: start -->
**Latest archived release:** 0.8.0

**Current formal release:** 0.8.0
[![R-CMD-check](https://github.com/stefanosbalaskas/eyeprocess/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/stefanosbalaskas/eyeprocess/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

`eyeprocess` is a vendor-neutral R framework for transforming heterogeneous
eye-tracking, pupillometry, behavioural, and biometric exports into validated,
analysis-ready process data. It provides first-class support for Gazepoint
Analysis and Gazepoint Biometrics exports, dedicated adapters for common
eye-tracking ecosystems, and downstream psychometric modelling.

## Design commitments

- Harmonize semantics, not merely column names.
- Retain native timestamps and source files.
- Record every timebase and coordinate transformation.
- Keep vendor-produced and package-derived ocular events distinguishable.
- Never resample, interpolate, clip, or exclude observations silently.
- Treat gaze, pupil, and physiology as observations—not automatic psychological
  constructs.
- Keep Gazepoint support deep while the canonical representation remains
  vendor-neutral.

## Supported inputs

| Ecosystem | Initial interface | Support level |
|---|---|---|
| Gazepoint Analysis | `read_gazepoint()`, `read_gazepoint_folder()` | First class |
| Gazepoint Biometrics | `read_gazepoint_biometrics()` | First class |
| Generic CSV/TSV | `read_eye_generic()` | Universal mapping |
| Tobii Pro Lab | `read_tobii()` | Dedicated |
| Pupil Labs Neon | `read_pupil_neon()` | Dedicated |
| Pupil Labs Core | `read_pupil_core()` | Dedicated |
| EyeLink ASC | `read_eyelink_asc()` | Dedicated |
| EyeLink Data Viewer | `read_eyelink_report()` | Explicit mapping |
| EyeLink EDF | `read_eyelink_edf()` | Local EDF2ASC bridge |
| SMI BeGaze ASCII | `read_smi()` | Legacy dedicated |
| Custom adapters | `register_eye_adapter()` | Extensible |


## Empirical export validation

Version 0.2.0.9002 adds a formal evidence framework for validating real export
files rather than treating adapter availability as proof of production
compatibility. It provides:

- `inspect_eye_source()` for file structure, fields, delimiters, hashes, and
  format-detection evidence;
- `validate_eye_source()` for adapter, canonical-schema, timebase, coordinate,
  provenance, quality, and round-trip checks;
- `init_validation_corpus()`, `validation_manifest()`, and
  `validate_eye_corpus()` for versioned multi-vendor compatibility corpora;
- `schema_coverage()` and `source_preservation_audit()` for loss-aware
  harmonization evidence;
- `anonymize_eye_dataset()` and `create_validation_bundle()` for reviewable,
  non-raw compatibility bundles.

Support levels are reported separately as declared, fixture-tested, or
empirically validated. Real vendor exports remain necessary before production
compatibility claims are made.

## Development validation status

Version 0.1.0.9003 is the validated baseline: on Windows 11 with R 4.6.1 it
installed successfully, passed the complete unit-test suite, completed
`R CMD check` with **0 errors, 0 warnings, and 0 notes**, passed
`pkgdown::check_pkgdown()`, and passed runtime smoke tests.

Version 0.2.0.9002 established the validated real-structure Gazepoint Analysis 7.2.0 adapter baseline. Version 0.3.0.9000 adds the complete downstream workflow and requires a fresh runtime validation after installation. The regression corpus is derived from six paired de-identified Gazepoint sample/fixation exports and four Data Summary reports. Production compatibility remains version-specific and must be confirmed against additional independent exports before a general compatibility claim is made.


Version 0.8.0 is the current formal release, cut from the validated 0.8.0.9000 development baseline. That baseline passed the focused 0.8 test suite and the complete eyeprocess test suite, pkgdown validation, installation with vignettes, installed-package smoke validation, and R CMD check with 0 errors, 0 warnings, and 0 notes. The formal release also passed definitive release-artifact validation and all six post-merge GitHub CI checks.
The original joint-process and dynamic models are explicitly experimental. They
must undergo parameter-recovery, calibration, coverage, misspecification, and
empirical-reproduction studies before confirmatory use.

See [`IMPLEMENTATION_STATUS.md`](IMPLEMENTATION_STATUS.md),
[`FUNCTION_REFERENCE.md`](FUNCTION_REFERENCE.md), and
[`STATIC_AUDIT.txt`](STATIC_AUDIT.txt).


## Gazepoint Analysis 7.2.0 real-export workflow

The enhanced Gazepoint adapter recognizes current export names and report structures:

```r
x <- read_gazepoint_folder(
  "path/to/eyeprocess-validation-corpus/cases/gazepoint-analysis-v7.2.0-demo"
)

gp_pair_exports(
  "path/to/eyeprocess-validation-corpus/cases/gazepoint-analysis-v7.2.0-demo"
)

validate_eye_dataset(x)
schema_coverage_summary(x)
source_preservation_audit(x, require_raw = TRUE)
```

For these exports, `TIMETICK(f=10000000)` is the monotonic recording clock. The `TIME(...)` field is retained as media-relative time because it restarts when the media changes. Gazepoint fixation identifiers are namespaced by media for the same reason. Multi-section Data Summary reports are imported with `read_gazepoint_summary()` and converted to AOI definitions and participant-AOI features.

## Installation from the local source tree

```r
install.packages(c(
  "ggplot2", "testthat", "knitr", "rmarkdown", "jsonlite"
))

install.packages(
  "path/to/eyeprocess",
  repos = NULL,
  type = "source"
)
```

For development:

```r
install.packages(c("devtools", "roxygen2", "pkgdown"))
devtools::load_all("path/to/eyeprocess")
devtools::test("path/to/eyeprocess")
devtools::check("path/to/eyeprocess")
```

## Gazepoint workflow

```r
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

```r
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

```r
x <- derive_all_features(x)

fit <- fit_explanatory_irt(
  x,
  score ~ dwell_time + first_fixation_latency + pupil_auc,
  engine = "lme4"
)

fit_rt <- fit_accuracy_rt(x, engine = "LNIRT")
```

Optional engines are deliberately not installed as mandatory dependencies.
Install only the modelling engines required for a study:

```r
install.packages(c("mirt", "TAM", "LNIRT", "lme4"))
```

## Simulation

```r
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

```r
interpretive_warnings()
analysis_readiness(x)
provenance_manifest(x)
```

The package does not equate fixation with attention, dwell time with difficulty,
pupil dilation with cognitive load, rapid response with guessing, or a
data-derived process factor with a named psychological construct.

## Complete Gazepoint downstream workflow

The integrated workflow converts a real Gazepoint Analysis folder into a
canonical dataset and every major downstream research artifact:

```r
library(eyeprocess)

result <- run_gazepoint_workflow(
  "path/to/eyeprocess-validation-corpus/cases/gazepoint-analysis-v7.2.0-demo",
  output_dir = "path/to/eyeprocess-downstream-output",
  overwrite = TRUE
)

result
validate_gazepoint_workflow(result)
```

The output includes QC evidence, media/trial reconstruction, vendor-fixation
and AOI summaries, processed pupil and valid-only biometric features, gaze and
physiological plots, a person-by-item-by-trial process table, IRT-ready response
templates, canonical exports, provenance, source fingerprints, a rerun script,
and Markdown/HTML reports. The workflow does not fabricate responses or fit an
IRT model automatically when observed response data are unavailable.

## Validation and interoperability programme

Development version 0.4.0.9000 adds Eye-Tracking-BIDS import/export,
optional Arrow/Parquet storage, explicit multi-vendor empirical-validation
gates, advanced-model evidence audits, parameter-recovery and SBC harnesses,
grouped validation, leakage and multiverse diagnostics, benchmark/reporting
assets, and four explicitly experimental advanced model families.

Software availability is not evidence of scientific validity: vendor claims
require independent real exports, and advanced models remain experimental
until their declared recovery, calibration, misspecification, grouped
validation, equivalence, and empirical-reproduction gates pass.

## Research-scale validation programme (0.5.0.9000)

The development programme now supports deterministic and resumable validation jobs, independent vendor-evidence corpora, stable object/storage contracts, optional Stan engines, and a synthetic multimodal benchmark.

Advanced dynamic-state, functional-pupil, strategy-mixture, and gaze-diffusion models remain explicitly experimental until their declared recovery, calibration, misspecification, grouped-validation, equivalence, sensitivity, and empirical-reproduction gates pass.

## Measurement-intelligence programme (0.6.0)

This development programme adds probabilistic and compositional AOI analysis, explicit process-measurement uncertainty, recalibration and device-linking audits, process reliability, pupil phase-amplitude registration, missingness sensitivity, recurrence and point-process models, representative scanpaths, cognitive episodes, multi-objective item-bank decisions, process-DIF monitoring, conditional reference centiles, and evidence-provenance graphs.

The implementations are auditable reference engines. Experimental spatial, temporal, fairness, normative, and anomaly outputs are review evidence rather than automatic causal, clinical, ability, or misconduct determinations.


## Process-IRT validation framework (0.7.0.9000)

Version 0.7.0.9000 established the process-IRT validation framework: explicit validation contracts, model-evidence workflows, advanced process-model infrastructure, parameter-recovery and diagnostic support, and conservative estimator maturity boundaries.

Advanced process models remain subject to their declared validation requirements; availability of a model interface is not treated as evidence that the estimator is suitable for unrestricted confirmatory use.

## Process measurement and deployment governance (0.8.0)

Version 0.8.0 extends eyeprocess with process-measurement and deployment-governance infrastructure for biometric/process pre-flight audits, exclusion manifests, deployment drift, temporal process windows, AOI trajectories, advanced pupillometry, visual-context/testlet IRT, multiblock representations, process-profile mixtures, external validity, streaming scoring, validation bundles, item seeding, presentation sensitivity, process-decision features, and advanced sensitivity and Bayesian/3PL diagnostics.

The 0.8 milestone also makes frontier-estimator status explicit: methods without an exact validated implementation remain gated rather than silently falling back to a simpler estimator. Process evidence is not interpreted automatically as a mental-state, clinical, ability, misconduct, or other person-level diagnosis.

## Citation

If you use `eyeprocess` in research, please cite the archived software release:

Balaskas, S. (2026). *eyeprocess: Harmonize Eye-Tracking, Pupillometry, Biometrics, and Psychometric Process Data* (Version 0.8.0). Zenodo. https://doi.org/10.5281/zenodo.21865277

- Version 0.8.0 DOI: `10.5281/zenodo.21865277`
- Concept DOI: `10.5281/zenodo.21844472`

See `CITATION.cff` for machine-readable citation metadata.

Version 0.8.0 is the current formal archived software release. Its version-specific Zenodo DOI is `10.5281/zenodo.21865277`; the concept DOI remains `10.5281/zenodo.21844472`. Studies using eyeprocess should report the exact package version and, when relevant, the source commit used.

## 0.10 M3: response + RT + gaze + pupil

The current 0.10 development line now contains a unified four-channel measurement layer built on the frozen M2 response + RT + gaze model. M3 adds pupil as a **neutral observed process channel**, not as an automatic cognitive-load, effort, attention, or arousal label.

Reference ladder: **M0 response -> M1 response + RT -> M2 response + RT + gaze -> M3 response + RT + gaze + pupil**. M3 reuses the existing `irt_*_channel()` / `multimodal_irt_spec()` architecture and the package's functional-pupil, deconvolution, confound, blink/quality and device-audit machinery rather than creating a parallel pupil subsystem.

The M3 summary-level pupil likelihood carries eight explicit nuisance terms: baseline, luminance, gaze X/Y, measurement quality, blink status, interpolation status, and time-on-task. Missing supplied nuisance values on observed-pupil trials are not silently imputed.

M3 provides: a four-channel CmdStan reference likelihood; explicit baseline/luminance/gaze-position/quality/blink/interpolation/time-on-task pupil nuisance adjustment; retained-truth simulation; identifiability/support audits; PPC; the complete eight-model response-anchored RT/gaze/pupil ablation lattice; process-information, redundancy and channel-conflict evidence; sensor value-of-information screens; pupil falsification controls; missingness/device stress; parameter recovery; and an explicit bridge to existing functional-pupil/deconvolution workflows.

Public M3 interfaces: `multimodal_m3_spec()`, `simulate_multimodal_m3()`, `fit_multimodal_m3()`, `audit_multimodal_m3_identifiability()`, `multimodal_m3_ppc()`, `multimodal_m3_ablation()`, `multimodal_m3_process_information()`, `multimodal_m3_negative_controls()`, `multimodal_m3_functional_bridge()`, `multimodal_m3_recovery()`, and `validate_multimodal_m3()`; publication-oriented visual diagnostics are consolidated behind S3 `plot(..., type = )` methods.

```r
sim <- simulate_multimodal_m3(n_person = 80, n_item = 10, seed = 20260815)
audit_multimodal_m3_identifiability(sim)
spec <- multimodal_m3_spec()
# fit <- fit_multimodal_m3(sim)  # requires CmdStanR/CmdStan
neg <- multimodal_m3_negative_controls(sim)
```

The evidence contract explicitly permits a scientifically important negative conclusion: **pupil may add no clear defensible response-target measurement information after RT, gaze, nuisance adjustment, and uncertainty are considered**. M3 uses ignorable channel omission in its current reference likelihood; informative missingness, device transport/equivalence, construct validity, and functional-trajectory sufficiency remain explicit validation questions rather than defaults.

M3 remains development/evidence-gated until the full recovery, missingness, device, negative-control, installed-package, multi-chain and empirical validation programme is frozen.
