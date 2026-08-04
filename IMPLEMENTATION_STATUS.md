# eyeprocess implementation status

Current development version: **0.3.0.9000**

## Implemented in the development source

- Canonical relational `eye_dataset` with 15 standardized tables.
- Explicit stream, clock, coordinate-space, AOI, quality, and provenance records.
- Generic CSV/TSV mappings and extensible adapter registry.
- Dedicated development adapters for Gazepoint Analysis, Gazepoint Biometrics,
  Tobii Pro Lab-style tabular exports, Pupil Labs Neon/Core folders, EyeLink ASC
  and Data Viewer reports, optional local EDF2ASC conversion, and SMI BeGaze
  text exports.
- Trial reconstruction, response construction, static and dynamic AOIs,
  coordinate conversion, clock alignment, and multimodal stream combination.
- Gaze and pupil filtering, interpolation, baseline correction, blink detection,
  I-VT and I-DT fixation detection, saccade derivation, and AOI visits.
- Gaze, pupil, response-time, biometric, sequence, transition, entropy, and
  process-feature extraction with provenance.
- Quality, leakage, feature-level, local-dependence, preprocessing, AOI,
  interpretive, exclusion, and readiness audits.
- Base-R plotting families for traces, scanpaths, heatmaps, pupils, AOIs,
  transitions, quality, psychometric summaries, and parameter recovery.
- Optional interfaces to `mirt`, `TAM`, `LNIRT`, `lme4`, and `brms`.
- Synthetic datasets, parameter-recovery scaffolding, power simulation,
  canonical export/import, reports, manifests, and package bridges.
- Manual pages, seven vignettes, pkgdown configuration, fixture exports, unit
  tests, and Windows installation/validation scripts.

## Validation status

Windows 11 validation with R 4.6.1 established that version 0.0.0.9004:

- installs and loads successfully;
- passes the complete unit-test suite;
- builds and rebuilds all vignettes;
- completes `R CMD check` with **0 errors, 0 warnings, and 0 notes**;
- passes `pkgdown::check_pkgdown()` with no problems; and
- passes installation and runtime smoke tests.

Version 0.2.0.9002 passed the complete Windows gate, including all unit tests,
vignettes, `R CMD check` with 0 errors/0 warnings/0 notes, pkgdown validation,
installation, runtime smoke tests, and the private real Gazepoint corpus.

Version 0.3.0.9000 adds the integrated downstream workflow and requires a fresh
Windows runtime validation before this milestone is closed.

## Evidence boundaries

Synthetic fixture compatibility demonstrates parser design, not compatibility
with every software version or export configuration. Each dedicated adapter
must be regression-tested against multiple real, de-identified exports before
its support level is described as production-ready.

Functions in `R/016-advanced-experimental.R` and original joint-process model
interfaces are exploratory. They must not be represented as established or
validated psychometric estimators until parameter recovery, calibration,
coverage, misspecification, and empirical-reproduction studies are complete.

## Empirical-format validation milestone

Version 0.2.0.9002 adds a formal compatibility-evidence layer. Adapter support
is now distinguished as declared, synthetic-fixture validated, or empirically
validated against real de-identified exports. The package can inspect source
structures, validate single exports or corpora, quantify canonical coverage,
audit preservation, test canonical round trips, anonymize datasets, and produce
reviewable validation bundles. No real vendor corpus is bundled; production
compatibility still requires user-supplied empirical exports.

## Gazepoint Analysis 7.2.0 real-export milestone

Version 0.2.0.9002 adds an adapter path derived from six paired Gazepoint
`*_all_gaze.csv` and `*_fixations.csv` files plus four multi-section Data
Summary reports. It explicitly handles blank `USER` fields, filename-derived
recording identities, monotonic `TIMETICK(f=10000000)` clocks, media-relative
`TIME(...)` resets, media-scoped fixation identifiers, AOI summary sections,
and embedded Gazepoint Biometrics channels. This version remains a runtime
validation candidate until the complete Windows gate and private real-corpus
validation have passed.


## Integrated downstream workflow milestone

Version 0.3.0.9000 adds the end-to-end real Gazepoint workflow from canonical
import through QC, media/trial reconstruction, fixation and AOI summaries, pupil
and biometric processing, plots, analysis-ready process tables, IRT-ready
response structures, canonical exports, provenance, and reproducible reports.
Responses and scores remain optional and are never fabricated.
