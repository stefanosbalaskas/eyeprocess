# eyeprocess implementation status

Current development version: **0.1.0.9003**

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

Version 0.1.0.9003 adds substantial empirical-format validation functionality,
new documentation, and new tests. It has passed the static source audit recorded
in `STATIC_AUDIT.txt`; a fresh Windows runtime validation is required before the
milestone is closed.

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

Version 0.1.0.9003 adds a formal compatibility-evidence layer. Adapter support
is now distinguished as declared, synthetic-fixture validated, or empirically
validated against real de-identified exports. The package can inspect source
structures, validate single exports or corpora, quantify canonical coverage,
audit preservation, test canonical round trips, anonymize datasets, and produce
reviewable validation bundles. No real vendor corpus is bundled; production
compatibility still requires user-supplied empirical exports.
