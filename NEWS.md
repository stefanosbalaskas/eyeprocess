# eyeprocess 0.8.0

## Process measurement and deployment governance

- Added biometric/process pre-flight governance and explicit exclusion manifests.
- Added deployment-drift auditing across device, site, vendor, and stimulus version.
- Added temporal process windows, AOI trajectory features, growth models, and window-sensitivity audits.
- Added advanced pupil frequency, activity, event-deconvolution, confound, fatigue, and filtering workflows.
- Added visual-context/testlet IRT and multiblock process representations.
- Added process-profile mixtures, external-validity tools, streaming scoring, and validation bundles.
- Added item-parameter seeding and candidate-bank auditing.
- Added nonclinical presentation/accessibility sensitivity workflows.
- Added process-decision proxies and structured/unstructured process-feature comparisons.
- Added advanced Rasch, mixture, imputation, Bayesian, and gaze-aware 3PL diagnostic infrastructure.
- Added explicit gated frontier estimators where an exact validated implementation is not available.
- Added 11 new pkgdown articles, focused 0.8 tests, reference documentation, and validation registries.

### Validation

- Focused 0.8 tests: PASS.
- Complete eyeprocess test suite: PASS.
- `pkgdown::check_pkgdown()`: PASS.
- `R CMD check`: 0 errors, 0 warnings, 0 notes.
- Installation with vignettes and installed-package smoke validation: PASS.
- GitHub PR #9: all 8 CI checks passed before merge.

### Scientific governance

- Process evidence is not automatically interpreted as a mental-state, clinical, ability, or misconduct diagnosis.
- Frontier estimators remain explicitly gated rather than silently substituting simpler estimators.
- Experimental and custom model specifications retain conservative maturity labels.

# eyeprocess 0.7.0.9000

## Process-IRT validation framework

- Added the process-IRT validation framework and associated validation contracts.
- Expanded advanced process-model validation, diagnostics, and evidence infrastructure.
- Preserved explicit experimental and validation-gated maturity boundaries for advanced estimators.
- Added focused validation coverage and pkgdown documentation for the 0.7 programme.

# eyeprocess 0.6.0

## Measurement uncertainty and AOI evidence

- Added probabilistic AOI assignment, fuzzy transition summaries, compositional AOI analysis, and explicit measurement-uncertainty propagation and budgets.
- Added calibration-drift detection, offline recalibration, process reliability and Generalizability Theory, and cross-device linking/equivalence audits.

## Temporal, spatial, and pupil-process science

- Added pupil phase-amplitude registration, informative-missingness and MNAR sensitivity, recurrence and cross-recurrence analysis, experimental fixation point-process models, representative scanpaths, and cognitive-episode segmentation.

## Decision intelligence and provenance

- Added multi-objective item-bank optimization, dynamic process-DIF and fairness drift, conditional process centiles, and auditable evidence/decision provenance graphs.
- Added a shared S3 plotting framework, 18 targeted test modules, eight articles, CI, manual documentation, and a standalone validation programme.

# eyeprocess 0.5.0.9000

## Research-scale validation and reproducible orchestration

- Added deterministic Monte Carlo job plans, atomic checkpoints, resumption, parallel execution, manifests, recovery/calibration/failure summaries, plots, reports, and model-promotion audits.
- Added manual research-validation workflows and chunk-based execution scripts; full simulations remain opt-in and are not run during ordinary package checks.

## Advanced model hardening

- Hardened dynamic IRTree models with transition designs, structural zeros, irregular time, uncertain states, multinomial and optional Stan hidden-state engines, decoding, residuals, comparison, and recovery programmes.
- Added functional pupil–IRT, theory-constrained strategy-mixture, and Wiener gaze-diffusion engines with explicit scientific-evidence gates.

## Vendor evidence, contracts, storage, and reproducibility

- Added independent multi-vendor corpus registration, fingerprinting, redaction, semantic comparison, round-trip loss audits, and declared/fixture/empirical compatibility matrices.
- Added stable API/object contracts, atomic partitioned Arrow/Parquet/CSV/RDS storage, schema migration, corruption detection, optional-engine adapters, and a fully synthetic multimodal public benchmark.

# eyeprocess 0.4.0.9000

* Added Eye-Tracking-BIDS import/export and optional RDS/Parquet/Arrow storage.
* Added interoperability contracts for eye-tracking, sequence, GDINA, diffIRT, and OpenMx ecosystems.
* Added multi-vendor empirical-validation audits and an executable validation-release programme.
* Added parameter-recovery, interval-coverage, simulation-based calibration, advanced-model evidence gates, engine-equivalence, empirical-reproduction, grouped-validation, leakage, multiverse, benchmarking, reporting, and public-benchmark infrastructure.
* Added dynamic gaze-state IRTree, functional pupil-IRT, theory-defined strategy-IRT, and gaze-informed diffusion model families with explicit experimental safeguards.
* Added tests, plots, templates, scripts, and four research vignettes.

# eyeprocess 0.3.0.9001

# eyeprocess 0.3.0.9000

- Added `run_gazepoint_workflow()` as the complete, reproducible downstream workflow for real Gazepoint folders.
- Added media-run reconstruction into explicit person-by-item-by-trial intervals with optional study-specific item and condition maps.
- Added integrated QC evidence for file pairing, canonical validation, timebases, coordinate spaces, sampling rates, gaze and pupil validity, trial coverage, episodes, and multimodal clock overlap.
- Added pupil interpolation/filtering and blink derivation with baseline correction disabled by default unless explicitly declared.
- Added valid-only biometric analysis values and trial-level EDA, heart-rate, IBI, dial, pupil, gaze, fixation, and AOI summaries while preserving native data.
- Added one-row-per-person-item-trial process tables, AOI/fixation/pupil/biometric tables, feature dictionaries, response templates, and conditional response/response-time matrices.
- Added a complete plot suite, canonical export, source fingerprint, workflow specification, rerun script, session information, Markdown/HTML report, and workflow-result object.
- Added workflow validation, documentation, a dedicated vignette, Windows runner script, and regression tests with and without observed responses.
- The workflow never fabricates responses, scores, IRT estimates, or psychological interpretations.

# eyeprocess 0.2.0.9002

- Renamed the packaged Gazepoint Analysis 7.2.0 regression fixtures to portable, space-free filenames.
- Updated all fixture references while preserving filename-based inference of the canonical participant identity `User 3`.
- Added regression coverage requiring portable packaged fixture paths.
- No importer, canonical schema, biometric mapping, or empirical-validation behavior changed.

# eyeprocess 0.2.0.9001

- Preserved the successful real Gazepoint Analysis 7.2.0 corpus validation while aligning legacy synthetic tests with current channel semantics.
- Treats the legacy `GSR` fixture field as `gsr_raw`; processed conductance remains represented by `GSR_US`/`EDA` columns.
- Added an explicit `recording_id` argument to folder-level Gazepoint import and forwards it safely for single-recording folders.
- Rejects a single `recording_id` override for multi-recording folders rather than creating duplicate canonical identifiers.
- Added regression coverage for direct and generic folder imports with recording-ID overrides.

# eyeprocess 0.2.0.9000

* Added empirical support for Gazepoint Analysis 7.2.0 `User *_all_gaze.csv`,
  `User *_fixations.csv`, and multi-section `Data_Summary_export_*.csv` files.
* Added filename-based participant and recording identity inference for paired
  Gazepoint exports whose `USER` field is blank.
* Added `TIMETICK(f=...)` normalization to zero-based recording seconds while
  retaining native ticks, media-relative time, source media identifiers, AOI
  labels, saccade measures, and video-frame values.
* Namespaced vendor fixation identifiers by media because Gazepoint restarts
  `FPOGID` for each media item.
* Added explicit Gazepoint biometric channels and validity handling for raw GSR,
  conductance in microsiemens, tonic/phasic components, heart rate, IBI, and
  engagement dial values.
* Added `read_gazepoint_summary()` and conversion of Gazepoint Data Summary
  reports into AOI definitions and participant-AOI feature records.
* Added paired-folder import, real-structure fixtures, regression tests, and a
  private six-recording empirical validation corpus.

# eyeprocess 0.1.0.9003

* Updated the validation-corpus regression test to match the intentional,
  idempotent behavior of `init_validation_corpus()`. Repeated initialization now
  verifies preservation of the manifest, README, and existing case files rather
  than expecting an obsolete non-empty-directory error.
* No package implementation or analysis behavior changed in this patch.

# eyeprocess 0.1.0.9002

* Removed repeated `write.csv()` `qmethod` warnings from canonical-folder
  serialization on Windows.
* Added explicit canonical units for imported biometric channels, including
  heart rate, interbeat intervals, electrodermal activity, and engagement-dial
  streams; unknown channels retain the explicit unit `vendor_units`.
* Made `init_validation_corpus()` idempotent and non-destructive. Re-running it
  now preserves existing manifests and case files unless `overwrite = TRUE`.
* Added regression tests for warning-free canonical serialization, populated
  biometric units, and safe repeated corpus initialization.

# eyeprocess 0.1.0.9001

* Corrected binocular eye-sample validation to use the composite key
  `recording_id + sample_id + eye`, eliminating false duplicate-key errors.
* Added versioned canonical-folder serialization with an explicit missing-value
  token, preserving the distinction between `NA` and genuine empty text while
  retaining compatibility with folders written by earlier development builds.
* Prevented Pupil Labs and EyeLink detectors from probing proprietary binary
  files as delimited text and suppressed detector-level parsing warnings.
* Made `validate_real_exports.R` exit cleanly with instructions when an
  initialized corpus has no cases, rather than raising an error.
* Added regression tests for binocular keys, missing-versus-empty round trips,
  and warning-free expected rejection of SMI IDF input.

* Added reproducible empirical export validation for individual files, folders,
  and initialized, versioned multi-vendor corpora.
* Added declared-versus-fixture-versus-empirical compatibility profiles.
* Added source manifests with file hashes, field inventories, delimiter and
  format-detection evidence.
* Added canonical schema-coverage and source-preservation audits.
* Added deterministic dataset fingerprints, canonical round-trip comparison,
  and table-level compatibility evidence.
* Added linked-identifier anonymization and shareable validation bundles that
  exclude raw vendor data, source paths, original filenames, and file hashes by
  default; human disclosure review remains mandatory.
* Added dedicated source validators for Tobii, Pupil Labs, EyeLink, SMI, and
  generic mapped exports, so all built-in adapters expose a validation hook.
* Added unique case identifiers, manifest-relative path resolution, exact
  format-family evidence assignment, a real-export validation vignette,
  manifest template, plots, reports, and regression tests.

# eyeprocess 0.0.0.9004

* Added `eyeprocess-package` to a dedicated Package overview section in the
  pkgdown reference index, resolving the final `pkgdown::check_pkgdown()`
  configuration error.
* No analytical, import, schema, plotting, or modelling behaviour changed.

# eyeprocess 0.0.0.9003

* Fixed the Windows installer version check by removing `read.dcf()` field-name
  attributes and comparing normalized character scalars. This eliminates a false
  mismatch when the installed and source versions are identical.
* Added the canonical pkgdown site URL to `DESCRIPTION` while retaining the
  GitHub repository URL, satisfying `pkgdown::check_pkgdown()`.
* Retained all 0.0.0.9002 package-check and runtime fixes.

# eyeprocess 0.0.0.9002

* Removed stale `hello()` scaffold artefacts during local installation and validation.
* Replaced the unexported `brms::gaussian()` reference with `stats::gaussian()`.
* Removed non-standard evaluation from stream construction and Gazepoint biometric matching.
* Excluded the repository-level `CITATION.cff` from built source packages while retaining `inst/CITATION`.
* Hardened the Windows installer against reinstalling a loaded namespace and against false version-success messages.

# eyeprocess 0.0.0.9001

* Fixed type-preserving row binding when canonical tables contain zero rows.
* Added stable identifiers to the interpretive-warning registry.
* Removed all-missing extrema warnings from fixation and saccade preprocessing.

# eyeprocess 0.0.0.9000

* Introduces the canonical `eye_dataset` relational object.
* Adds generic CSV/TSV mapping and an extensible adapter registry.
* Adds first-class Gazepoint Analysis and Gazepoint Biometrics importers.
* Adds dedicated Tobii Pro Lab, Pupil Labs Neon/Core, EyeLink ASC/report, and
  SMI BeGaze text adapters.
* Adds explicit clock and coordinate-space management.
* Adds trial reconstruction, AOI registration and assignment, ocular-event
  derivation, pupil preprocessing, quality audits, feature extraction, plots,
  simulations, reporting, and optional psychometric modelling interfaces.
