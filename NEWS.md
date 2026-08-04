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
