# eyeprocess 0.1.0.9003 empirical-format validation milestone

## Purpose

This milestone converts vendor-support claims into reviewable evidence.
It does not declare any adapter production-compatible merely because its
parser exists or passes a synthetic fixture.

## New capabilities

- Twelve declared export-format profiles covering Gazepoint, Gazepoint
  Biometrics, Tobii Pro Lab, Pupil Labs Neon/Core, EyeLink ASC/Data
  Viewer/EDF, SMI BeGaze text, and mapped generic files.
- File and folder inspection with hashes, delimiters, fields,
  readability, and adapter-detection evidence.
- Single-source and multi-case corpus validation.
- Per-case requirements for expected import, gaze, native timestamps,
  coordinate spaces, provenance, raw retention, and canonical round
  trips.
- Canonical schema-coverage and source-preservation audits.
- Tolerance-aware, key-aligned canonical export/re-import comparison.
- Dedicated source validators for every built-in adapter.
- Exact format-family compatibility accounting without duplicated cases.
- Unique manifest case identifiers, explicit logical parsing, and paths
  resolved relative to the manifest location.
- Private validation-corpus initialization and batch-validation scripts.
- Linked-identifier anonymization across all canonical tables.
- Conservative validation bundles that omit raw exports, local paths,
  original filenames, hashes, free-text values, and vendor metadata by
  default.
- Markdown reports, evidence CSVs, compatibility matrices, plots, tests,
  and a real-export validation vignette.

## Evidence boundary

The package contains synthetic fixtures only. Real compatibility
requires multiple de-identified exports for each device, software
version, and export configuration. Every warning and every anonymized
bundle must receive human review.

## Runtime gate

The preceding version, 0.0.0.9004, passed installation, all unit tests,
`R CMD check` with 0 errors, 0 warnings, and 0 notes,
[`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html),
and runtime smoke tests on Windows 11 with R 4.6.1.

Version 0.1.0.9003 must pass the same Windows validation script before
this milestone is considered closed.
