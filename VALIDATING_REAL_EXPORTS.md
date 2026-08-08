# Validating real eye-tracking exports

`eyeprocess` 0.2.0.9002 distinguishes implemented support from evidence
of compatibility with real exports. The package contains synthetic
fixtures, but real device/software combinations must be validated
separately.

## Recommended corpus layout

Initialize a corpus outside the package source tree:

``` r

library(eyeprocess)
init_validation_corpus(
  "path/to/eyeprocess-validation-corpus"
)
```

The initializer is idempotent: running it again preserves an existing
manifest and all case files unless `overwrite = TRUE` is requested
explicitly. The resulting layout can be expanded as follows:

``` text
path/to/eyeprocess-validation-corpus/
  validation-manifest.csv
  gp-analysis-7-2/
  gp-biometrics-7-2/
  tobii-prolab-1/
  pupil-neon-2026/
  eyelink-asc/
```

Each directory should contain one de-identified export case. Keep
proprietary binary files only when their local converter is available.
Do not commit the corpus to a public repository.

Edit the generated `validation-manifest.csv` and add one unique row per
case. The `vendor` field must use an adapter name shown by
[`supported_eye_formats()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-adapters.md).

## Batch validation

Edit the paths at the top of `validate_real_exports.R`, then run:

``` r

source("path/to/eyeprocess/validate_real_exports.R")
```

The script writes:

- a corpus summary CSV;
- a Markdown validation report;
- the compatibility matrix;
- an RDS object containing all validation evidence.

## Acceptance rules

A case is not production validated merely because it imports. Review:

1.  format-detection confidence;
2.  adapter-specific warnings;
3.  canonical validation errors;
4.  critical schema coverage;
5.  native and normalized timestamps;
6.  coordinate-space and unit declarations;
7.  pupil and biometric channel units;
8.  trial, event, and AOI reconstruction;
9.  vendor-derived versus recomputed ocular events;
10. canonical export/re-import fidelity.

Use
[`create_validation_bundle()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md)
only after reviewing anonymization. Bundles exclude raw exports but
automated de-identification cannot recognize every study-specific
free-text disclosure.

## An initialized but empty corpus

[`init_validation_corpus()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md)
deliberately creates an empty manifest. This is not an error: no
empirical compatibility claim can be made until a de-identified real
export is added. Running `validate_real_exports.R` with an empty
manifest now prints actionable instructions and returns without creating
output files.

Add one source file or folder per case and one manifest row per case
before running validation. Use
[`supported_eye_formats()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-adapters.md)
for adapter names and
[`eye_format_profiles()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-format-validation.md)
for `format_family` identifiers.

## Gazepoint Analysis 7.2.0 corpus case

The project validation corpus may contain one folder case with multiple
paired recordings. For the current development case, the manifest path
is relative:

``` text
cases/gazepoint-analysis-v7.2.0-demo
```

The folder contains six `User *_all_gaze.csv` files, six paired
`User *_fixations.csv` files, and four `Data_Summary_export_*.csv`
reports. Version 0.2.0.9002 identifies each `User N` pair as one
recording, uses `TIMETICK(f=10000000)` for recording-level order,
retains media-relative time, and imports the Data Summary sections as
AOI definitions and summary features.
