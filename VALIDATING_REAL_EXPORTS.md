# Validating real eye-tracking exports

`eyeprocess` 0.1.0.9003 distinguishes implemented support from evidence of
compatibility with real exports. The package contains synthetic fixtures, but
real device/software combinations must be validated separately.

## Recommended corpus layout

Initialize a corpus outside the package source tree:

```r
library(eyeprocess)
init_validation_corpus(
  "C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess-validation-corpus"
)
```

The initializer is idempotent: running it again preserves an existing manifest
and all case files unless `overwrite = TRUE` is requested explicitly. The
resulting layout can be expanded as follows:

```text
C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess-validation-corpus/
  validation-manifest.csv
  gp-analysis-7-2/
  gp-biometrics-7-2/
  tobii-prolab-1/
  pupil-neon-2026/
  eyelink-asc/
```

Each directory should contain one de-identified export case. Keep proprietary
binary files only when their local converter is available. Do not commit the
corpus to a public repository.

Edit the generated `validation-manifest.csv` and add one unique row per case. The
`vendor` field must use an adapter name shown by `supported_eye_formats()`.

## Batch validation

Edit the paths at the top of `validate_real_exports.R`, then run:

```r
source("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess/validate_real_exports.R")
```

The script writes:

- a corpus summary CSV;
- a Markdown validation report;
- the compatibility matrix;
- an RDS object containing all validation evidence.

## Acceptance rules

A case is not production validated merely because it imports. Review:

1. format-detection confidence;
2. adapter-specific warnings;
3. canonical validation errors;
4. critical schema coverage;
5. native and normalized timestamps;
6. coordinate-space and unit declarations;
7. pupil and biometric channel units;
8. trial, event, and AOI reconstruction;
9. vendor-derived versus recomputed ocular events;
10. canonical export/re-import fidelity.

Use `create_validation_bundle()` only after reviewing anonymization. Bundles
exclude raw exports but automated de-identification cannot recognize every
study-specific free-text disclosure.

## An initialized but empty corpus

`init_validation_corpus()` deliberately creates an empty manifest. This is not
an error: no empirical compatibility claim can be made until a de-identified
real export is added. Running `validate_real_exports.R` with an empty manifest
now prints actionable instructions and returns without creating output files.

Add one source file or folder per case and one manifest row per case before
running validation. Use `supported_eye_formats()` for adapter names and
`eye_format_profiles()` for `format_family` identifiers.

