# eyeprocess 0.1.0.9001 validation hotfix

This development hotfix addresses the failures observed during the first
Windows runtime validation of the empirical export-validation milestone.

## Corrections

1. **Binocular primary keys**
   `eye_samples` now uses the composite primary key
   `recording_id + sample_id + eye`. Left- and right-eye rows sharing a native
   sample identifier are no longer incorrectly reported as duplicates.

2. **Loss-aware canonical folders**
   Canonical folder export now writes versioned serialization metadata and an
   explicit missing-value token. Re-import therefore preserves the distinction
   between `NA` and genuine empty character values. Legacy folders written by
   earlier development versions remain readable.

3. **Binary source detection**
   Pupil Labs and EyeLink detectors no longer attempt delimited parsing of
   unsupported binary file extensions. Adapter detection is warning-contained,
   eliminating the repeated `readTableHeader` warnings from expected SMI IDF
   rejection tests.

4. **Empty empirical corpus workflow**
   `validate_real_exports.R` now returns cleanly with actionable instructions
   when the corpus manifest contains no cases. An initialized corpus is a safe
   empty template, not empirical evidence.

5. **Overwrite safety**
   Canonical-folder overwrite removes package-managed stale artifacts such as
   `raw.rds` and old manifests before writing the replacement dataset.

## Validation status

The source received static lexical, metadata, namespace, YAML, and whitespace
checks in the generation environment. A complete R runtime gate remains to be
run on Windows using `validate_eyeprocess.R`.
