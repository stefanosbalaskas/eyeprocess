# Validate real eye-tracking exports and compatibility corpora

Build reproducible evidence for source-format compatibility. The
validation framework distinguishes declared support, synthetic-fixture
testing, and empirical validation with real exports. It audits source
structure, adapter detection, canonical import, field coverage, native
timestamp preservation, coordinate semantics, provenance, and canonical
round-trip fidelity.

## Details

`validate_eye_source()` validates one file or folder. Use
`validation_manifest()` and `validate_eye_corpus()` to evaluate a
versioned collection of exports from multiple vendors, devices, and
software versions. `init_validation_corpus()` is idempotent and
preserves an existing manifest and case files unless `overwrite = TRUE`
is supplied. A source can be retained temporarily in the result for
creation of an anonymized validation bundle; raw vendor data are never
included in bundles by default.

`anonymize_eye_dataset()` replaces participant, recording, and session
identifiers, removes raw tables and source paths by default, and records
the operation in provenance. Automated anonymization cannot guarantee
removal of all study-specific free text, so exported bundles still
require human review.

## Value

Depending on the function, an `eye_format_validation_spec`, source-file
manifest, schema-coverage table, source-preservation audit, canonical
round-trip result, `eye_format_validation`, `eye_corpus_validation`,
anonymized `eye_dataset`, report path, or validation-bundle path.

## See also

[`read_eye_export()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-adapters.md),
[`validate_eye_dataset()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-class.md),
[`supported_eye_formats()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-adapters.md),
[`write_eye_dataset()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-export-report.md),
[`provenance_manifest()`](https://stefanosbalaskas.github.io/eyeprocess/reference/eyeprocess-class.md)
