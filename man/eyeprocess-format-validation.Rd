\name{eyeprocess-format-validation}
\alias{format_validation_spec}
\alias{eye_format_profiles}
\alias{format_compatibility_matrix}
\alias{inspect_eye_source}
\alias{schema_coverage}
\alias{schema_coverage_summary}
\alias{source_preservation_audit}
\alias{fingerprint_eye_dataset}
\alias{compare_eye_datasets}
\alias{roundtrip_eye_dataset}
\alias{validate_eye_source}
\alias{validation_manifest}
\alias{write_validation_manifest}
\alias{read_validation_manifest}
\alias{discover_validation_cases}
\alias{init_validation_corpus}
\alias{validate_eye_corpus}
\alias{anonymize_eye_dataset}
\alias{write_format_validation_report}
\alias{create_validation_bundle}
\alias{validate_tobii_export}
\alias{validate_pupillabs_export}
\alias{validate_eyelink_export}
\alias{validate_smi_export}
\alias{validate_generic_export}
\alias{print.eye_format_validation_spec}
\alias{print.eye_roundtrip_validation}
\alias{print.eye_format_validation}
\alias{summary.eye_format_validation}
\alias{plot.eye_format_validation}
\alias{print.eye_corpus_validation}
\alias{plot.eye_corpus_validation}
\title{Validate real eye-tracking exports and compatibility corpora}
\description{
Build reproducible evidence for source-format compatibility. The validation
framework distinguishes declared support, synthetic-fixture testing, and
empirical validation with real exports. It audits source structure, adapter
detection, canonical import, field coverage, native timestamp preservation,
coordinate semantics, provenance, and canonical round-trip fidelity.
}
\details{
\code{validate_eye_source()} validates one file or folder. Use
\code{validation_manifest()} and \code{validate_eye_corpus()} to evaluate a
versioned collection of exports from multiple vendors, devices, and software
versions. \code{init_validation_corpus()} is idempotent and preserves an existing
manifest and case files unless \code{overwrite = TRUE} is supplied. A source
can be retained temporarily in the result for creation of an anonymized
validation bundle; raw vendor data are never included in bundles by default.

\code{anonymize_eye_dataset()} replaces participant, recording, and session
identifiers, removes raw tables and source paths by default, and records the
operation in provenance. Automated anonymization cannot guarantee removal of
all study-specific free text, so exported bundles still require human review.
}
\value{
Depending on the function, an \code{eye_format_validation_spec}, source-file
manifest, schema-coverage table, source-preservation audit, canonical
round-trip result, \code{eye_format_validation},
\code{eye_corpus_validation}, anonymized \code{eye_dataset}, report path, or
validation-bundle path.
}
\seealso{
\code{read_eye_export()}, \code{validate_eye_dataset()},
\code{supported_eye_formats()}, \code{write_eye_dataset()},
\code{provenance_manifest()}
}
\keyword{data}
\keyword{methods}
