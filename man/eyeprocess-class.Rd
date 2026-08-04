\name{eyeprocess-class}
\alias{new_eye_dataset}
\alias{is_eye_dataset}
\alias{as_eye_dataset}
\alias{as_eye_dataset.eye_dataset}
\alias{as_eye_dataset.data.frame}
\alias{as_eye_dataset.default}
\alias{print.eye_dataset}
\alias{summary.eye_dataset}
\alias{print.summary.eye_dataset}
\alias{validate_eye_dataset}
\alias{print.eye_validation}
\alias{get_eye_table}
\alias{set_eye_table}
\alias{append_eye_table}
\alias{add_provenance}
\alias{provenance_manifest}
\alias{compact_eye_dataset}
\title{Create and manage eyeprocess datasets}
\description{Construct, inspect, validate, modify, and summarize canonical eye-tracking datasets.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
