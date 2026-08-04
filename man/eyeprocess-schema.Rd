\name{eyeprocess-schema}
\alias{eye_schema}
\alias{schema_table}
\alias{empty_eye_table}
\alias{standardize_eye_table}
\alias{validate_eye_table}
\alias{canonical_table_names}
\alias{new_coordinate_space}
\title{Canonical schemas and coordinate spaces}
\description{Define, inspect, standardize, and validate the relational tables used by eyeprocess.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
