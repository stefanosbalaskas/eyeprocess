\name{eyeprocess-export-report}
\alias{write_eye_dataset}
\alias{read_eye_dataset}
\alias{export_canonical}
\alias{import_canonical}
\alias{write_provenance}
\alias{report_eye_dataset}
\alias{report_processirt}
\alias{as_eye_dataset.gp3_recording}
\alias{as_eye_dataset.gp3_analysis}
\alias{as_eye_dataset.gp3_data}
\alias{as_eye_dataset.gpbiometrics_data}
\alias{as_eye_biometrics}
\alias{as_eye_biometrics.eye_dataset}
\alias{as_eye_biometrics.data.frame}
\title{Export, reporting, and package bridges}
\description{Persist canonical datasets, produce provenance-aware reports, and bridge gp3tools or gpbiometrics objects.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
