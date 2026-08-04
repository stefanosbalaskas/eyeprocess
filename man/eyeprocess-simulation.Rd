\name{eyeprocess-simulation}
\alias{simulate_eye_dataset}
\alias{simulate_process_irt}
\alias{parameter_recovery}
\alias{summary.eye_parameter_recovery}
\alias{plot.eye_parameter_recovery}
\alias{power_process_simulation}
\title{Simulate and validate eyeprocess workflows}
\description{Simulate canonical multimodal datasets and conduct parameter-recovery or power experiments.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
