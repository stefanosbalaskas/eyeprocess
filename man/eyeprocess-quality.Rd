\name{eyeprocess-quality}
\alias{store_quality}
\alias{audit_sampling_rate}
\alias{audit_signal_quality}
\alias{audit_pupil_quality}
\alias{audit_episodes}
\alias{audit_event_order}
\alias{audit_trial_coverage}
\alias{audit_aois}
\alias{audit_missingness}
\alias{check_process_leakage}
\alias{check_feature_level}
\alias{interpretive_warnings}
\alias{analysis_readiness}
\alias{print.eye_readiness}
\alias{compare_preprocessing}
\alias{compare_aoi_definitions}
\alias{sensitivity_process}
\alias{print.eye_sensitivity}
\title{Quality control, sensitivity, and governance}
\description{Audit data quality, flag process leakage, assess readiness, and compare preprocessing or AOI specifications.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
