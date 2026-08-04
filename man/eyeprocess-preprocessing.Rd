\name{eyeprocess-preprocessing}
\alias{preprocess_spec}
\alias{print.eye_preprocess_spec}
\alias{rolling_apply}
\alias{filter_gaze}
\alias{flag_gaze_outliers}
\alias{gaze_velocity}
\alias{interpolate_pupil}
\alias{filter_pupil}
\alias{baseline_pupil}
\alias{pupil_deconvolve}
\alias{detect_blinks}
\alias{detect_fixations_ivt}
\alias{detect_fixations_idt}
\alias{detect_saccades}
\alias{preprocess_eye}
\title{Gaze and pupil preprocessing}
\description{Declare preprocessing, filter signals, interpolate pupil gaps, detect ocular events, and retain a transformation record.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
