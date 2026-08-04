\name{eyeprocess-trials-aoi}
\alias{build_trials}
\alias{build_stimulus_intervals}
\alias{assign_trials}
\alias{add_responses}
\alias{build_item_responses}
\alias{new_aoi}
\alias{print.eye_aoi}
\alias{register_aois}
\alias{assign_aois}
\alias{build_aoi_visits}
\title{Trials, responses, stimuli, and areas of interest}
\description{Reconstruct trial intervals, attach responses, register static or dynamic AOIs, and derive AOI visits.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
