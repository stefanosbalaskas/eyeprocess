\name{eyeprocess-experimental}
\alias{process_irt_spec}
\alias{print.process_irt_spec}
\alias{fit_process_irt}
\alias{fit_gaze_informed_irt}
\alias{fit_pupil_informed_irt}
\alias{fit_multimodal_irt}
\alias{process_irt_diagnostics}
\alias{functional_pupil_features}
\alias{fit_strategy_mixture}
\alias{print.eye_strategy_mixture}
\alias{estimate_ez_diffusion}
\alias{fit_gaze_weighted_choice}
\alias{model_missing_process}
\alias{sensitivity_missing_process}
\alias{print.eye_missing_sensitivity}
\title{Experimental psychometric process models}
\description{Experimental interfaces for multimodal IRT, functional pupil summaries, strategy mixtures, diffusion summaries, and missing-process sensitivity.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
