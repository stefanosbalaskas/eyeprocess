\name{eyeprocess-features}
\alias{feature_spec}
\alias{print.eye_feature_spec}
\alias{trial_table}
\alias{summarize_fixations}
\alias{scanpath_sequence}
\alias{transition_matrix}
\alias{gaze_entropy}
\alias{transition_entropy}
\alias{derive_gaze_features}
\alias{derive_pupil_features}
\alias{derive_rt_features}
\alias{derive_biometric_features}
\alias{derive_all_features}
\alias{features_wide}
\alias{feature_dictionary}
\title{Derive gaze, pupil, response-time, and biometric features}
\description{Generate declared person-item-trial features, scanpaths, transitions, entropy measures, and feature dictionaries.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
