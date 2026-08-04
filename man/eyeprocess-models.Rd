\name{eyeprocess-models}
\alias{response_matrix}
\alias{response_time_matrix}
\alias{align_response_matrices}
\alias{model_data}
\alias{print.eyeprocess_model}
\alias{summary.eyeprocess_model}
\alias{print.summary.eyeprocess_model}
\alias{fit_irt}
\alias{fit_explanatory_irt}
\alias{fit_accuracy_rt}
\alias{print.eye_two_stage_rt}
\alias{fit_dif}
\alias{fit_shared_process_factor}
\alias{item_parameters}
\alias{person_scores}
\alias{model_fit_statistics}
\alias{predict.eyeprocess_model}
\alias{check_local_dependence}
\alias{fit_joint_process_model}
\alias{fit_dynamic_aoi_model}
\alias{print.eye_dynamic_aoi}
\title{Psychometric and process-data models}
\description{Prepare response matrices and fit optional IRT, explanatory, response-time, DIF, shared-process, and dynamic models.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
