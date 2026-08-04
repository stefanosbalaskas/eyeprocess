\name{eyeprocess-import-gazepoint}
\alias{is_gazepoint_export}
\alias{gp_identify_export_type}
\alias{gp_profile_export}
\alias{gp_list_export_fields}
\alias{gp_validate_export}
\alias{read_gazepoint}
\alias{read_gazepoint_gaze}
\alias{read_gazepoint_biometrics}
\alias{read_gazepoint_fixations}
\alias{read_gazepoint_events}
\alias{read_gazepoint_combined}
\alias{read_gazepoint_folder}
\alias{gp_pair_exports}
\alias{gp_match_recordings}
\alias{gp_match_biometrics}
\alias{gp_audit_file_pairs}
\alias{read_gazepoint_aoi_statistics}
\alias{gp_parse_user_events}
\alias{gp_parse_media_events}
\alias{gp_reconstruct_trials}
\alias{gp_reconstruct_stimuli}
\alias{gp_align_media_ids}
\alias{gp_parse_markers}
\alias{gp_check_sampling_rate}
\alias{gp_check_validity_fields}
\alias{gp_check_fixation_ids}
\alias{gp_check_media_timing}
\alias{gp_check_pupil_channels}
\alias{gp_check_biometrics_sync}
\title{Import Gazepoint and Gazepoint Biometrics exports}
\description{Detect, profile, import, pair, validate, and reconstruct Gazepoint Analysis and Biometrics exports.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
