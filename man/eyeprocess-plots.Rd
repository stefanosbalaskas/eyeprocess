\name{eyeprocess-plots}
\alias{plot.eye_dataset}
\alias{plot_eye_overview}
\alias{plot_eye_trace}
\alias{plot_fixations}
\alias{plot_scanpath}
\alias{plot_gaze_heatmap}
\alias{plot_aoi_dwell}
\alias{plot_transition_matrix}
\alias{plot_pupil_timeseries}
\alias{plot_biometrics}
\alias{plot_signal_quality}
\alias{plot_sampling_rate}
\alias{plot_missingness}
\alias{plot_trial_timeline}
\alias{plot_feature_distribution}
\alias{plot_feature_correlation}
\alias{plot_coordinate_spaces}
\alias{plot_clock_alignment}
\alias{plot_item_difficulty}
\alias{plot_model_diagnostics}
\title{Visualize eye-tracking and multimodal process data}
\description{Base-R plots for traces, fixations, scanpaths, heatmaps, AOIs, pupil, biometrics, quality, timing, and models.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
