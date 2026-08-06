\name{functional_pupil_irt_spec}
\alias{functional_pupil_irt_spec}
\title{Specify a functional pupil-IRT model}
\usage{
functional_pupil_irt_spec(df = 6L, basis = c("natural_spline", "bspline"),
  response = "score", engine = c("two_stage_glm", "two_stage_lme4", "brms", "stan"),
  alignment = c("trial", "event"), event_time_column = NULL, latency_ms = 200,
  baseline_window = c(-200, 0), baseline_method = c("subtract", "percent", "zscore"),
  min_baseline_samples = 3L, drop_invalid_baseline = TRUE, time_window = NULL,
  pupil_column = NULL, time_column = NULL, participant_column = "participant_id",
  item_column = "item_id", trial_column = "trial_id", luminance_column = NULL,
  gaze_x_column = NULL, gaze_y_column = NULL, blink_column = NULL,
  interpolated_column = NULL, max_interpolated_fraction = 0.20,
  nuisance_by_participant = FALSE, include_response_time = TRUE, ar1 = TRUE,
  participant_effect = TRUE, item_effect = TRUE, chains = 4L,
  parallel_chains = chains, iter_warmup = 1000L, iter_sampling = 1000L,
  adapt_delta = 0.95, max_treedepth = 12L)
}
\arguments{
  \item{df}{Basis degrees of freedom.}
  \item{basis}{Natural spline or B-spline basis.}
  \item{response}{Response field.}
  \item{engine}{Two-stage baseline, multilevel baseline, brms bridge, or}
  \item{alignment}{Trial- or event-aligned time.}
  \item{event_time_column}{Event timestamp column required for event alignment.}
  \item{latency_ms}{Physiological latency shift applied before basis creation.}
  \item{baseline_window}{Numeric time window used for baseline correction.}
  \item{baseline_method}{Subtract, percent change, or z score.}
  \item{min_baseline_samples}{Minimum finite baseline samples per trial.}
  \item{drop_invalid_baseline}{Whether to exclude trials with invalid baselines.}
  \item{time_window}{Optional analysis time window.}
  \item{pupil_column}{Column names.}
  \item{time_column}{Column names.}
  \item{participant_column}{Column names.}
  \item{item_column}{Column names.}
  \item{trial_column}{Column names.}
  \item{luminance_column}{Optional nuisance columns.}
  \item{gaze_x_column}{Optional nuisance columns.}
  \item{gaze_y_column}{Optional nuisance columns.}
  \item{blink_column}{Optional quality columns.}
  \item{interpolated_column}{Optional quality columns.}
  \item{max_interpolated_fraction}{Maximum interpolated fraction per trial.}
  \item{nuisance_by_participant}{Whether nuisance residualization includes participant fixed effects.}
  \item{include_response_time}{Include response time in supported joint bridges.}
  \item{ar1}{Include trial-wise AR(1) residual structure in Stan.}
  \item{participant_effect}{Include participant/item pupil effects.}
  \item{item_effect}{Include participant/item pupil effects.}
  \item{chains}{CmdStan controls.}
  \item{parallel_chains}{CmdStan controls.}
  \item{iter_warmup}{CmdStan controls.}
  \item{iter_sampling}{CmdStan controls.}
  \item{adapt_delta}{CmdStan sampler controls.}
  \item{max_treedepth}{CmdStan sampler controls.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
