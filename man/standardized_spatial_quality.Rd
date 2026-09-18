\name{standardized_spatial_quality}
\alias{standardized_spatial_quality}
\alias{validate_gaze_quality_inputs}
\alias{compute_gaze_accuracy}
\alias{compute_rms_s2s}
\alias{compute_gaze_sd_precision}
\alias{compute_bcea}
\alias{compute_gaze_precision}
\alias{estimate_sampling_interval}
\alias{estimate_sampling_jitter}
\alias{estimate_effective_sampling_rate}
\alias{compute_valid_sample_fraction}
\alias{compute_gaze_data_loss}
\alias{summarise_spatial_quality}
\alias{summarise_sampling_quality}
\alias{create_gaze_quality_report}
\alias{compare_gaze_quality_sessions}
\alias{compare_gaze_quality_conditions}
\alias{plot_gaze_accuracy}
\alias{plot_gaze_precision}
\alias{plot_bcea}
\alias{plot_sampling_intervals}
\alias{plot_gaze_quality_dashboard}
\alias{report_gaze_quality}
\alias{simulate_gaze_quality_calibration}
\title{Standardized vendor-neutral gaze data-quality metrics}
\description{
Compute and report separable eye-tracking quality dimensions: target-referenced
accuracy, RMS sample-to-sample precision, spatial standard-deviation precision,
BCEA, effective sampling behavior, sampling jitter, valid-data fraction, and
data loss. User thresholds create review flags only and never trigger automatic
exclusion.
}
\usage{
validate_gaze_quality_inputs(data, x = "gaze_x", y = "gaze_y", time = NULL,
  target_x = NULL, target_y = NULL, by = NULL, unit = "degrees",
  time_unit = "ms", unit_column = NULL)

compute_gaze_accuracy(data, x = "gaze_x", y = "gaze_y",
  target_x = "target_x", target_y = "target_y", by = NULL,
  unit = "degrees", output_unit = NULL, geometry = NULL)

compute_rms_s2s(data, x = "gaze_x", y = "gaze_y", time = NULL, by = NULL,
  unit = "degrees", output_unit = NULL, geometry = NULL,
  dimension = c("2d", "horizontal", "vertical"), time_unit = "ms",
  max_gap_ms = NULL)

compute_gaze_sd_precision(data, x = "gaze_x", y = "gaze_y", by = NULL,
  unit = "degrees", output_unit = NULL, geometry = NULL)

compute_bcea(data, x = "gaze_x", y = "gaze_y", by = NULL,
  probability = 0.68, unit = "degrees", output_unit = NULL,
  geometry = NULL)

compute_gaze_precision(data, x = "gaze_x", y = "gaze_y", time = NULL,
  by = NULL, probability = 0.68, unit = "degrees", output_unit = NULL,
  geometry = NULL, dimension = "2d", time_unit = "ms", max_gap_ms = NULL)

estimate_sampling_interval(data, time = "timestamp_ms", by = NULL,
  time_unit = "ms")

estimate_sampling_jitter(data, time = "timestamp_ms", by = NULL,
  time_unit = "ms")

estimate_effective_sampling_rate(data, time = "timestamp_ms", by = NULL,
  time_unit = "ms", nominal_sampling_hz = NULL,
  dropped_interval_factor = 1.5, x = NULL, y = NULL, valid = NULL)

compute_valid_sample_fraction(data, x = "gaze_x", y = "gaze_y",
  valid = NULL, by = NULL)

compute_gaze_data_loss(data, x = "gaze_x", y = "gaze_y",
  time = "timestamp_ms", valid = NULL, missing_reason = NULL,
  by = NULL, time_unit = "ms")

summarise_spatial_quality(data, x = "gaze_x", y = "gaze_y",
  target_x = "target_x", target_y = "target_y", time = NULL, by = NULL,
  unit = "degrees", output_unit = NULL, geometry = NULL,
  dimension = "2d", time_unit = "ms", max_gap_ms = NULL,
  probability = 0.68)

summarise_sampling_quality(data, time = "timestamp_ms", by = NULL,
  time_unit = "ms", nominal_sampling_hz = NULL,
  dropped_interval_factor = 1.5, x = NULL, y = NULL, valid = NULL)

create_gaze_quality_report(data, x = "gaze_x", y = "gaze_y",
  time = "timestamp_ms", target_x = "target_x", target_y = "target_y",
  valid = NULL, missing_reason = NULL, by = NULL, unit = "degrees",
  output_unit = NULL, geometry = NULL, time_unit = "ms",
  nominal_sampling_hz = NULL, bcea_probability = 0.68,
  max_gap_ms = NULL, thresholds = NULL, preprocessing_spec = NULL,
  event_detector = NULL, aoi_specification = NULL, quality_rules = NULL,
  model_specification = NULL, software_version = NULL,
  unit_column = NULL)

compare_gaze_quality_sessions(data, session = "session_id", by = NULL, ...)

compare_gaze_quality_conditions(data, condition = "condition", by = NULL, ...)

plot_gaze_accuracy(report, metric = "accuracy_mean", ...)
plot_gaze_precision(report, metric = "precision_rms_s2s", ...)
plot_bcea(report, ...)
plot_sampling_intervals(data, time = "timestamp_ms", time_unit = "ms", ...)
plot_gaze_quality_dashboard(report, ...)
report_gaze_quality(report, digits = 3L)

simulate_gaze_quality_calibration(seed = 20260918L,
  samples_per_target = 18L, nominal_sampling_hz = 60)
}
\arguments{
  \item{data}{A data frame or object coercible to a data frame.}
  \item{x, y}{Columns containing horizontal and vertical gaze coordinates.}
  \item{time}{Timestamp column.}
  \item{target_x, target_y}{Known validation-target coordinate columns.}
  \item{by}{Optional grouping columns defining the quality-analysis unit.}
  \item{unit}{Input coordinate unit: \code{"degrees"}, \code{"pixels"}, or \code{"normalized"}.}
  \item{output_unit}{Optional explicit output coordinate unit. No conversion occurs when omitted.}
  \item{geometry}{Named screen/viewing geometry used only for explicit coordinate conversion.}
  \item{time_unit}{Timestamp unit: seconds, milliseconds, microseconds, or nanoseconds.}
  \item{unit_column}{Optional column used to reject mixed or conflicting coordinate units. For the canonical report, an existing \code{coordinate_unit} column is detected automatically when this argument is omitted.}
  \item{dimension}{RMS-S2S dimension: two-dimensional, horizontal, or vertical.}
  \item{max_gap_ms}{Optional explicit maximum interval permitted for an adjacent-sample RMS pair.}
  \item{probability, bcea_probability}{Probability region used for BCEA.}
  \item{nominal_sampling_hz}{Optional nominal hardware rate used for comparison and dropped-interval diagnostics.}
  \item{dropped_interval_factor}{Multiple of the nominal interval used to count long intervals.}
  \item{valid}{Optional explicit validity column. Missing gaze coordinates remain unavailable regardless of this flag.}
  \item{missing_reason}{Optional loss-reason column such as blink, tracker invalidity, or off-screen.}
  \item{thresholds}{Optional named study-specific review thresholds; these never exclude data automatically.}
  \item{preprocessing_spec, event_detector, aoi_specification, quality_rules, model_specification, software_version}{Optional provenance fields preserved on the canonical report.}
  \item{session, condition}{Grouping column used for convenience comparisons.}
  \item{report}{A quality-report-like data frame.}
  \item{metric}{Metric column plotted by a focused quality plot.}
  \item{digits}{Number of digits in the compact reporting string.}
  \item{seed}{Deterministic synthetic-data seed.}
  \item{samples_per_target}{Synthetic samples generated at each of nine validation targets.}
  \item{...}{Additional arguments forwarded to the canonical report or base plotting function.}
}
\details{
Accuracy is target-referenced error. RMS-S2S precision quantifies adjacent-sample
fluctuation and never bridges a missing sample. Spatial SD and BCEA quantify
point-cloud spread around a stable target and use population standard deviations
(denominator \eqn{n}). BCEA always records its probability level.

Effective sampling frequency is estimated from the effective sample count divided
by an estimated recording duration (timestamp span plus one median positive
inter-sample interval). When gaze coordinates and validity are supplied, the
effective sample count contains valid gaze observations with finite timestamps;
otherwise the metric describes the timestamp stream.

The package deliberately does not impose universal acceptability cutoffs. A
\code{gaze_quality_report} stores review flags and provenance while preserving all
analysis units for downstream filtering, weighting, stratification, sensitivity
analysis, and transparent reporting.
}
\value{
Metric functions return data frames with explicit units and diagnostics.
\code{create_gaze_quality_report()} returns a data frame of class
\code{gaze_quality_report} with a \code{gaze_quality_provenance} attribute.
Plot functions draw base-R diagnostics and invisibly return the plotted data.
\code{report_gaze_quality()} returns a compact character summary.
\code{simulate_gaze_quality_calibration()} returns a deterministic synthetic
nine-point validation data set spanning six quality profiles.
}
\examples{
d <- simulate_gaze_quality_calibration(samples_per_target = 4)

q <- create_gaze_quality_report(
  d,
  by = c("profile", "target_id"),
  valid = "valid",
  missing_reason = "missing_reason",
  nominal_sampling_hz = 60
)

head(q[c(
  "profile", "target_id", "accuracy_mean", "precision_rms_s2s",
  "bcea", "effective_sampling_hz", "data_loss_fraction"
)])

report_gaze_quality(q)
}
\references{
Dunn, M. J., et al. (2024). A minimal reporting guideline for eye-tracking
studies. \emph{Behavior Research Methods}.

Niehorster, D. C., et al. (2026). How to determine the quality of eye-tracking
data: A tutorial. \emph{Behavior Research Methods}.
doi:10.3758/s13428-026-03039-4.
}
