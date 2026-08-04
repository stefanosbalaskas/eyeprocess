\name{gazepoint_workflow_spec}
\alias{gazepoint_workflow_spec}
\alias{build_gazepoint_media_trials}
\alias{derive_gazepoint_workflow_features}
\alias{gazepoint_analysis_tables}
\alias{gazepoint_irt_tables}
\alias{plot_gazepoint_workflow}
\alias{run_gazepoint_workflow}
\alias{validate_gazepoint_workflow}
\alias{write_gazepoint_workflow_report}
\title{Integrated Gazepoint Downstream Workflow}
\description{
Imports a real Gazepoint export folder and executes the complete downstream
workflow from canonical harmonization through QC, media/trial reconstruction,
feature derivation, plots, analysis-ready tables, IRT-ready templates, and a
reproducible report.
}
\usage{
gazepoint_workflow_spec(
  expected_sampling_rate = 60,
  sampling_tolerance_hz = 5,
  minimum_valid_gaze = 0.8,
  minimum_valid_pupil = 0.7,
  pupil_interpolation = "linear",
  pupil_max_gap_ms = 150,
  pupil_filter = "median",
  pupil_window = 5L,
  pupil_baseline = "none",
  pupil_baseline_window = c(0, 0.5),
  detect_blinks = TRUE,
  biometric_channels = c("eda", "skin_conductance_level",
    "skin_conductance_response", "heart_rate", "interbeat_interval",
    "engagement_dial"),
  create_plots = TRUE,
  create_html_report = TRUE,
  retain_raw = TRUE
)

build_gazepoint_media_trials(x, item_map = NULL, overwrite = TRUE)

derive_gazepoint_workflow_features(x, reset_workflow_features = TRUE)

gazepoint_analysis_tables(x)

gazepoint_irt_tables(x, process_table = NULL)

plot_gazepoint_workflow(x, directory, channels = NULL, expected_hz = 60)

run_gazepoint_workflow(
  path,
  output_dir = file.path(getwd(), "eyeprocess-gazepoint-workflow"),
  responses = NULL,
  score_key = NULL,
  item_map = NULL,
  spec = gazepoint_workflow_spec(),
  overwrite = FALSE,
  quiet = FALSE
)

validate_gazepoint_workflow(x)

write_gazepoint_workflow_report(
  workflow,
  path = file.path(workflow$output_dir, "gazepoint-workflow-report.md"),
  render_html = workflow$spec$create_html_report
)
}
\arguments{
  \item{expected_sampling_rate}{Expected gaze sampling rate in hertz.}
  \item{sampling_tolerance_hz}{Allowed absolute deviation from the expected rate.}
  \item{minimum_valid_gaze}{Minimum acceptable valid-gaze fraction.}
  \item{minimum_valid_pupil}{Minimum acceptable valid-pupil fraction.}
  \item{pupil_interpolation}{Pupil interpolation method.}
  \item{pupil_max_gap_ms}{Maximum interpolated pupil gap in milliseconds.}
  \item{pupil_filter}{Pupil filter method.}
  \item{pupil_window}{Pupil-filter window in samples.}
  \item{pupil_baseline}{Pupil baseline-correction method.}
  \item{pupil_baseline_window}{Window relative to media/trial onset.}
  \item{detect_blinks}{Whether to derive blink episodes.}
  \item{biometric_channels}{Biometric channels used in workflow plots.}
  \item{create_plots}{Whether the complete plot suite is generated.}
  \item{create_html_report}{Whether an HTML report is rendered when possible.}
  \item{retain_raw}{Whether native exports are retained in the canonical object.}
  \item{x}{An \code{eye_dataset} or workflow object, depending on the function.}
  \item{item_map}{Optional data frame or CSV mapping \code{stimulus_id} to
  \code{item_id} and optionally \code{condition_id}.}
  \item{overwrite}{Whether existing trials or output directories are replaced.}
  \item{reset_workflow_features}{Whether workflow-derived features are replaced.}
  \item{process_table}{Optional analysis-ready process table.}
  \item{directory}{Plot output directory.}
  \item{channels}{Biometric channels to plot.}
  \item{expected_hz}{Expected sampling rate used in plot diagnostics.}
  \item{path}{Gazepoint folder or report destination, depending on the function.}
  \item{output_dir}{Workflow output directory.}
  \item{responses}{Optional observed response data frame or CSV file.}
  \item{score_key}{Optional named vector of correct responses by item id.}
  \item{spec}{A workflow specification.}
  \item{quiet}{Whether progress messages are suppressed.}
  \item{workflow}{An \code{eye_gazepoint_workflow} object.}
  \item{render_html}{Whether to render an HTML report.}
}
\details{
Media presentations are reconstructed as explicit trials. When no behavioural
responses are supplied, the workflow creates a response template and labels the
outputs as process-ready but response-pending. It does not fabricate responses,
scores, ability estimates, item parameters, or psychological interpretations.

Only observations with valid vendor flags enter biometric analysis features;
native values are preserved. Pupil baseline correction is disabled by default
because media-onset data are not necessarily a true pre-stimulus baseline.
}
\value{
The specification function returns an \code{eye_gazepoint_workflow_spec}.
The runner returns an \code{eye_gazepoint_workflow}. Table builders return
named lists or data frames, and plotting returns a plot manifest.
}
\examples{
\dontrun{
source_dir <- "C:/path/to/gazepoint-exports"
output_dir <- "C:/path/to/eyeprocess-output"

result <- run_gazepoint_workflow(
  source_dir,
  output_dir = output_dir,
  overwrite = TRUE
)

print(result)
validate_gazepoint_workflow(result)
}
}
