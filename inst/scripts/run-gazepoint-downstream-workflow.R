# Complete real Gazepoint downstream workflow for the validated private corpus.

source_dir <- paste0(
  "path/to/",
  "eyeprocess-validation-corpus/cases/gazepoint-analysis-v7.2.0-demo"
)
output_dir <- paste0(
  "path/to/",
  "eyeprocess-downstream-output"
)

if (!requireNamespace("eyeprocess", quietly = TRUE)) {
  stop("Install eyeprocess before running this script.", call. = FALSE)
}

spec <- eyeprocess::gazepoint_workflow_spec(
  expected_sampling_rate = 60,
  sampling_tolerance_hz = 5,
  minimum_valid_gaze = 0.80,
  minimum_valid_pupil = 0.70,
  pupil_interpolation = "linear",
  pupil_max_gap_ms = 150,
  pupil_filter = "median",
  pupil_window = 5L,
  pupil_baseline = "none",
  detect_blinks = TRUE,
  create_plots = TRUE,
  create_html_report = TRUE,
  retain_raw = TRUE
)

result <- eyeprocess::run_gazepoint_workflow(
  source_dir,
  output_dir = output_dir,
  spec = spec,
  overwrite = TRUE
)

print(result)
print(eyeprocess::validate_gazepoint_workflow(result))
cat("\nWorkflow output:\n", output_dir, "\n", sep = "")
