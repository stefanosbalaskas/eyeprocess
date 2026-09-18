\name{aoi_perturbation_uncertainty}
\alias{aoi_perturbation_uncertainty}
\alias{validate_aoi_geometry}
\alias{convert_aoi_margin_to_degrees}
\alias{convert_aoi_margin_to_pixels}
\alias{aoi_perturbation_spec}
\alias{dilate_aoi}
\alias{erode_aoi}
\alias{translate_aoi}
\alias{jitter_aoi}
\alias{perturb_aoi_geometry}
\alias{create_aoi_perturbation_grid}
\alias{apply_aoi_perturbation_grid}
\alias{compare_aoi_assignments}
\alias{estimate_aoi_assignment_stability}
\alias{estimate_fixation_assignment_probability}
\alias{recompute_aoi_features}
\alias{run_aoi_sensitivity_analysis}
\alias{assess_aoi_inference_stability}
\alias{summarise_aoi_sensitivity}
\alias{report_aoi_sensitivity}
\alias{plot_aoi_perturbations}
\alias{plot_aoi_assignment_stability}
\alias{plot_aoi_coefficient_stability}
\alias{plot_aoi_robustness_surface}
\title{AOI Perturbation and Uncertainty Analysis}
\description{
Vendor-neutral tools for treating area-of-interest (AOI) geometry as an explicit
analytical assumption. The workflow validates rectangle and polygon geometry,
constructs reproducible perturbations, remaps gaze or fixation coordinates,
recomputes AOI features, optionally reruns a user-supplied statistical model,
and reports assignment and inference stability without interpreting robustness
frequencies as probabilities that a scientific conclusion is true.
}
\usage{
validate_aoi_geometry(aois, allow_overlap = TRUE)

convert_aoi_margin_to_degrees(
  margin_pixels, screen_width_px, screen_height_px,
  viewing_distance, physical_screen_size
)

convert_aoi_margin_to_pixels(
  margin_degrees, screen_width_px, screen_height_px,
  viewing_distance, physical_screen_size
)

aoi_perturbation_spec(
  perturbation_id, operation = "baseline", margin_x = 0,
  margin_y = margin_x, translation_x = 0, translation_y = 0,
  unit = "px", screen_width_px = NULL, screen_height_px = NULL,
  viewing_distance = NULL, physical_screen_size = NULL,
  degrees_per_pixel = NULL, seed = NULL, boundary_policy = "warn"
)

dilate_aoi(aois, margin, ...)
erode_aoi(aois, margin, ...)
translate_aoi(aois, x = 0, y = 0, ...)
jitter_aoi(aois, x, y = x, seed = 20260918L, ...)
perturb_aoi_geometry(aois, spec)

create_aoi_perturbation_grid(
  dilations = NULL, erosions = NULL, translations_x = NULL,
  translations_y = NULL, translations_xy = NULL,
  jitters = NULL, anisotropic = NULL,
  unit = "px", include_baseline = TRUE, seed = 20260918L, ...
)

apply_aoi_perturbation_grid(aois, grid)
compare_aoi_assignments(baseline, perturbed, ids = seq_along(baseline))

estimate_aoi_assignment_stability(
  comparisons, metadata = NULL, group_cols = NULL
)

estimate_fixation_assignment_probability(
  assignments, ids = NULL, include_baseline = TRUE
)

recompute_aoi_features(
  data, assignments, participant_col = NULL, trial_col = NULL,
  duration_col = NULL, time_col = NULL, perturbation_id = NULL,
  aoi_levels = NULL
)

run_aoi_sensitivity_analysis(
  data, aois, grid, x_col, y_col, observation_id_col = NULL,
  participant_col = NULL, trial_col = NULL, duration_col = NULL,
  time_col = NULL, overlap_policy = "ambiguous", model_callback = NULL,
  preprocessing_specification = NULL, event_detector = NULL,
  quality_rules = NULL, model_specification = NULL
)

assess_aoi_inference_stability(x, term = NULL)
summarise_aoi_sensitivity(x)
report_aoi_sensitivity(x)

plot_aoi_perturbations(
  x, perturbation_id = NULL, data = NULL,
  x_col = NULL, y_col = NULL, ...
)

plot_aoi_assignment_stability(x, ...)
plot_aoi_coefficient_stability(x, term, ...)

plot_aoi_robustness_surface(
  x, value_col = "proportion_unchanged",
  x_col = "margin_x", y_col = "margin_y", ...
)
}
\arguments{
\item{aois}{A data frame containing rectangular or polygonal AOI geometry.}
\item{allow_overlap}{Whether overlapping AOIs are allowed during validation.}
\item{margin_pixels, margin_degrees, margin, margin_x, margin_y}{Perturbation margins in the declared unit.}
\item{screen_width_px, screen_height_px}{Declared display resolution in pixels.}
\item{viewing_distance}{Viewing distance in the same physical unit used for screen size.}
\item{physical_screen_size}{Length-two physical screen width and height.}
\item{perturbation_id}{Stable identifier for a perturbation branch.}
\item{operation}{One of baseline, dilation, erosion, translate, jitter, or anisotropic expansion.}
\item{translation_x, translation_y, x, y}{Horizontal and vertical translation values.}
\item{unit}{Either pixels (px) or degrees of visual angle (deg).}
\item{degrees_per_pixel}{Optional explicit horizontal and vertical degrees-per-pixel values.}
\item{seed}{Reproducible random seed used for jitter.}
\item{boundary_policy}{One of warn, clip, error, or allow for geometry outside the declared screen.}
\item{spec}{An AOI perturbation specification.}
\item{dilations, erosions, translations_x, translations_y, translations_xy, jitters, anisotropic}{Vectors or pairs defining a deterministic perturbation grid. `translations_xy` accepts a length-two vector, two-column matrix/data frame, or list of x/y pairs.}
\item{include_baseline}{Whether to include the unchanged nominal branch.}
\item{grid}{An AOI perturbation grid.}
\item{baseline, perturbed}{Nominal and perturbed AOI assignment vectors.}
\item{ids}{Optional observation identifiers.}
\item{comparisons}{Named AOI assignment comparison objects.}
\item{metadata}{Optional one-row-per-observation metadata.}
\item{group_cols}{Optional participant, trial, or other grouping columns.}
\item{assignments}{AOI assignment vector or named branch list, depending on the function.}
\item{data}{Sample- or fixation-level data.}
\item{participant_col, trial_col}{Optional participant and trial grouping columns.}
\item{duration_col, time_col}{Optional duration and time columns used for AOI feature recomputation.}
\item{aoi_levels}{Optional complete AOI identifier universe. When supplied, observed participant/trial groups retain explicit zero-count AOI cells; groups with no valid assignment opportunity retain missing values rather than zeros.}
\item{x_col, y_col}{Coordinate columns in data or robustness-surface dimensions.}
\item{observation_id_col}{Optional unique observation identifier column.}
\item{overlap_policy}{How observed overlapping membership is represented; defaults to explicit ambiguity.}
\item{model_callback}{Optional function receiving recomputed features, assigned row data, and the current perturbation specification.}
\item{preprocessing_specification, event_detector, quality_rules, model_specification}{Provenance fields retained in the result object.}
\item{x}{An AOI sensitivity, perturbation, or stability object, depending on the function.}
\item{term}{Model term to summarize or plot.}
\item{value_col}{Robustness quantity used in the surface plot.}
\item{...}{Additional perturbation-specification or plotting arguments.}
}
\details{
Overlapping AOIs are not resolved silently. The default row-level assignment
for overlap is the explicit label \code{__ambiguous__}. Missing coordinates
remain missing. Feature recomputation distinguishes structural zero AOI cells from groups with no valid assignment opportunity; the latter remain missing. Degree-based perturbations require declared display geometry
and viewing distance, or explicit degrees-per-pixel values.

A model callback must explicitly choose and fit the desired estimator and
return at least term, estimate, SE, CI_low, CI_high, p_value,
model_converged, and N. Callback errors and non-converged fits remain visible
in the audit trail.

Same-sign proportions and assignment frequencies are descriptive sensitivity
summaries conditional on the declared perturbation set. They must not be
reported as probabilities that the scientific conclusion is true.
}
\value{
Functions return validated AOI geometry, structured perturbation specifications
and grids, perturbed geometry, assignment-comparison and stability objects,
recomputed feature tables, AOI sensitivity objects, descriptive summaries,
Markdown reporting text, or plots as documented by each function.
}
\examples{
aois <- data.frame(
  aoi_id = c("claim", "disclosure"),
  xmin = c(100, 100), xmax = c(400, 400),
  ymin = c(100, 240), ymax = c(220, 320)
)

grid <- create_aoi_perturbation_grid(
  dilations = c(5, 10),
  erosions = 5,
  translations_x = 5,
  translations_xy = c(5, -5)
)

gaze <- data.frame(
  id = 1:5,
  x = c(150, 390, 405, 180, NA),
  y = c(150, 215, 215, 270, NA),
  duration = c(.1, .1, .1, .1, NA)
)

result <- run_aoi_sensitivity_analysis(
  gaze, aois, grid,
  x_col = "x", y_col = "y",
  observation_id_col = "id",
  duration_col = "duration"
)

summarise_aoi_sensitivity(result)
}
