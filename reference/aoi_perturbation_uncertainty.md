# AOI Perturbation and Uncertainty Analysis

Vendor-neutral tools for treating area-of-interest (AOI) geometry as an
explicit analytical assumption. The workflow validates rectangle and
polygon geometry, constructs reproducible perturbations, remaps gaze or
fixation coordinates, recomputes AOI features, optionally reruns a
user-supplied statistical model, and reports assignment and inference
stability without interpreting robustness frequencies as probabilities
that a scientific conclusion is true.

## Usage

``` r
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
  aoi_levels = NULL, observation_level = c("fixation", "sample")
)

run_aoi_sensitivity_analysis(
  data, aois, grid, x_col, y_col, observation_id_col = NULL,
  participant_col = NULL, trial_col = NULL, duration_col = NULL,
  time_col = NULL, observation_level = c("fixation", "sample"),
  overlap_policy = "ambiguous", model_callback = NULL,
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
```

## Arguments

- aois:

  A data frame containing rectangular or polygonal AOI geometry.

- allow_overlap:

  Whether overlapping AOIs are allowed during validation.

- margin_pixels, margin_degrees, margin, margin_x, margin_y:

  Perturbation margins in the declared unit.

- screen_width_px, screen_height_px:

  Declared display resolution in pixels.

- viewing_distance:

  Viewing distance in the same physical unit used for screen size.

- physical_screen_size:

  Length-two physical screen width and height.

- perturbation_id:

  Stable identifier for a perturbation branch.

- operation:

  One of baseline, dilation, erosion, translate, jitter, or anisotropic
  expansion.

- translation_x, translation_y, y:

  Horizontal and vertical translation values.

- unit:

  Either pixels (px) or degrees of visual angle (deg).

- degrees_per_pixel:

  Optional explicit horizontal and vertical degrees-per-pixel values.

- seed:

  Reproducible random seed used for jitter.

- boundary_policy:

  One of warn, clip, error, or allow for geometry outside the declared
  screen.

- spec:

  An AOI perturbation specification.

- dilations, erosions, translations_x, translations_y, translations_xy,
  jitters, anisotropic:

  Vectors or pairs defining a deterministic perturbation grid.
  \`translations_xy\` accepts a length-two vector, two-column
  matrix/data frame, or list of x/y pairs.

- include_baseline:

  Whether to include the unchanged nominal branch.

- grid:

  An AOI perturbation grid.

- baseline, perturbed:

  Nominal and perturbed AOI assignment vectors.

- ids:

  Optional observation identifiers.

- comparisons:

  Named AOI assignment comparison objects.

- metadata:

  Optional one-row-per-observation metadata.

- group_cols:

  Optional participant, trial, or other grouping columns.

- assignments:

  AOI assignment vector or named branch list, depending on the function.

- data:

  Sample- or fixation-level data.

- participant_col, trial_col:

  Optional participant and trial grouping columns.

- duration_col, time_col:

  Optional duration and time columns used for AOI feature recomputation.

- aoi_levels:

  Optional complete AOI identifier universe. When supplied, observed
  participant/trial groups retain explicit zero-count AOI cells; groups
  with no valid assignment opportunity retain missing values rather than
  zeros.

- observation_level:

  Explicit input level, either fixation or sample. Universal observation
  counts/timing are retained without labeling sample rows as fixations.

- x_col, y_col:

  Coordinate columns in data or robustness-surface dimensions.

- observation_id_col:

  Optional unique observation identifier column.

- overlap_policy:

  How observed overlapping membership is represented; defaults to
  explicit ambiguity.

- model_callback:

  Optional function receiving recomputed features, assigned row data,
  and the current perturbation specification.

- preprocessing_specification, event_detector, quality_rules,
  model_specification:

  Provenance fields retained in the result object.

- x:

  Horizontal translation or jitter value for geometry helpers, or an AOI
  sensitivity, perturbation, or stability object for plotting and
  summary methods, depending on the function.

- term:

  Model term to summarize or plot.

- value_col:

  Robustness quantity used in the surface plot.

- ...:

  Additional perturbation-specification or plotting arguments.

## Details

Overlapping AOIs are not resolved silently. The default row-level
assignment for overlap is the explicit label `__ambiguous__`. Missing
coordinates remain missing. Feature recomputation distinguishes
structural zero AOI cells from groups with no valid assignment
opportunity; the latter remain missing. Degree-based perturbations
require declared display geometry and viewing distance, or explicit
degrees-per-pixel values.

A model callback must explicitly choose and fit the desired estimator
and return at least term, estimate, SE, CI_low, CI_high, p_value,
model_converged, and N. Convergence must be explicit; converged rows
require finite estimates, standard errors, confidence limits, and
positive integer N. Callback errors and non-converged fits remain
visible in the audit trail, and inference summaries retain the model-N
range across converged perturbations.

Same-sign proportions and assignment frequencies are descriptive
sensitivity summaries conditional on the declared perturbation set. They
must not be reported as probabilities that the scientific conclusion is
true.

## Value

Functions return validated AOI geometry, structured perturbation
specifications and grids, perturbed geometry, assignment-comparison and
stability objects, recomputed feature tables, AOI sensitivity objects,
descriptive summaries, Markdown reporting text, or plots as documented
by each function.

## Examples

``` r
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
  duration_col = "duration",
  observation_level = "fixation"
)
#> Warning: No time_col supplied; first_fixation is returned as NA rather than inferred from row order.
#> Warning: No time_col supplied; first_fixation is returned as NA rather than inferred from row order.
#> Warning: No time_col supplied; first_fixation is returned as NA rather than inferred from row order.
#> Warning: No time_col supplied; first_fixation is returned as NA rather than inferred from row order.
#> Warning: No time_col supplied; first_fixation is returned as NA rather than inferred from row order.
#> Warning: No time_col supplied; first_fixation is returned as NA rather than inferred from row order.

summarise_aoi_sensitivity(result)
#> $assignment_stability
#>                   perturbation_id n_total n_comparable proportion_unchanged
#> baseline                 baseline       5            4                 1.00
#> dilate_10_px         dilate_10_px       5            4                 0.75
#> dilate_5_px           dilate_5_px       5            4                 0.75
#> erode_5_px             erode_5_px       5            4                 1.00
#> shift_x_5_px         shift_x_5_px       5            4                 0.75
#> shift_xy_5_-5_px shift_xy_5_-5_px       5            4                 0.75
#>                  proportion_newly_assigned proportion_lost
#> baseline                              0.00               0
#> dilate_10_px                          0.25               0
#> dilate_5_px                           0.25               0
#> erode_5_px                            0.00               0
#> shift_x_5_px                          0.25               0
#> shift_xy_5_-5_px                      0.25               0
#>                  proportion_reassigned
#> baseline                             0
#> dilate_10_px                         0
#> dilate_5_px                          0
#> erode_5_px                           0
#> shift_x_5_px                         0
#> shift_xy_5_-5_px                     0
#> 
#> $inference_stability
#> data frame with 0 columns and 0 rows
#> 
#> $perturbation_audit
#>    perturbation_id    status message geometry_hash
#> 1         baseline completed    <NA>     15663-463
#> 2      dilate_5_px completed    <NA>     16473-463
#> 3     dilate_10_px completed    <NA>     16773-463
#> 4       erode_5_px completed    <NA>     16638-463
#> 5     shift_x_5_px completed    <NA>     15953-463
#> 6 shift_xy_5_-5_px completed    <NA>     16508-463
#> 
#> $failures
#> [1] perturbation_id message         stage          
#> <0 rows> (or 0-length row.names)
#> 
#> $n_planned
#> [1] 6
#> 
#> $n_completed
#> [1] 6
#> 
#> $n_geometry_failed
#> [1] 0
#> 
#> $n_model_failures
#> [1] 0
#> 
#> $caveat
#> [1] "Robustness proportions summarize the declared perturbation set; they are not probabilities that the substantive conclusion is true."
#> 
#> attr(,"class")
#> [1] "eye_aoi_sensitivity_summary" "list"                       
```
