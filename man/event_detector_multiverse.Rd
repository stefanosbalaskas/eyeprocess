\name{event_detector_multiverse}
\alias{define_event_detector_spec}
\alias{validate_event_detector_spec}
\alias{create_detector_multiverse}
\alias{detect_events_with_spec}
\alias{import_external_detector_events}
\alias{run_detector_multiverse}
\alias{match_detected_events}
\alias{compare_event_catalogues}
\alias{estimate_detector_agreement}
\alias{summarise_detector_events}
\alias{summarise_detector_disagreement}
\alias{propagate_detector_to_aoi}
\alias{propagate_detector_to_features}
\alias{run_detector_inference_multiverse}
\alias{assess_detector_inference_stability}
\alias{summarise_detector_robustness}
\alias{plot_detector_event_timeline}
\alias{plot_detector_agreement}
\alias{plot_detector_feature_distributions}
\alias{plot_detector_coefficient_stability}
\alias{plot_detector_multiverse}
\alias{report_detector_multiverse}
\alias{simulate_detector_multiverse_data}
\title{Event-detector multiverse and inference robustness}
\description{
Build explicit event-detector specifications, run detector branches independently,
compare event catalogues, propagate detector choice through AOI features and an
identical statistical model, and report scientific inference stability.
}
\usage{
define_event_detector_spec(
  detector_id, algorithm, velocity_threshold = NULL,
  dispersion_threshold = NULL, minimum_duration_ms = NULL,
  maximum_gap_ms = NULL, merge_rule = "none", sampling_rate = NULL,
  smoothing = NULL, filter = NULL, coordinate_unit = "degrees",
  implementation = NULL, implementation_version = NULL,
  parameters = list(), callback = NULL
)
validate_event_detector_spec(spec)
create_detector_multiverse(
  specs = NULL, base_spec = NULL, parameter_grid = NULL,
  id_template = "\%s_\%03d", label = "event_detector_multiverse"
)
detect_events_with_spec(x, spec)
import_external_detector_events(events, spec, dataset = NULL)
run_detector_multiverse(x, multiverse, continue_on_error = TRUE)
match_detected_events(
  reference, candidate, event_type = "fixation",
  onset_tolerance_ms = 75, minimum_overlap = 0.10
)
compare_event_catalogues(
  reference, candidate, event_type = "fixation",
  onset_tolerance_ms = 75, minimum_overlap = 0.10
)
estimate_detector_agreement(
  x, event_type = "fixation",
  onset_tolerance_ms = 75, minimum_overlap = 0.10
)
summarise_detector_events(x)
summarise_detector_disagreement(x, event_type = "fixation")
propagate_detector_to_aoi(x, overlap = "error", continue_on_error = TRUE)
propagate_detector_to_features(x, continue_on_error = TRUE)
run_detector_inference_multiverse(
  x, model_spec, model_callback = NULL, minimum_valid_fraction = NULL
)
assess_detector_inference_stability(
  x, term, substantive_threshold = NULL,
  direction = c("above", "below", "absolute")
)
summarise_detector_robustness(
  x, inference = NULL, term = NULL,
  substantive_threshold = NULL, direction = "above"
)
plot_detector_event_timeline(x, trial_id = NULL, event_type = "fixation", ...)
plot_detector_agreement(x, metric = "mean_event_overlap", ...)
plot_detector_feature_distributions(
  x, feature = "dwell_time_ms", aoi_id = NULL, ...
)
plot_detector_coefficient_stability(x, term, ...)
plot_detector_multiverse(
  x, inference = NULL, term = NULL,
  feature = "dwell_time_ms", aoi_id = NULL
)
report_detector_multiverse(
  x, inference = NULL, term = NULL,
  substantive_threshold = NULL, path = NULL
)
simulate_detector_multiverse_data(
  n_participants = 12L, sampling_rate = 60,
  trial_duration_s = 2, seed = 20260918L
)
}
\arguments{
\item{detector_id}{Unique detector-branch identifier.}
\item{algorithm}{One of \code{"ivt"}, \code{"idt"}, \code{"adaptive_velocity"},
\code{"remodnav"}, \code{"external"}, or \code{"vendor"}.}
\item{velocity_threshold}{Explicit velocity threshold in the declared coordinate unit per second.}
\item{dispersion_threshold}{Explicit dispersion threshold in the declared coordinate unit.}
\item{minimum_duration_ms}{Minimum event duration in milliseconds.}
\item{maximum_gap_ms}{Maximum gap used by algorithms that support gap handling.}
\item{merge_rule}{Recorded merge rule. No undeclared merging is applied.}
\item{sampling_rate}{Declared sample rate in Hz.}
\item{smoothing,filter}{Recorded preprocessing choices.}
\item{coordinate_unit}{\code{"degrees"}, \code{"pixels"}, or \code{"normalized"}.}
\item{implementation,implementation_version}{Detector implementation identity and version.}
\item{parameters}{Named algorithm-specific parameter list.}
\item{callback}{Explicit external detector or model callback.}
\item{spec}{An event-detector specification.}
\item{specs}{A list of explicit detector specifications.}
\item{base_spec}{Base specification for an explicit parameter grid.}
\item{parameter_grid}{Named list of parameter values. Values are study-specific alternatives, not universal defaults.}
\item{id_template}{\code{sprintf()} template for generated branch identifiers.}
\item{label}{Multiverse label.}
\item{x}{An \code{eye_dataset}, detector multiverse result, or detector inference result as required.}
\item{events,reference,candidate}{Canonical or canonicalizable event data frames.}
\item{dataset}{Optional source \code{eye_dataset} used to attach provenance to imported events.}
\item{continue_on_error}{Whether to retain a failed branch and continue other specifications.}
\item{event_type}{Event type to compare, usually \code{"fixation"}.}
\item{onset_tolerance_ms}{Allowed onset difference for near-boundary matches.}
\item{minimum_overlap}{Minimum temporal intersection-over-union for event matching.}
\item{overlap}{AOI-overlap rule. The default \code{"error"} prevents silent ambiguity resolution.}
\item{model_spec}{Named list declaring the model engine, formula, outcome, and optional AOI/model settings.}
\item{model_callback}{Specialist model callback returning the documented tidy coefficient contract.}
\item{minimum_valid_fraction}{Explicit minimum gaze-validity fraction before modelling.}
\item{term}{Model coefficient term to assess.}
\item{substantive_threshold}{Optional analyst-declared effect threshold.}
\item{direction}{How the substantive threshold is interpreted.}
\item{inference}{Optional detector inference result.}
\item{trial_id}{Optional trial filter.}
\item{metric}{Agreement metric to plot.}
\item{feature}{Propagated feature to plot.}
\item{aoi_id}{Optional AOI filter.}
\item{path}{Optional Markdown report path.}
\item{n_participants}{Synthetic-example participant count.}
\item{trial_duration_s}{Synthetic trial duration in seconds.}
\item{seed}{Synthetic-data random seed.}
\item{...}{Additional base-graphics arguments.}
}
\details{
The multiverse is intended to answer whether event detection materially changes
the quantities and scientific inference used by a study. It is not a mechanism
for selecting the branch with a preferred p-value.

Zero-event trials remain represented during feature propagation. Trials with no
valid gaze remain missing rather than being converted to zero. AOI ambiguity is
an error unless an analyst explicitly selects another overlap rule. Model engines
are explicit; formula engines use \code{na.fail}; and non-converged model branches
are retained diagnostically but excluded from coefficient-stability summaries.

\code{algorithm = "remodnav"} calls an installed REMoDNaV executable and never
substitutes an internal approximation. Pixel coordinates require an explicit
\code{parameters$px2deg}. The built-in adaptive-velocity implementation is
reported as a robust-MAD reference detector and must not be described as a named
external algorithm.
}
\value{
Specification functions return detector specification or multiverse objects.
Execution/propagation functions return governed multiverse result objects.
Comparison and stability functions return data frames or named summary lists.
Plotting functions draw base-R diagnostics and invisibly return plotted data.
\code{report_detector_multiverse()} returns Markdown text.
\code{simulate_detector_multiverse_data()} returns a synthetic \code{eye_dataset}.
}
\examples{
d <- simulate_detector_multiverse_data(n_participants = 4, seed = 1)

s1 <- define_event_detector_spec(
  "ivt25", "ivt",
  velocity_threshold = 25,
  minimum_duration_ms = 60,
  maximum_gap_ms = 75,
  sampling_rate = 60
)
s2 <- define_event_detector_spec(
  "ivt35", "ivt",
  velocity_threshold = 35,
  minimum_duration_ms = 60,
  maximum_gap_ms = 75,
  sampling_rate = 60
)

mv <- create_detector_multiverse(list(s1, s2))
out <- run_detector_multiverse(d, mv)
out <- propagate_detector_to_aoi(out)
out <- propagate_detector_to_features(out)
summarise_detector_events(out)
}
\seealso{
\code{\link{detect_fixations_ivt}}, \code{\link{detect_fixations_idt}},
\code{\link{assign_aois}}, \code{\link{preprocessing_multiverse}}
}
