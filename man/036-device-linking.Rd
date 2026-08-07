\name{fit_device_linking}
\alias{fit_device_linking}
\alias{apply_device_linking}
\alias{audit_device_equivalence}
\alias{estimate_device_specific_error}
\alias{plot.eye_device_linking}
\alias{plot.eye_device_equivalence}
\alias{plot_device_agreement}
\alias{plot_device_bias_by_magnitude}
\alias{plot_device_transfer_curve}
\alias{plot_device_equivalence_intervals}
\alias{plot_cross_vendor_metric_matrix}
\title{Cross-device and cross-vendor metric linking}
\description{
Cross-device and cross-vendor metric linking. These functions form the eyeprocess 0.6.0.9000 measurement-intelligence programme and use dependency-free reference implementations with explicit evidence limits.
}
\usage{
fit_device_linking(x, metric, reference_device, method = c("mixed_bland_altman",
  "hierarchical", "equipercentile"), device_col = "device", id_cols = c("person_id",
  "item_id"))
apply_device_linking(x, linking_model, metric = NULL, device_col = NULL,
  output_col = NULL)
audit_device_equivalence(x, equivalence_margin, by = c("metric", "task", "aoi"))
estimate_device_specific_error(x)
plot_device_agreement(x, ...)
plot_device_bias_by_magnitude(x, ...)
plot_device_transfer_curve(x, ...)
plot_device_equivalence_intervals(x, ...)
plot_cross_vendor_metric_matrix(x, ...)
}
\arguments{
\item{x}{Input object or data structure appropriate for the selected analysis.}
\item{metric}{Argument controlling `metric`; see the function usage and returned audit metadata.}
\item{reference_device}{Argument controlling `reference_device`; see the function usage and returned audit metadata.}
\item{method}{Argument controlling `method`; see the function usage and returned audit metadata.}
\item{device_col}{Argument controlling `device_col`; see the function usage and returned audit metadata.}
\item{id_cols}{Argument controlling `id_cols`; see the function usage and returned audit metadata.}
\item{linking_model}{Argument controlling `linking_model`; see the function usage and returned audit metadata.}
\item{output_col}{Argument controlling `output_col`; see the function usage and returned audit metadata.}
\item{equivalence_margin}{Argument controlling `equivalence_margin`; see the function usage and returned audit metadata.}
\item{by}{Argument controlling `by`; see the function usage and returned audit metadata.}
\item{...}{Additional arguments passed to the underlying method or plotting function.}
}
\details{
The APIs return auditable S3 objects. Plot wrappers call registered base-graphics methods. Experimental or approximate engines are labelled in object status fields and should be validated before confirmatory or operational use.
}
\value{
An eyeprocess result object, data frame, model object, plot, or audit table as documented by the individual function.
}
\seealso{\code{plot_diagnostics()}, \code{plot_evidence()}, and \code{plot_sensitivity()}.}
\keyword{methods}
