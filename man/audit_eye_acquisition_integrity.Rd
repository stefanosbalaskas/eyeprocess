\name{audit_eye_acquisition_integrity}
\alias{audit_eye_acquisition_integrity}
\title{Audit acquisition timestamp integrity}
\usage{
audit_eye_acquisition_integrity(
  time_s, valid = NULL, device_native_rate_hz = NULL,
  sdk_delivered_rate_hz = NULL, analysis_grid_rate_hz = NULL,
  catchup_long_ratio = 2.5, catchup_short_ratio = 0.5
)
}
\arguments{
\item{time_s}{Numeric timestamps in seconds in original recording order.}
\item{valid}{Optional complete logical per-sample validity mask.}
\item{device_native_rate_hz}{Optional declared hardware-native rate.}
\item{sdk_delivered_rate_hz}{Optional declared acquisition-SDK output rate.}
\item{analysis_grid_rate_hz}{Optional declared resampled analysis-grid rate.}
\item{catchup_long_ratio}{Multiplier defining unusually long intervals.}
\item{catchup_short_ratio}{Multiplier defining unusually short next intervals.}
}
\description{
Returns interval quantiles, duplicate or quantized timestamp count,
catch-up candidates, empirical rates and valid-state fraction, without
silently deduplicating observations or asserting precise clock synchronization.
}
\value{An \code{eye_acquisition_integrity} list with \code{summary},
\code{intervals}, and \code{claim_boundary}.}
