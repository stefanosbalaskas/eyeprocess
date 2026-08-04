\name{eyeprocess-coordinates-time}
\alias{register_coordinate_space}
\alias{coordinate_space}
\alias{convert_xy}
\alias{convert_coordinates}
\alias{audit_coordinate_spaces}
\alias{estimate_sampling_rate}
\alias{normalize_timebase}
\alias{audit_timebase}
\alias{align_clock}
\alias{estimate_clock_transform}
\alias{print.eye_clock_transform}
\alias{apply_clock_transform}
\alias{synchronize_eye_biometrics}
\alias{audit_clock_sync}
\title{Coordinate and timebase management}
\description{Register and convert coordinate spaces, normalize native clocks, estimate sampling rates, and synchronize streams.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
