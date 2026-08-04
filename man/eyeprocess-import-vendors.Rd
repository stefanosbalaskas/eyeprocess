\name{eyeprocess-import-vendors}
\alias{is_tobii_export}
\alias{read_tobii}
\alias{is_pupil_labs_export}
\alias{pupil_labs_format}
\alias{read_pupillabs}
\alias{read_pupil_neon}
\alias{read_pupil_core}
\alias{is_eyelink_export}
\alias{read_eyelink_asc}
\alias{read_eyelink_report}
\alias{read_eyelink_edf}
\alias{is_smi_export}
\alias{read_smi}
\alias{read_smi_raw_export}
\alias{read_smi_event_export}
\alias{read_smi_aoi_export}
\title{Import Tobii, Pupil Labs, EyeLink, and SMI exports}
\description{Dedicated import adapters for common and legacy eye-tracking export ecosystems.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
