\name{eyeprocess-adapters}
\alias{eye_mapping}
\alias{print.eye_mapping}
\alias{validate_eye_mapping}
\alias{infer_eye_mapping}
\alias{register_eye_adapter}
\alias{unregister_eye_adapter}
\alias{supported_eye_formats}
\alias{detect_eye_format}
\alias{print.eye_format_detection}
\alias{read_eye_export}
\alias{read_eye_folder}
\alias{combine_eye_datasets}
\alias{remap_recording_ids}
\title{Mappings and adapter registry}
\description{Map heterogeneous exports, register import adapters, detect formats, and combine canonical datasets.}
\details{These functions operate on the canonical relational representation used by \pkg{eyeprocess}. Native fields and timestamps are retained whenever possible, and transformations should be recorded in the provenance table. Optional modelling engines are used only when their packages are installed.}
\value{The returned value depends on the function. Import and transformation functions generally return an \code{eye_dataset}; audit and feature functions return data frames or enriched datasets; plotting functions return their plotted data invisibly; modelling functions return engine-specific or \code{eyeprocess_model} objects.}
\seealso{\code{new_eye_dataset()}, \code{read_eye_export()}, \code{validate_eye_dataset()}, \code{provenance_manifest()}}
\keyword{methods}
