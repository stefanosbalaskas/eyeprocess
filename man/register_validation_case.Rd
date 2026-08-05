\name{register_validation_case}
\alias{register_validation_case}
\title{Register an independent validation case}
\usage{
register_validation_case(corpus_path, source_path, vendor, device_model,
  software_name, software_version, hardware_version = NA_character_,
  export_profile = NA_character_, sampling_rate_hz = NA_real_,
  coordinate_system = NA_character_, timebase = NA_character_,
  event_semantics = NA_character_, ocular_structure = NA_character_,
  missingness_convention = NA_character_, vendor_fixations = NA_character_,
  package_transformations = NA_character_, unsupported_fields = NA_character_,
  independent_source = TRUE, licence_reviewed = FALSE, redistribution_allowed = FALSE,
  support_level = c("declared", "fixture-tested", "empirically-validated"),
  mode = c("reference", "copy"), case_id = NULL, notes = NA_character_)
}
\arguments{
  \item{corpus_path}{Corpus directory.}
  \item{source_path}{Real export file or directory.}
  \item{vendor}{Vendor name.}
  \item{device_model}{Hardware model.}
  \item{software_name}{Export software and version.}
  \item{software_version}{Export software and version.}
  \item{hardware_version}{Optional hardware/firmware version.}
  \item{export_profile}{Export options/profile.}
  \item{sampling_rate_hz}{Nominal or observed rate.}
  \item{coordinate_system}{Semantics metadata.}
  \item{timebase}{Semantics metadata.}
  \item{event_semantics}{Semantics metadata.}
  \item{ocular_structure}{Monocular/binocular structure.}
  \item{missingness_convention}{Vendor missing-value convention.}
  \item{vendor_fixations}{Description of vendor-derived fixation fields.}
  \item{package_transformations}{Declared package transformations.}
  \item{unsupported_fields}{Known unsupported fields.}
  \item{independent_source}{Whether independently obtained.}
  \item{licence_reviewed}{Whether licensing review is complete.}
  \item{redistribution_allowed}{Whether redacted material may be redistributed.}
  \item{support_level}{Declared support level.}
  \item{mode}{Reference source in place or copy it into the private corpus.}
  \item{case_id}{Optional explicit identifier.}
  \item{notes}{Notes.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
