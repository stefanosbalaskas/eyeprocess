\name{redact_validation_case}
\alias{redact_validation_case}
\title{Redact a validation case without inventing replacement data}
\usage{
redact_validation_case(source_path, output_path, id_columns = c("participant_id",
  "subject", "participant", "recording_id", "session_id"), remove_columns = c("name",
  "email", "address", "birthdate", "date_of_birth"), text_redactor = NULL, salt,
  copy_non_tabular = FALSE, overwrite = FALSE)
}
\arguments{
  \item{source_path}{Source file/directory.}
  \item{output_path}{Redacted output directory.}
  \item{id_columns}{Identifier columns to pseudonymize.}
  \item{remove_columns}{Columns to remove.}
  \item{text_redactor}{Optional function applied to character columns.}
  \item{salt}{Required project-specific salt.}
  \item{copy_non_tabular}{Copy unsupported files unchanged.}
  \item{overwrite}{Replace output.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
