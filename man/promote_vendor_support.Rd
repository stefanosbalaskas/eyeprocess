\name{promote_vendor_support}
\alias{promote_vendor_support}
\title{Promote a case support level only when evidence is supplied}
\usage{
promote_vendor_support(corpus_path, case_id, level = c("fixture-tested",
  "empirically-validated"), validation, reviewer, notes = NA_character_)
}
\arguments{
  \item{corpus_path}{Corpus directory.}
  \item{case_id}{Case identifier.}
  \item{level}{New support level.}
  \item{validation}{Validation result/audit.}
  \item{reviewer}{Reviewer identifier.}
  \item{notes}{Notes.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
