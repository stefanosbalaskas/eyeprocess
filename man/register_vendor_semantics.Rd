\name{register_vendor_semantics}
\alias{register_vendor_semantics}
\title{Register vendor-field semantics}
\usage{
register_vendor_semantics(corpus_path, vendor, native_field, native_meaning,
  canonical_table, canonical_field, unit = NA_character_, transformation = "identity",
  loss_risk = c("none", "low", "moderate", "high", "unsupported"),
  evidence_case_id = NA_character_)
}
\arguments{
  \item{corpus_path}{Corpus directory.}
  \item{vendor}{Vendor.}
  \item{native_field}{Native field and meaning.}
  \item{native_meaning}{Native field and meaning.}
  \item{canonical_table}{Canonical destination.}
  \item{canonical_field}{Canonical destination.}
  \item{unit}{Unit.}
  \item{transformation}{Transformation description.}
  \item{loss_risk}{None, low, moderate, high, or unsupported.}
  \item{evidence_case_id}{Supporting case.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
