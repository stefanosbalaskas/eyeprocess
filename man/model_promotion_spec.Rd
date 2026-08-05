\name{model_promotion_spec}
\alias{model_promotion_spec}
\title{Specify evidence gates for model promotion}
\usage{
model_promotion_spec(model_families = c("dynamic_irtree", "functional_pupil_irt",
  "theory_strategy_irt", "gaze_diffusion_irt"), require_completion = TRUE,
  require_sbc = TRUE, require_misspecification = TRUE,
  require_grouped_validation = TRUE, require_engine_equivalence = TRUE,
  require_empirical_reproduction = TRUE, require_preprocessing_sensitivity = TRUE,
  require_multi_vendor = FALSE)
}
\arguments{
  \item{model_families}{Model families to audit.}
  \item{require_completion}{Require complete Monte Carlo evidence.}
  \item{require_sbc}{Require simulation-based calibration.}
  \item{require_misspecification}{Require declared misspecification studies.}
  \item{require_grouped_validation}{Require grouped out-of-sample validation.}
  \item{require_engine_equivalence}{Require external-engine comparison.}
  \item{require_empirical_reproduction}{Require an empirical reproduction.}
  \item{require_preprocessing_sensitivity}{Require preprocessing/AOI sensitivity.}
  \item{require_multi_vendor}{Require independent multi-vendor evidence.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
