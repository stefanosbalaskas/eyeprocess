\name{run_validation_jobs}
\alias{run_validation_jobs}
\title{Run validation jobs with checkpointing and deterministic seeds}
\usage{
run_validation_jobs(plan, simulator, fitter, extractor, truth_extractor, output_dir,
  workers = 1L, backend = c("auto", "sequential", "future"), isolation = c("auto",
  "in_process", "callr"), timeout_seconds = Inf, memory_limit_mb = Inf,
  stale_lock_seconds = 3600, overwrite = FALSE, fail_fast = FALSE,
  progress = interactive(), job_ids = NULL, chunks = NULL, simulation_args = list(),
  fit_args = list(), diagnostics_extractor = NULL, draws_extractor = NULL,
  predictions_extractor = NULL, confidence = 0.95, run_metadata = list())
}
\arguments{
  \item{plan}{Validation plan or manifest directory.}
  \item{simulator}{Simulation function.}
  \item{fitter}{Fitting function receiving the simulated object first.}
  \item{extractor}{Function extracting parameter estimates.}
  \item{truth_extractor}{Function extracting named true parameter values.}
  \item{output_dir}{Validation output directory.}
  \item{workers}{Number of workers.}
  \item{backend}{Sequential or optional `future` backend.}
  \item{isolation}{In-process execution or optional `callr` isolation.}
  \item{timeout_seconds}{Per-job timeout. Enforced only with `callr` isolation.}
  \item{memory_limit_mb}{Best-effort per-job memory limit.}
  \item{stale_lock_seconds}{Age after which an abandoned job lock may be reclaimed.}
  \item{overwrite}{Re-run completed checkpoints.}
  \item{fail_fast}{Stop after the first failed/nonconverged job.}
  \item{progress}{Display progress in sequential mode.}
  \item{job_ids}{Optional subset of job identifiers.}
  \item{chunks}{Optional subset of chunk identifiers.}
  \item{simulation_args}{Additional simulator arguments.}
  \item{fit_args}{Additional fitter arguments.}
  \item{diagnostics_extractor}{Optional diagnostics extractor.}
  \item{draws_extractor}{Optional posterior-draw extractor.}
  \item{predictions_extractor}{Optional prediction extractor.}
  \item{confidence}{Confidence level used when standard errors are supplied.}
  \item{run_metadata}{Named metadata included in the runner fingerprint; use it to record code, prior, or engine variants captured outside function bodies.}
}
\value{The documented eyeprocess object, data frame, plot, report path, or adapter result.}
\description{Part of the research-scale validation, advanced-model, interoperability, storage, adapter, or reproducibility programme. Experimental model functions remain subject to declared evidence gates.}
