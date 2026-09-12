#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L) stop("Usage: run-validation-chunk.R <manifest-dir> <chunk-id> <workflow-file>")
manifest_dir <- normalizePath(args[1L], winslash = "/", mustWork = TRUE)
chunk_id <- args[2L]
workflow_file <- normalizePath(args[3L], winslash = "/", mustWork = TRUE)
library(eyeprocess)
workflow <- new.env(parent = asNamespace("eyeprocess"))
sys.source(workflow_file, workflow)
required <- c("simulator", "fitter", "extractor", "truth_extractor")
missing <- required[!vapply(required, exists, logical(1), envir = workflow, inherits = FALSE)]
if (length(missing)) stop("Workflow file is missing: ", paste(missing, collapse = ", "))
plan <- read_validation_job_manifest(manifest_dir)
run_validation_jobs(
  plan = plan,
  simulator = workflow$simulator,
  fitter = workflow$fitter,
  extractor = workflow$extractor,
  truth_extractor = workflow$truth_extractor,
  diagnostics_extractor = if (exists("diagnostics_extractor", workflow, inherits = FALSE)) workflow$diagnostics_extractor else NULL,
  draws_extractor = if (exists("draws_extractor", workflow, inherits = FALSE)) workflow$draws_extractor else NULL,
  predictions_extractor = if (exists("predictions_extractor", workflow, inherits = FALSE)) workflow$predictions_extractor else NULL,
  output_dir = manifest_dir,
  chunks = chunk_id,
  workers = as.integer(Sys.getenv("EYEPROCESS_WORKERS", "1")),
  backend = Sys.getenv("EYEPROCESS_BACKEND", "sequential"),
  isolation = Sys.getenv("EYEPROCESS_ISOLATION", "callr"),
  timeout_seconds = as.numeric(Sys.getenv("EYEPROCESS_TIMEOUT", "7200")),
  memory_limit_mb = as.numeric(Sys.getenv("EYEPROCESS_MEMORY_MB", "8192")),
  progress = TRUE
)
