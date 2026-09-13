args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) {
  stop("Usage: Rscript run-complete-validation-program.R <corpus> <output_dir> [jobs.rds|jobs.R]")
}
library(eyeprocess)
programme_args <- list(corpus = args[[1L]], output_dir = args[[2L]], overwrite = TRUE)
if (length(args) >= 3L) {
  job_path <- normalizePath(args[[3L]], winslash = "/", mustWork = TRUE)
  jobs <- if (grepl("\\.[Rr]$", job_path)) {
    environment <- new.env(parent = asNamespace("eyeprocess"))
    sys.source(job_path, envir = environment)
    if (!exists("validation_jobs", envir = environment, inherits = FALSE)) {
      stop("An R job file must create a named list called `validation_jobs`.")
    }
    get("validation_jobs", envir = environment, inherits = FALSE)
  } else {
    readRDS(job_path)
  }
  if (!is.list(jobs)) stop("The jobs file must contain a named list of programme arguments.")
  programme_args <- c(programme_args, jobs)
}
result <- do.call(run_eyeprocess_validation_program, programme_args)
print(result)
