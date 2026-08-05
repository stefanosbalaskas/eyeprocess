# Research-scale validation orchestration -------------------------------------

.ve_stop <- function(...) {
  if (exists(".eye_stop", mode = "function", inherits = TRUE)) {
    .eye_stop(...)
  } else {
    stop(paste0(...), call. = FALSE)
  }
}

.ve_warn <- function(...) warning(paste0(...), call. = FALSE)

.ve_or <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

.ve_scalar_int <- function(x, name, minimum = 0L) {
  value <- suppressWarnings(as.integer(x))
  if (length(value) != 1L || is.na(value) || value < minimum) {
    .ve_stop("`", name, "` must be a single integer >= ", minimum, ".")
  }
  value
}

.ve_scalar_num <- function(x, name, minimum = -Inf, maximum = Inf, finite = TRUE) {
  value <- suppressWarnings(as.numeric(x))
  bad <- length(value) != 1L || is.na(value) ||
    (isTRUE(finite) && !is.finite(value)) || value < minimum || value > maximum
  if (bad) .ve_stop("`", name, "` is outside its allowed range.")
  value
}

.ve_safe_name <- function(x, fallback = "unnamed") {
  x <- as.character(x)[1L]
  if (is.na(x) || !nzchar(x)) x <- fallback
  x <- gsub("[^A-Za-z0-9_.-]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) fallback else x
}

.ve_canonical_scalar <- function(x) {
  if (length(x) == 0L || is.null(x)) return("<NULL>")
  if (length(x) > 1L) return(paste(vapply(x, .ve_canonical_scalar, character(1)), collapse = ","))
  if (is.na(x)) return("<NA>")
  if (is.numeric(x)) return(format(x, digits = 17L, scientific = TRUE, trim = TRUE))
  if (is.logical(x)) return(if (isTRUE(x)) "TRUE" else "FALSE")
  enc2utf8(as.character(x))
}

.ve_canonical_row <- function(x) {
  if (is.data.frame(x)) x <- as.list(x[1L, , drop = FALSE])
  if (!is.list(x)) .ve_stop("A design cell must be a list or one-row data frame.")
  nm <- sort(names(x))
  paste0(nm, "=", vapply(x[nm], .ve_canonical_scalar, character(1)), collapse = "|")
}

.ve_hash_int <- function(text, modulus = 2147483629) {
  bytes <- utf8ToInt(enc2utf8(paste(text, collapse = "\n")))
  if (!length(bytes)) return(1L)
  value <- 2166136261 %% modulus
  for (b in bytes) value <- (value * 16777619 + b + 1) %% modulus
  as.integer(value)
}

#' Allocate a deterministic validation seed
#'
#' The seed depends on the canonical design-cell contents, replication number,
#' stream, and base seed. It therefore remains stable when jobs are reordered,
#' split across machines, resumed, or collected from separate directories.
#'
#' @param design A list or one-row data frame describing a design cell.
#' @param replication Positive replication number.
#' @param base_seed Base integer seed.
#' @param stream Optional independent stream number.
#' @return A positive integer suitable for `set.seed()`.
#' @export
validation_seed <- function(design, replication, base_seed = 1L, stream = 1L) {
  replication <- .ve_scalar_int(replication, "replication", 1L)
  base_seed <- .ve_scalar_int(base_seed, "base_seed", 0L)
  stream <- .ve_scalar_int(stream, "stream", 1L)
  key <- paste(.ve_canonical_row(design), replication, base_seed, stream, sep = "|")
  seed <- (.ve_hash_int(key) + base_seed + 104729L * stream) %% 2147483646L
  as.integer(seed + 1L)
}

.ve_expand_grid <- function(grid) {
  if (is.null(grid)) return(data.frame(.scenario = 1L))
  if (is.data.frame(grid)) {
    if (!nrow(grid)) .ve_stop("`grid` must contain at least one design cell.")
    out <- grid
  } else if (is.list(grid)) {
    out <- do.call(expand.grid, c(grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE))
    if (!nrow(out)) .ve_stop("`grid` must contain at least one design cell.")
  } else {
    .ve_stop("`grid` must be a data frame, named list, or NULL.")
  }
  rownames(out) <- NULL
  out
}

.ve_plan_id <- function(grid, replications, base_seed, model_family) {
  cells <- vapply(seq_len(nrow(grid)), function(i) .ve_canonical_row(grid[i, , drop = FALSE]), character(1))
  paste0(.ve_safe_name(model_family, "validation"), "-", sprintf("%010d", .ve_hash_int(c(cells, replications, base_seed))))
}

.ve_object_fingerprint <- function(object, prefix = "object") {
  raw <- serialize(object, NULL, version = 3)
  paste0(prefix, "-", sprintf("%010d", .ve_hash_int(paste(as.integer(raw), collapse = ","))))
}

.ve_plan_fingerprint <- function(jobs, model_family, base_seed) {
  stable <- jobs[setdiff(names(jobs), c("status", "attempt"))]
  .ve_object_fingerprint(list(model_family = model_family, base_seed = base_seed, jobs = stable), "plan")
}

#' Create a deterministic validation job plan
#'
#' @param grid Scenario grid or named list of factor levels.
#' @param replications Replications per design cell.
#' @param base_seed Base seed used for deterministic seed allocation.
#' @param model_family Model-family label.
#' @param plan_id Optional explicit plan identifier.
#' @param chunk_size Number of jobs assigned to each chunk.
#' @param metadata Arbitrary plan metadata.
#' @return An `eye_validation_job_plan` object.
#' @export
validation_job_plan <- function(
    grid = NULL,
    replications = 100L,
    base_seed = 1L,
    model_family = "unspecified",
    plan_id = NULL,
    chunk_size = 1L,
    metadata = list()) {
  grid <- .ve_expand_grid(grid)
  replications <- .ve_scalar_int(replications, "replications", 1L)
  base_seed <- .ve_scalar_int(base_seed, "base_seed", 0L)
  chunk_size <- .ve_scalar_int(chunk_size, "chunk_size", 1L)
  model_family <- .ve_safe_name(model_family, "unspecified")
  if (is.null(plan_id)) plan_id <- .ve_plan_id(grid, replications, base_seed, model_family)
  plan_id <- .ve_safe_name(plan_id, "validation-plan")

  scenario_id <- sprintf("S%05d", seq_len(nrow(grid)))
  jobs <- vector("list", nrow(grid) * replications)
  k <- 0L
  for (s in seq_len(nrow(grid))) {
    design <- grid[s, , drop = FALSE]
    for (r in seq_len(replications)) {
      k <- k + 1L
      seed <- validation_seed(design, r, base_seed)
      jobs[[k]] <- cbind(
        data.frame(
          plan_id = plan_id,
          model_family = model_family,
          scenario_id = scenario_id[s],
          replication = r,
          seed = seed,
          job_id = paste0(scenario_id[s], "-R", sprintf("%06d", r)),
          stringsAsFactors = FALSE
        ),
        design,
        stringsAsFactors = FALSE
      )
    }
  }
  jobs <- do.call(rbind, jobs)
  rownames(jobs) <- NULL
  jobs$chunk_id <- paste0("C", sprintf("%05d", ceiling(seq_len(nrow(jobs)) / chunk_size)))
  jobs$expected_checkpoint <- paste0("job-", jobs$job_id, ".rds")
  jobs$status <- "planned"
  jobs$attempt <- 0L

  out <- list(
    plan_id = plan_id,
    model_family = model_family,
    jobs = jobs,
    grid = grid,
    replications = replications,
    base_seed = base_seed,
    chunk_size = chunk_size,
    metadata = metadata,
    created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    schema_version = "1.0.0"
  )
  out$plan_fingerprint <- .ve_plan_fingerprint(out$jobs, out$model_family, out$base_seed)
  class(out) <- "eye_validation_job_plan"
  out
}

#' @export
print.eye_validation_job_plan <- function(x, ...) {
  cat("Research-scale validation job plan\n")
  cat("Plan:         ", x$plan_id, "\n", sep = "")
  cat("Model family: ", x$model_family, "\n", sep = "")
  cat("Scenarios:    ", nrow(x$grid), "\n", sep = "")
  cat("Replications: ", x$replications, "\n", sep = "")
  cat("Jobs:         ", nrow(x$jobs), "\n", sep = "")
  cat("Chunks:       ", length(unique(x$jobs$chunk_id)), "\n", sep = "")
  cat("Fingerprint:  ", .ve_or(x$plan_fingerprint, "unavailable"), "\n", sep = "")
  invisible(x)
}

#' @export
summary.eye_validation_job_plan <- function(object, ...) {
  data.frame(
    plan_id = object$plan_id,
    model_family = object$model_family,
    scenarios = nrow(object$grid),
    replications = object$replications,
    jobs = nrow(object$jobs),
    chunks = length(unique(object$jobs$chunk_id)),
    base_seed = object$base_seed,
    stringsAsFactors = FALSE
  )
}

.ve_atomic_save_rds <- function(object, path, compress = "xz") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(".", basename(path), "-"), tmpdir = dirname(path))
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(object, temporary, version = 3, compress = compress)
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(temporary, path)) .ve_stop("Could not atomically move checkpoint to `", path, "`.")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.ve_atomic_write_csv <- function(x, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile(pattern = paste0(".", basename(path), "-"), tmpdir = dirname(path), fileext = ".csv")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(x, temporary, row.names = FALSE, na = "")
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(temporary, path)) .ve_stop("Could not atomically move CSV to `", path, "`.")
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

.ve_function_fingerprint <- function(fun) {
  if (!is.function(fun)) return(NA_character_)
  text <- paste(deparse(fun, width.cutoff = 500L), collapse = "\n")
  sprintf("fn-%010d", .ve_hash_int(text))
}

.ve_git_commit <- function(path = ".") {
  git <- Sys.which("git")
  if (!nzchar(git)) return(NA_character_)
  out <- suppressWarnings(tryCatch(
    system2(git, c("-C", normalizePath(path, winslash = "/", mustWork = FALSE), "rev-parse", "HEAD"), stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  ))
  if (length(out) && nzchar(out[1L])) out[1L] else NA_character_
}

.ve_manifest_metadata <- function(plan) {
  list(
    schema_version = plan$schema_version,
    plan_id = plan$plan_id,
    plan_fingerprint = .ve_or(plan$plan_fingerprint, .ve_plan_fingerprint(plan$jobs, plan$model_family, plan$base_seed)),
    model_family = plan$model_family,
    created_utc = plan$created_utc,
    written_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    replications = plan$replications,
    scenarios = nrow(plan$grid),
    jobs = nrow(plan$jobs),
    base_seed = plan$base_seed,
    chunk_size = plan$chunk_size,
    eyeprocess_version = tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) NA_character_),
    r_version = R.version.string,
    platform = R.version$platform,
    git_commit = .ve_git_commit(),
    metadata = plan$metadata
  )
}

#' Write a machine-readable validation manifest
#'
#' @param plan An `eye_validation_job_plan`.
#' @param path Manifest directory.
#' @param overwrite Replace an existing manifest directory.
#' @return Normalized manifest directory.
#' @export
write_validation_job_manifest <- function(plan, path, overwrite = FALSE) {
  if (!inherits(plan, "eye_validation_job_plan")) .ve_stop("`plan` must be created by `validation_job_plan()`.")
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (dir.exists(path) && !isTRUE(overwrite)) .ve_stop("Manifest directory already exists: ", path)
  if (dir.exists(path) && isTRUE(overwrite)) unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "checkpoints"), showWarnings = FALSE)
  dir.create(file.path(path, "logs"), showWarnings = FALSE)
  dir.create(file.path(path, "artifacts"), showWarnings = FALSE)
  .ve_atomic_save_rds(plan, file.path(path, "plan.rds"), compress = "xz")
  .ve_atomic_write_csv(plan$jobs, file.path(path, "jobs.csv"))
  .ve_atomic_write_csv(plan$grid, file.path(path, "design-grid.csv"))
  metadata <- .ve_manifest_metadata(plan)
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(metadata, file.path(path, "manifest.json"), auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  } else {
    saveRDS(metadata, file.path(path, "manifest-metadata.rds"), version = 3)
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Read a validation manifest
#'
#' @param path Manifest directory.
#' @return An `eye_validation_job_plan` with its manifest path attached.
#' @export
read_validation_job_manifest <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  plan_file <- file.path(path, "plan.rds")
  if (!file.exists(plan_file)) .ve_stop("Validation manifest is missing `plan.rds`.")
  plan <- readRDS(plan_file)
  if (!inherits(plan, "eye_validation_job_plan")) .ve_stop("Manifest plan has an unsupported class.")
  attr(plan, "manifest_path") <- path
  plan
}

#' Split a validation plan into independent chunks
#'
#' @param plan Validation plan.
#' @param chunks Optional chunk identifiers.
#' @return Named list of validation plans.
#' @export
split_validation_plan <- function(plan, chunks = NULL) {
  if (!inherits(plan, "eye_validation_job_plan")) .ve_stop("Expected an `eye_validation_job_plan`.")
  available <- unique(plan$jobs$chunk_id)
  if (is.null(chunks)) chunks <- available
  chunks <- intersect(as.character(chunks), available)
  if (!length(chunks)) .ve_stop("No requested chunks exist in the plan.")
  setNames(lapply(chunks, function(chunk) {
    out <- plan
    out$jobs <- plan$jobs[plan$jobs$chunk_id == chunk, , drop = FALSE]
    out$metadata$parent_plan_id <- plan$plan_id
    out$metadata$selected_chunk <- chunk
    out
  }), chunks)
}

.ve_supported_args <- function(fun, args) {
  formals_names <- names(formals(fun))
  if (is.null(formals_names) || "..." %in% formals_names) return(args)
  arg_names <- names(args)
  if (is.null(arg_names)) return(args)
  unnamed <- is.na(arg_names) | !nzchar(arg_names)
  keep <- unnamed | arg_names %in% formals_names
  args[keep]
}

.ve_call <- function(fun, args) {
  do.call(fun, .ve_supported_args(fun, args))
}

.ve_condition_capture <- function(expr) {
  warnings <- character()
  messages <- character()
  value <- withCallingHandlers(
    expr,
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      messages <<- c(messages, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, warnings = unique(warnings), messages = unique(messages))
}

.ve_standardize_estimates <- function(estimates, truth = NULL, confidence = 0.95) {
  if (is.numeric(estimates) && !is.null(names(estimates))) {
    estimates <- data.frame(parameter = names(estimates), estimate = as.numeric(estimates), stringsAsFactors = FALSE)
  }
  if (!is.data.frame(estimates) || !all(c("parameter", "estimate") %in% names(estimates))) {
    .ve_stop("The estimator extractor must return a data frame with `parameter` and `estimate`.")
  }
  estimates$parameter <- as.character(estimates$parameter)
  estimates$estimate <- suppressWarnings(as.numeric(estimates$estimate))
  if (!"std_error" %in% names(estimates)) {
    if ("sd" %in% names(estimates)) estimates$std_error <- estimates$sd else estimates$std_error <- NA_real_
  }
  estimates$std_error <- suppressWarnings(as.numeric(estimates$std_error))
  alpha <- 1 - confidence
  lower_alias <- intersect(c("q2.5", "q025", "q5", "q05"), names(estimates))
  upper_alias <- intersect(c("q97.5", "q975", "q95"), names(estimates))
  if (!"lower" %in% names(estimates)) {
    if (length(lower_alias)) estimates$lower <- estimates[[lower_alias[1L]]] else estimates$lower <- estimates$estimate - stats::qnorm(1 - alpha / 2) * estimates$std_error
  }
  if (!"upper" %in% names(estimates)) {
    if (length(upper_alias)) estimates$upper <- estimates[[upper_alias[1L]]] else estimates$upper <- estimates$estimate + stats::qnorm(1 - alpha / 2) * estimates$std_error
  }
  estimates$lower <- suppressWarnings(as.numeric(estimates$lower))
  estimates$upper <- suppressWarnings(as.numeric(estimates$upper))
  if (is.list(truth)) truth <- unlist(truth, recursive = TRUE, use.names = TRUE)
  if (is.null(truth)) truth <- numeric()
  if (is.numeric(truth) && is.null(names(truth)) && length(truth) == nrow(estimates)) names(truth) <- estimates$parameter
  estimates$truth <- unname(suppressWarnings(as.numeric(truth[match(estimates$parameter, names(truth))])))
  estimates$bias <- estimates$estimate - estimates$truth
  estimates$squared_error <- estimates$bias^2
  estimates$absolute_error <- abs(estimates$bias)
  estimates$relative_bias <- ifelse(is.finite(estimates$truth) & abs(estimates$truth) > sqrt(.Machine$double.eps), estimates$bias / estimates$truth, NA_real_)
  estimates$covered <- ifelse(
    is.finite(estimates$lower) & is.finite(estimates$upper) & is.finite(estimates$truth),
    estimates$lower <= estimates$truth & estimates$upper >= estimates$truth,
    NA
  )
  estimates$interval_width <- estimates$upper - estimates$lower
  estimates
}

.ve_default_diagnostics <- function(fit) {
  converged <- TRUE
  iterations <- NA_integer_
  divergences <- NA_integer_
  max_rhat <- NA_real_
  min_ess_bulk <- NA_real_
  min_ess_tail <- NA_real_
  if (is.list(fit)) {
    if (!is.null(fit$converged)) converged <- isTRUE(fit$converged)
    if (!is.null(fit$iterations)) iterations <- suppressWarnings(as.integer(fit$iterations)[1L])
    if (!is.null(fit$diagnostics) && is.list(fit$diagnostics)) {
      d <- fit$diagnostics
      if (!is.null(d$converged)) converged <- isTRUE(d$converged)
      if (!is.null(d$divergences)) divergences <- suppressWarnings(as.integer(d$divergences)[1L])
      if (!is.null(d$max_rhat)) max_rhat <- suppressWarnings(as.numeric(d$max_rhat)[1L])
      if (!is.null(d$min_ess_bulk)) min_ess_bulk <- suppressWarnings(as.numeric(d$min_ess_bulk)[1L])
      if (!is.null(d$min_ess_tail)) min_ess_tail <- suppressWarnings(as.numeric(d$min_ess_tail)[1L])
    }
  }
  data.frame(
    converged = converged,
    iterations = iterations,
    divergences = divergences,
    max_rhat = max_rhat,
    min_ess_bulk = min_ess_bulk,
    min_ess_tail = min_ess_tail,
    stringsAsFactors = FALSE
  )
}

.ve_job_result <- function(job, status, stage, started, warnings = character(), messages = character(), error = NA_character_, estimates = data.frame(), diagnostics = NULL, draws = NULL, predictions = NULL, artifacts = list()) {
  ended <- Sys.time()
  if (is.null(diagnostics)) diagnostics <- data.frame(converged = identical(status, "complete"), stringsAsFactors = FALSE)
  diagnostics$job_id <- job$job_id
  list(
    schema_version = "1.0.0",
    plan_id = job$plan_id,
    job = job,
    status = status,
    stage = stage,
    started_utc = format(started, tz = "UTC", usetz = TRUE),
    ended_utc = format(ended, tz = "UTC", usetz = TRUE),
    elapsed_seconds = as.numeric(difftime(ended, started, units = "secs")),
    warnings = unique(warnings),
    messages = unique(messages),
    error = error,
    estimates = estimates,
    diagnostics = diagnostics,
    draws = draws,
    predictions = predictions,
    artifacts = artifacts,
    session = list(r_version = R.version.string, platform = R.version$platform)
  )
}

.ve_run_job_core <- function(
    job,
    simulator,
    fitter,
    extractor,
    truth_extractor,
    simulation_args = list(),
    fit_args = list(),
    diagnostics_extractor = NULL,
    draws_extractor = NULL,
    predictions_extractor = NULL,
    confidence = 0.95,
    memory_limit_mb = Inf) {
  started <- Sys.time()
  accumulated_warnings <- character()
  accumulated_messages <- character()
  design_names <- setdiff(names(job), c(
    "plan_id", "model_family", "scenario_id", "replication", "seed", "job_id",
    "chunk_id", "expected_checkpoint", "status", "attempt"
  ))
  design <- as.list(job[design_names])
  set.seed(as.integer(job$seed))

  sim_capture <- tryCatch(
    .ve_condition_capture(.ve_call(simulator, c(design, simulation_args, list(seed = as.integer(job$seed))))),
    error = identity
  )
  if (inherits(sim_capture, "error")) {
    return(.ve_job_result(job, "failed", "simulation", started, error = conditionMessage(sim_capture)))
  }
  accumulated_warnings <- c(accumulated_warnings, sim_capture$warnings)
  accumulated_messages <- c(accumulated_messages, sim_capture$messages)
  simulation <- sim_capture$value

  if (is.finite(memory_limit_mb)) {
    sim_mb <- as.numeric(utils::object.size(simulation)) / 1024^2
    if (sim_mb > memory_limit_mb) {
      return(.ve_job_result(job, "failed", "memory", started, accumulated_warnings, accumulated_messages,
        sprintf("Simulation object size %.2f MB exceeds the configured %.2f MB limit.", sim_mb, memory_limit_mb)))
    }
  }

  fit_capture <- tryCatch(
    .ve_condition_capture(.ve_call(fitter, c(list(simulation), fit_args))),
    error = identity
  )
  if (inherits(fit_capture, "error")) {
    return(.ve_job_result(job, "failed", "fit", started, accumulated_warnings, accumulated_messages, conditionMessage(fit_capture)))
  }
  accumulated_warnings <- c(accumulated_warnings, fit_capture$warnings)
  accumulated_messages <- c(accumulated_messages, fit_capture$messages)
  fit <- fit_capture$value
  if (is.finite(memory_limit_mb)) {
    fit_mb <- as.numeric(utils::object.size(fit)) / 1024^2
    if (fit_mb > memory_limit_mb) {
      return(.ve_job_result(job, "failed", "memory", started, accumulated_warnings, accumulated_messages,
        sprintf("Fit object size %.2f MB exceeds the configured %.2f MB limit.", fit_mb, memory_limit_mb)))
    }
  }

  estimates_capture <- tryCatch(.ve_condition_capture(.ve_call(extractor, list(fit))), error = identity)
  if (inherits(estimates_capture, "error")) {
    return(.ve_job_result(job, "failed", "extract", started, accumulated_warnings, accumulated_messages, conditionMessage(estimates_capture)))
  }
  accumulated_warnings <- c(accumulated_warnings, estimates_capture$warnings)
  accumulated_messages <- c(accumulated_messages, estimates_capture$messages)

  truth_capture <- tryCatch(.ve_condition_capture(.ve_call(truth_extractor, list(simulation))), error = identity)
  if (inherits(truth_capture, "error")) {
    return(.ve_job_result(job, "failed", "truth", started, accumulated_warnings, accumulated_messages, conditionMessage(truth_capture)))
  }
  accumulated_warnings <- c(accumulated_warnings, truth_capture$warnings)
  accumulated_messages <- c(accumulated_messages, truth_capture$messages)

  estimates <- tryCatch(.ve_standardize_estimates(estimates_capture$value, truth_capture$value, confidence), error = identity)
  if (inherits(estimates, "error")) {
    return(.ve_job_result(job, "failed", "standardize", started, accumulated_warnings, accumulated_messages, conditionMessage(estimates)))
  }

  diagnostics <- if (is.function(diagnostics_extractor)) {
    tryCatch(.ve_call(diagnostics_extractor, list(fit)), error = function(e) data.frame(converged = FALSE, diagnostic_error = conditionMessage(e), stringsAsFactors = FALSE))
  } else {
    .ve_default_diagnostics(fit)
  }
  if (!is.data.frame(diagnostics)) diagnostics <- as.data.frame(diagnostics, stringsAsFactors = FALSE)
  if (!"converged" %in% names(diagnostics)) diagnostics$converged <- TRUE

  draws <- if (is.function(draws_extractor)) tryCatch(.ve_call(draws_extractor, list(fit)), error = function(e) structure(NULL, error = conditionMessage(e))) else NULL
  predictions <- if (is.function(predictions_extractor)) tryCatch(.ve_call(predictions_extractor, list(fit, simulation)), error = function(e) structure(NULL, error = conditionMessage(e))) else NULL

  convergence <- suppressWarnings(as.logical(diagnostics$converged))
  converged <- length(convergence) > 0L && any(!is.na(convergence)) && all(convergence[!is.na(convergence)])
  .ve_job_result(
    job, if (converged) "complete" else "nonconverged",
    "complete", started, accumulated_warnings, accumulated_messages, NA_character_, estimates,
    diagnostics, draws, predictions,
    artifacts = list(
      simulation_size_mb = as.numeric(utils::object.size(simulation)) / 1024^2,
      fit_size_mb = as.numeric(utils::object.size(fit)) / 1024^2
    )
  )
}

.ve_run_job_isolated <- function(args, timeout_seconds = Inf, memory_limit_mb = Inf) {
  if (!requireNamespace("callr", quietly = TRUE)) .ve_stop("`callr` is required for isolated validation jobs.")
  environment <- character()
  if (is.finite(memory_limit_mb)) environment <- c(R_MAX_VSIZE = paste0(ceiling(memory_limit_mb), "M"))
  callr::r(
    func = function(payload) do.call(payload$runner, payload$args),
    args = list(payload = list(runner = .ve_run_job_core, args = args)),
    timeout = if (is.finite(timeout_seconds)) timeout_seconds else Inf,
    env = if (length(environment)) environment else NULL,
    spinner = FALSE,
    show = FALSE
  )
}

.ve_checkpoint_path <- function(output_dir, job_id) file.path(output_dir, "checkpoints", paste0("job-", job_id, ".rds"))
.ve_lock_path <- function(output_dir, job_id) file.path(output_dir, "checkpoints", paste0("job-", job_id, ".lock"))

.ve_acquire_lock <- function(path, stale_after_seconds = 3600) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(path) && is.finite(stale_after_seconds)) {
    age <- as.numeric(difftime(Sys.time(), file.info(path)$mtime, units = "secs"))
    if (is.finite(age) && age > stale_after_seconds) unlink(path, recursive = TRUE, force = TRUE)
  }
  acquired <- dir.create(path, recursive = FALSE, showWarnings = FALSE)
  if (isTRUE(acquired)) {
    owner <- data.frame(pid = Sys.getpid(), host = Sys.info()[["nodename"]], acquired_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), stringsAsFactors = FALSE)
    try(utils::write.csv(owner, file.path(path, "owner.csv"), row.names = FALSE), silent = TRUE)
  }
  isTRUE(acquired)
}

.ve_release_lock <- function(path) unlink(path, recursive = TRUE, force = TRUE)

.ve_run_and_checkpoint <- function(job, output_dir, runner_args, isolation, timeout_seconds, memory_limit_mb, overwrite, fail_fast, stale_lock_seconds) {
  checkpoint <- .ve_checkpoint_path(output_dir, job$job_id)
  if (file.exists(checkpoint) && !isTRUE(overwrite)) {
    existing <- tryCatch(readRDS(checkpoint), error = identity)
    if (!inherits(existing, "error")) return(existing)
    return(.ve_job_result(job, "corrupt", "checkpoint", Sys.time(), error = conditionMessage(existing)))
  }
  lock <- .ve_lock_path(output_dir, job$job_id)
  if (!.ve_acquire_lock(lock, stale_after_seconds = stale_lock_seconds)) {
    return(.ve_job_result(job, "locked", "lock", Sys.time(), error = "Another process holds this job lock."))
  }
  on.exit(.ve_release_lock(lock), add = TRUE)
  runner_args$job <- job
  result <- tryCatch(
    if (identical(isolation, "callr")) {
      .ve_run_job_isolated(runner_args, timeout_seconds, memory_limit_mb)
    } else {
      .ve_run_job_core(
        job = runner_args$job,
        simulator = runner_args$simulator,
        fitter = runner_args$fitter,
        extractor = runner_args$extractor,
        truth_extractor = runner_args$truth_extractor,
        simulation_args = runner_args$simulation_args,
        fit_args = runner_args$fit_args,
        diagnostics_extractor = runner_args$diagnostics_extractor,
        draws_extractor = runner_args$draws_extractor,
        predictions_extractor = runner_args$predictions_extractor,
        confidence = runner_args$confidence,
        memory_limit_mb = memory_limit_mb
      )
    },
    error = function(e) .ve_job_result(job, "failed", "runner", Sys.time(), error = conditionMessage(e))
  )
  .ve_atomic_save_rds(result, checkpoint, compress = "xz")
  log_row <- data.frame(
    job_id = job$job_id,
    scenario_id = job$scenario_id,
    replication = job$replication,
    seed = job$seed,
    status = result$status,
    stage = result$stage,
    elapsed_seconds = result$elapsed_seconds,
    warnings = length(result$warnings),
    error = .ve_or(result$error, NA_character_),
    checkpoint = normalizePath(checkpoint, winslash = "/", mustWork = TRUE),
    stringsAsFactors = FALSE
  )
  .ve_atomic_write_csv(log_row, file.path(output_dir, "logs", paste0("job-", job$job_id, ".csv")))
  if (isTRUE(fail_fast) && result$status %in% c("failed", "nonconverged")) .ve_stop("Validation job failed: ", job$job_id, " (", result$stage, ")")
  result
}

.ve_backend <- function(backend, workers) {
  backend <- match.arg(backend, c("auto", "sequential", "future"))
  if (backend == "auto") {
    if (workers > 1L && requireNamespace("future.apply", quietly = TRUE) && requireNamespace("future", quietly = TRUE)) "future" else "sequential"
  } else backend
}

.ve_isolation <- function(isolation, timeout_seconds) {
  isolation <- match.arg(isolation, c("auto", "in_process", "callr"))
  if (isolation == "auto") {
    if (is.finite(timeout_seconds) && requireNamespace("callr", quietly = TRUE)) "callr" else "in_process"
  } else isolation
}

.ve_update_status_table <- function(output_dir, plan) {
  logs <- list.files(file.path(output_dir, "logs"), pattern = "^job-.*\\.csv$", full.names = TRUE)
  if (!length(logs)) return(invisible(NULL))
  rows <- lapply(logs, function(path) tryCatch(utils::read.csv(path, stringsAsFactors = FALSE), error = function(e) NULL))
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(invisible(NULL))
  status <- do.call(rbind, rows)
  status <- status[match(plan$jobs$job_id, status$job_id, nomatch = 0L), , drop = FALSE]
  .ve_atomic_write_csv(status, file.path(output_dir, "job-status.csv"))
  invisible(status)
}

#' Run validation jobs with checkpointing and deterministic seeds
#'
#' @param plan Validation plan or manifest directory.
#' @param simulator Simulation function.
#' @param fitter Fitting function receiving the simulated object first.
#' @param extractor Function extracting parameter estimates.
#' @param truth_extractor Function extracting named true parameter values.
#' @param output_dir Validation output directory.
#' @param workers Number of workers.
#' @param backend Sequential or optional `future` backend.
#' @param isolation In-process execution or optional `callr` isolation.
#' @param timeout_seconds Per-job timeout. Enforced only with `callr` isolation.
#' @param memory_limit_mb Best-effort per-job memory limit.
#' @param stale_lock_seconds Age after which an abandoned job lock may be reclaimed.
#' @param overwrite Re-run completed checkpoints.
#' @param fail_fast Stop after the first failed/nonconverged job.
#' @param progress Display progress in sequential mode.
#' @param job_ids Optional subset of job identifiers.
#' @param chunks Optional subset of chunk identifiers.
#' @param simulation_args Additional simulator arguments.
#' @param fit_args Additional fitter arguments.
#' @param diagnostics_extractor Optional diagnostics extractor.
#' @param draws_extractor Optional posterior-draw extractor.
#' @param predictions_extractor Optional prediction extractor.
#' @param confidence Confidence level used when standard errors are supplied.
#' @param run_metadata Named metadata included in the runner fingerprint; use it to record code, prior, or engine variants captured outside function bodies.
#' @return An `eye_validation_run` object.
#' @export
run_validation_jobs <- function(
    plan,
    simulator,
    fitter,
    extractor,
    truth_extractor,
    output_dir,
    workers = 1L,
    backend = c("auto", "sequential", "future"),
    isolation = c("auto", "in_process", "callr"),
    timeout_seconds = Inf,
    memory_limit_mb = Inf,
    stale_lock_seconds = 3600,
    overwrite = FALSE,
    fail_fast = FALSE,
    progress = interactive(),
    job_ids = NULL,
    chunks = NULL,
    simulation_args = list(),
    fit_args = list(),
    diagnostics_extractor = NULL,
    draws_extractor = NULL,
    predictions_extractor = NULL,
    confidence = 0.95,
    run_metadata = list()) {
  if (is.character(plan) && length(plan) == 1L) plan <- read_validation_job_manifest(plan)
  if (!inherits(plan, "eye_validation_job_plan")) .ve_stop("`plan` must be a validation plan or manifest path.")
  functions <- list(simulator, fitter, extractor, truth_extractor)
  if (!all(vapply(functions, is.function, logical(1)))) .ve_stop("Simulator, fitter, extractor, and truth extractor must be functions.")
  workers <- .ve_scalar_int(workers, "workers", 1L)
  timeout_seconds <- .ve_scalar_num(timeout_seconds, "timeout_seconds", 0, Inf, finite = FALSE)
  memory_limit_mb <- .ve_scalar_num(memory_limit_mb, "memory_limit_mb", 0, Inf, finite = FALSE)
  stale_lock_seconds <- .ve_scalar_num(stale_lock_seconds, "stale_lock_seconds", 0, Inf, finite = FALSE)
  confidence <- .ve_scalar_num(confidence, "confidence", 0, 1)
  if (confidence <= 0 || confidence >= 1) .ve_stop("`confidence` must lie strictly between 0 and 1.")
  if (!is.list(run_metadata)) .ve_stop("`run_metadata` must be a list.")
  backend <- .ve_backend(backend, workers)
  isolation <- .ve_isolation(isolation, timeout_seconds)
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  plan_file <- file.path(output_dir, "plan.rds")
  if (!dir.exists(output_dir)) {
    write_validation_job_manifest(plan, output_dir, overwrite = FALSE)
  } else if (!file.exists(plan_file)) {
    existing <- list.files(output_dir, all.files = TRUE, no.. = TRUE)
    if (length(existing)) .ve_stop("Existing output directory is not a validation manifest: ", output_dir)
    write_validation_job_manifest(plan, output_dir, overwrite = TRUE)
  } else {
    existing_plan <- readRDS(plan_file)
    existing_fingerprint <- if (inherits(existing_plan, "eye_validation_job_plan")) .ve_or(existing_plan$plan_fingerprint, .ve_plan_fingerprint(existing_plan$jobs, existing_plan$model_family, existing_plan$base_seed)) else NA_character_
    requested_fingerprint <- .ve_or(plan$plan_fingerprint, .ve_plan_fingerprint(plan$jobs, plan$model_family, plan$base_seed))
    if (!inherits(existing_plan, "eye_validation_job_plan") || !identical(existing_plan$plan_id, plan$plan_id) || !identical(existing_fingerprint, requested_fingerprint)) {
      .ve_stop("The output directory belongs to a different validation plan or plan revision.")
    }
  }
  jobs <- plan$jobs
  if (!is.null(job_ids)) jobs <- jobs[jobs$job_id %in% as.character(job_ids), , drop = FALSE]
  if (!is.null(chunks)) jobs <- jobs[jobs$chunk_id %in% as.character(chunks), , drop = FALSE]
  if (!nrow(jobs)) .ve_stop("No jobs were selected.")

  runner_args <- list(
    simulator = simulator,
    fitter = fitter,
    extractor = extractor,
    truth_extractor = truth_extractor,
    simulation_args = simulation_args,
    fit_args = fit_args,
    diagnostics_extractor = diagnostics_extractor,
    draws_extractor = draws_extractor,
    predictions_extractor = predictions_extractor,
    confidence = confidence
  )
  runner_manifest <- list(
    schema_version = "1.0.0",
    plan_fingerprint = .ve_or(plan$plan_fingerprint, .ve_plan_fingerprint(plan$jobs, plan$model_family, plan$base_seed)),
    function_fingerprints = c(
      simulator = .ve_function_fingerprint(simulator), fitter = .ve_function_fingerprint(fitter),
      extractor = .ve_function_fingerprint(extractor), truth_extractor = .ve_function_fingerprint(truth_extractor),
      diagnostics_extractor = .ve_function_fingerprint(diagnostics_extractor), draws_extractor = .ve_function_fingerprint(draws_extractor),
      predictions_extractor = .ve_function_fingerprint(predictions_extractor)
    ),
    argument_fingerprint = .ve_object_fingerprint(list(simulation_args = simulation_args, fit_args = fit_args, confidence = confidence, run_metadata = run_metadata), "runner-args"),
    run_metadata = run_metadata
  )
  runner_manifest$fingerprint <- .ve_object_fingerprint(runner_manifest, "runner")
  runner_path <- file.path(output_dir, "runner-manifest.rds")
  if (file.exists(runner_path)) {
    existing_runner <- tryCatch(readRDS(runner_path), error = identity)
    if (inherits(existing_runner, "error") || !identical(existing_runner$fingerprint, runner_manifest$fingerprint)) .ve_stop("Runner functions or declared run metadata differ from the existing validation execution.")
  } else {
    .ve_atomic_save_rds(runner_manifest, runner_path, compress = "xz")
  }
  one <- function(i) {
    job <- as.list(jobs[i, , drop = FALSE])
    .ve_run_and_checkpoint(job, output_dir, runner_args, isolation, timeout_seconds, memory_limit_mb, overwrite, fail_fast, stale_lock_seconds)
  }
  started <- Sys.time()
  if (backend == "future") {
    old_plan <- future::plan()
    on.exit(future::plan(old_plan), add = TRUE)
    future::plan(future::multisession, workers = workers)
    results <- future.apply::future_lapply(seq_len(nrow(jobs)), one, future.seed = TRUE)
  } else {
    results <- vector("list", nrow(jobs))
    for (i in seq_len(nrow(jobs))) {
      results[[i]] <- one(i)
      if (isTRUE(progress)) message(sprintf("[%d/%d] %s: %s", i, nrow(jobs), jobs$job_id[i], results[[i]]$status))
    }
  }
  .ve_update_status_table(output_dir, plan)
  out <- list(
    plan = plan,
    selected_jobs = jobs,
    results = results,
    output_dir = output_dir,
    backend = backend,
    isolation = isolation,
    workers = workers,
    started_utc = format(started, tz = "UTC", usetz = TRUE),
    elapsed_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
    function_fingerprints = runner_manifest$function_fingerprints,
    runner_fingerprint = runner_manifest$fingerprint,
    run_metadata = run_metadata
  )
  class(out) <- "eye_validation_run"
  out
}

#' Resume incomplete or failed validation jobs
#'
#' @param plan Validation plan or manifest path.
#' @param output_dir Validation output directory.
#' @param retry Which statuses to re-run.
#' @param ... Passed to `run_validation_jobs()`.
#' @return An `eye_validation_run`.
#' @export
resume_validation_jobs <- function(plan, output_dir, retry = c("missing", "failed", "nonconverged", "locked", "corrupt"), ...) {
  if (is.character(plan) && length(plan) == 1L) plan <- read_validation_job_manifest(plan)
  if (!inherits(plan, "eye_validation_job_plan")) .ve_stop("Expected a validation plan.")
  output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  statuses <- vapply(plan$jobs$job_id, function(id) {
    path <- .ve_checkpoint_path(output_dir, id)
    if (!file.exists(path)) return("missing")
    tryCatch(as.character(readRDS(path)$status)[1L], error = function(e) "corrupt")
  }, character(1))
  selected <- plan$jobs$job_id[statuses %in% retry]
  if (!length(selected)) {
    out <- list(plan = plan, selected_jobs = plan$jobs[0, , drop = FALSE], results = list(), output_dir = output_dir, backend = "none", isolation = "none", workers = 0L, elapsed_seconds = 0)
    class(out) <- "eye_validation_run"
    return(out)
  }
  run_validation_jobs(plan = plan, output_dir = output_dir, job_ids = selected, overwrite = TRUE, ...)
}

#' @export
print.eye_validation_run <- function(x, ...) {
  statuses <- table(vapply(x$results, function(z) z$status, character(1)))
  cat("Validation execution run\n")
  cat("Jobs:      ", length(x$results), "\n", sep = "")
  cat("Backend:   ", x$backend, "\n", sep = "")
  cat("Isolation: ", x$isolation, "\n", sep = "")
  cat("Elapsed:   ", format(round(x$elapsed_seconds, 2), nsmall = 2), " seconds\n", sep = "")
  if (length(statuses)) print(statuses)
  invisible(x)
}

.ve_bind_rows <- function(rows) {
  rows <- Filter(function(x) is.data.frame(x) && nrow(x), rows)
  if (!length(rows)) return(data.frame())
  columns <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(x) {
    missing <- setdiff(columns, names(x))
    for (nm in missing) x[[nm]] <- NA
    x[columns]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.ve_annotate_job_frame <- function(frame, result) {
  if (!is.data.frame(frame) || !nrow(frame)) return(data.frame())
  job <- result$job
  metadata <- setdiff(names(job), names(frame))
  for (nm in metadata) frame[[nm]] <- job[[nm]]
  frame$status <- result$status
  frame$stage <- result$stage
  frame$elapsed_seconds <- result$elapsed_seconds
  frame$warning_count <- length(result$warnings)
  frame$error <- result$error
  frame
}

#' Collect validation checkpoints from one or more directories
#'
#' @param path Manifest/output directory or character vector of directories.
#' @param plan Optional validation plan.
#' @param strict Fail when duplicate job identifiers disagree.
#' @return An `eye_validation_collection`.
#' @export
collect_validation_jobs <- function(path, plan = NULL, strict = TRUE) {
  paths <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- unique(unlist(lapply(paths, function(root) list.files(file.path(root, "checkpoints"), pattern = "^job-.*\\.rds$", full.names = TRUE))))
  results <- lapply(files, function(file) tryCatch(readRDS(file), error = function(e) list(status = "corrupt", error = conditionMessage(e), source_file = file)))
  valid <- vapply(results, function(x) is.list(x) && !is.null(x$job$job_id), logical(1))
  corrupt <- results[!valid]
  results <- results[valid]
  ids <- vapply(results, function(x) as.character(x$job$job_id)[1L], character(1))
  if (anyDuplicated(ids)) {
    groups <- split(seq_along(ids), ids)
    keep <- integer()
    for (group in groups) {
      if (length(group) == 1L) {
        keep <- c(keep, group)
      } else {
        fingerprints <- vapply(results[group], function(x) {
          raw <- serialize(x, NULL, version = 3)
          sprintf("%010d", .ve_hash_int(paste(as.integer(raw), collapse = ",")))
        }, character(1))
        if (isTRUE(strict) && length(unique(fingerprints)) > 1L) .ve_stop("Conflicting checkpoints were found for job `", ids[group[1L]], "`.")
        keep <- c(keep, tail(group, 1L))
      }
    }
    results <- results[keep]
    ids <- ids[keep]
  }
  if (is.null(plan)) {
    candidate <- file.path(paths[1L], "plan.rds")
    if (file.exists(candidate)) plan <- readRDS(candidate)
  }
  estimates <- .ve_bind_rows(lapply(results, function(x) .ve_annotate_job_frame(x$estimates, x)))
  diagnostics <- .ve_bind_rows(lapply(results, function(x) .ve_annotate_job_frame(x$diagnostics, x)))
  predictions <- .ve_bind_rows(lapply(results, function(x) .ve_annotate_job_frame(x$predictions, x)))
  draws <- .ve_bind_rows(lapply(results, function(x) .ve_annotate_job_frame(x$draws, x)))
  job_summary <- .ve_bind_rows(lapply(results, function(x) {
    job <- as.data.frame(x$job, stringsAsFactors = FALSE)
    job$status <- x$status
    job$stage <- x$stage
    job$elapsed_seconds <- x$elapsed_seconds
    job$warning_count <- length(x$warnings)
    job$message_count <- length(x$messages)
    job$error <- x$error
    job
  }))
  out <- list(
    plan = plan,
    paths = paths,
    results = results,
    jobs = job_summary,
    estimates = estimates,
    diagnostics = diagnostics,
    predictions = predictions,
    draws = draws,
    corrupt = corrupt,
    collected_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  class(out) <- "eye_validation_collection"
  out
}

#' @export
print.eye_validation_collection <- function(x, ...) {
  cat("Collected validation evidence\n")
  cat("Jobs:        ", nrow(x$jobs), "\n", sep = "")
  cat("Estimates:   ", nrow(x$estimates), "\n", sep = "")
  cat("Diagnostics: ", nrow(x$diagnostics), "\n", sep = "")
  cat("Draws:       ", nrow(x$draws), "\n", sep = "")
  cat("Predictions: ", nrow(x$predictions), "\n", sep = "")
  if (nrow(x$jobs)) print(table(x$jobs$status, useNA = "ifany"))
  invisible(x)
}

.ve_group_summary <- function(data, keys, fun) {
  if (!nrow(data)) return(data.frame())
  keys <- intersect(keys, names(data))
  if (!length(keys)) return(fun(data))
  encoded <- lapply(data[keys], function(value) ifelse(is.na(value), "<NA>", enc2utf8(as.character(value))))
  group_key <- do.call(paste, c(encoded, sep = "\r"))
  groups <- split(seq_len(nrow(data)), group_key, drop = TRUE)
  out <- lapply(groups, function(index) cbind(data[index[1L], keys, drop = FALSE], fun(data[index, , drop = FALSE])))
  .ve_bind_rows(out)
}

#' Summarize parameter recovery
#'
#' @param x Validation collection or estimates data frame.
#' @param by Additional grouping columns.
#' @return Parameter-level bias, RMSE, coverage, and standard-error metrics.
#' @export
validation_recovery_summary <- function(x, by = character()) {
  d <- if (inherits(x, "eye_validation_collection")) x$estimates else x
  if (!is.data.frame(d)) .ve_stop("Expected a validation collection or estimates data frame.")
  required <- c("parameter", "estimate", "truth")
  if (!all(required %in% names(d))) .ve_stop("Recovery data require: ", paste(required, collapse = ", "))
  if (!"status" %in% names(d)) d$status <- "complete"
  if (!"std_error" %in% names(d)) d$std_error <- NA_real_
  if (!"relative_bias" %in% names(d)) {
    denominator <- abs(d$truth)
    d$relative_bias <- ifelse(is.finite(denominator) & denominator > sqrt(.Machine$double.eps),
                              (d$estimate - d$truth) / denominator, NA_real_)
  }
  if (!"covered" %in% names(d)) {
    if (all(c("lower", "upper") %in% names(d))) {
      d$covered <- is.finite(d$lower) & is.finite(d$upper) & is.finite(d$truth) &
        d$lower <= d$truth & d$truth <= d$upper
    } else {
      d$covered <- NA
    }
  }
  if (!"interval_width" %in% names(d)) {
    d$interval_width <- if (all(c("lower", "upper") %in% names(d))) d$upper - d$lower else NA_real_
  }
  keys <- unique(c(intersect(c("model_family", "scenario_id"), names(d)), by, "parameter"))
  .ve_group_summary(d, keys, function(z) {
    ok <- is.finite(z$estimate) & is.finite(z$truth) & !(z$status %in% c("failed", "nonconverged"))
    covered <- ok & !is.na(z$covered)
    model_se <- ok & is.finite(z$std_error)
    data.frame(
      replications = if ("replication" %in% names(z)) length(unique(z$replication)) else nrow(z),
      successful = sum(ok),
      failure_rate = mean(!ok),
      mean_truth = if (any(ok)) mean(z$truth[ok]) else NA_real_,
      mean_estimate = if (any(ok)) mean(z$estimate[ok]) else NA_real_,
      bias = if (any(ok)) mean(z$estimate[ok] - z$truth[ok]) else NA_real_,
      absolute_bias = if (any(ok)) mean(abs(z$estimate[ok] - z$truth[ok])) else NA_real_,
      relative_bias = if (any(ok & is.finite(z$relative_bias))) mean(z$relative_bias[ok & is.finite(z$relative_bias)]) else NA_real_,
      rmse = if (any(ok)) sqrt(mean((z$estimate[ok] - z$truth[ok])^2)) else NA_real_,
      empirical_sd = if (sum(ok) > 1L) stats::sd(z$estimate[ok]) else NA_real_,
      mean_model_se = if (any(model_se)) mean(z$std_error[model_se]) else NA_real_,
      se_ratio = if (any(model_se) && sum(ok) > 1L) mean(z$std_error[model_se]) / stats::sd(z$estimate[ok]) else NA_real_,
      coverage = if (any(covered)) mean(z$covered[covered]) else NA_real_,
      mean_interval_width = if (any(covered)) mean(z$interval_width[covered]) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
}

#' Summarize convergence and execution failures
#'
#' @param x Validation collection or job table.
#' @param by Grouping columns.
#' @return Failure summary.
#' @export
validation_failure_summary <- function(x, by = c("model_family", "scenario_id")) {
  d <- if (inherits(x, "eye_validation_collection")) x$jobs else x
  if (!is.data.frame(d)) .ve_stop("Expected a validation collection or job table.")
  keys <- intersect(by, names(d))
  .ve_group_summary(d, keys, function(z) data.frame(
    jobs = nrow(z),
    successes = sum(z$status == "complete", na.rm = TRUE),
    nonconverged = sum(z$status == "nonconverged", na.rm = TRUE),
    failed = sum(z$status == "failed", na.rm = TRUE),
    locked = sum(z$status == "locked", na.rm = TRUE),
    failure_rate = mean(z$status != "complete", na.rm = TRUE),
    warning_rate = mean(z$warning_count > 0, na.rm = TRUE),
    stringsAsFactors = FALSE
  ))
}

#' Summarize validation runtime and checkpoint scale
#'
#' @param x Validation collection or job table.
#' @param by Grouping columns.
#' @return Runtime summary.
#' @export
validation_runtime_summary <- function(x, by = c("model_family", "scenario_id")) {
  d <- if (inherits(x, "eye_validation_collection")) x$jobs else x
  if (!is.data.frame(d) || !"elapsed_seconds" %in% names(d)) .ve_stop("Runtime data are unavailable.")
  keys <- intersect(by, names(d))
  .ve_group_summary(d, keys, function(z) {
    value <- z$elapsed_seconds[is.finite(z$elapsed_seconds)]
    data.frame(
      jobs = nrow(z),
      total_seconds = sum(value),
      mean_seconds = if (length(value)) mean(value) else NA_real_,
      median_seconds = if (length(value)) stats::median(value) else NA_real_,
      p90_seconds = if (length(value)) unname(stats::quantile(value, 0.90, names = FALSE)) else NA_real_,
      max_seconds = if (length(value)) max(value) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
}

#' Summarize prediction calibration
#'
#' Prediction rows must contain `observed` and `predicted`. Optional `weight`
#' values are honored.
#'
#' @param x Validation collection or prediction data frame.
#' @param by Grouping columns.
#' @param bins Number of reliability bins.
#' @return Calibration summary.
#' @export
validation_calibration_summary <- function(x, by = c("model_family", "scenario_id"), bins = 10L) {
  d <- if (inherits(x, "eye_validation_collection")) x$predictions else x
  if (!is.data.frame(d) || !all(c("observed", "predicted") %in% names(d))) .ve_stop("Prediction data require `observed` and `predicted` columns.")
  bins <- .ve_scalar_int(bins, "bins", 2L)
  d$observed <- as.numeric(d$observed)
  if (any(is.finite(d$observed) & !d$observed %in% 0:1)) .ve_stop("Calibration outcomes must be binary 0/1.")
  d$predicted <- pmin(pmax(as.numeric(d$predicted), 1e-8), 1 - 1e-8)
  if (!"weight" %in% names(d)) d$weight <- 1
  keys <- intersect(by, names(d))
  .ve_group_summary(d, keys, function(z) {
    ok <- is.finite(z$observed) & is.finite(z$predicted) & is.finite(z$weight) & z$weight > 0
    z <- z[ok, , drop = FALSE]
    if (!nrow(z)) return(data.frame(n = 0L, calibration_intercept = NA_real_, calibration_slope = NA_real_, brier = NA_real_, log_loss = NA_real_, ece = NA_real_))
    lp <- stats::qlogis(z$predicted)
    intercept_fit <- tryCatch(stats::glm(z$observed ~ 1 + offset(lp), family = stats::binomial(), weights = z$weight), error = function(e) NULL)
    slope_fit <- tryCatch(stats::glm(z$observed ~ lp, family = stats::binomial(), weights = z$weight), error = function(e) NULL)
    bin <- cut(z$predicted, breaks = seq(0, 1, length.out = bins + 1L), include.lowest = TRUE)
    ece <- sum(vapply(split(seq_len(nrow(z)), bin), function(index) {
      sum(z$weight[index]) / sum(z$weight) * abs(stats::weighted.mean(z$observed[index], z$weight[index]) - stats::weighted.mean(z$predicted[index], z$weight[index]))
    }, numeric(1)))
    data.frame(
      n = nrow(z),
      calibration_intercept = if (!is.null(intercept_fit)) stats::coef(intercept_fit)[1L] else NA_real_,
      calibration_slope = if (!is.null(slope_fit) && length(stats::coef(slope_fit)) > 1L) stats::coef(slope_fit)[2L] else NA_real_,
      brier = stats::weighted.mean((z$observed - z$predicted)^2, z$weight),
      log_loss = -stats::weighted.mean(z$observed * log(z$predicted) + (1 - z$observed) * log1p(-z$predicted), z$weight),
      ece = ece,
      stringsAsFactors = FALSE
    )
  })
}

#' Summarize simulation-based calibration ranks
#'
#' Draw rows must contain `parameter`, `draw`, and `truth`.
#'
#' @param x Validation collection or posterior draws data frame.
#' @param by Grouping columns.
#' @param bins Rank-histogram bins.
#' @return SBC summary.
#' @export
validation_sbc_summary <- function(x, by = c("model_family", "scenario_id"), bins = 10L) {
  d <- if (inherits(x, "eye_validation_collection")) x$draws else x
  if (!is.data.frame(d) || !all(c("parameter", "draw", "truth", "job_id") %in% names(d))) .ve_stop("SBC draws require `job_id`, `parameter`, `draw`, and `truth`.")
  bins <- .ve_scalar_int(bins, "bins", 2L)
  rank_rows <- lapply(split(seq_len(nrow(d)), interaction(d$job_id, d$parameter, drop = TRUE)), function(index) {
    z <- d[index, , drop = FALSE]
    z <- z[is.finite(z$draw) & is.finite(z$truth), , drop = FALSE]
    if (!nrow(z)) return(NULL)
    base <- z[1L, unique(c(intersect(by, names(z)), "job_id", "parameter")), drop = FALSE]
    cbind(base, data.frame(rank = sum(z$draw < z$truth[1L]) + 0.5 * sum(z$draw == z$truth[1L]), draws = nrow(z), stringsAsFactors = FALSE))
  })
  ranks <- .ve_bind_rows(rank_rows)
  keys <- unique(c(intersect(by, names(ranks)), "parameter"))
  .ve_group_summary(ranks, keys, function(z) {
    scaled <- (z$rank + 0.5) / (z$draws + 1)
    counts <- tabulate(pmin(bins, floor(scaled * bins) + 1L), nbins = bins)
    expected <- sum(counts) / bins
    chi_square <- if (expected > 0) sum((counts - expected)^2 / expected) else NA_real_
    p_value <- if (is.finite(chi_square)) stats::pchisq(chi_square, df = bins - 1L, lower.tail = FALSE) else NA_real_
    data.frame(
      replications = nrow(z),
      mean_scaled_rank = mean(scaled),
      rank_variance = stats::var(scaled),
      chi_square = chi_square,
      p_value = p_value,
      stringsAsFactors = FALSE
    )
  })
}

#' Specify completion and scientific-promotion thresholds
#'
#' @param required_replications Required completed replications per scenario.
#' @param max_failure_rate Maximum tolerated job failure rate.
#' @param max_absolute_bias Maximum absolute mean bias.
#' @param max_rmse Maximum RMSE.
#' @param min_coverage Minimum interval coverage.
#' @param max_coverage Maximum interval coverage.
#' @param max_rhat Maximum acceptable R-hat.
#' @param min_ess_bulk Minimum bulk effective sample size.
#' @param max_divergence_rate Maximum divergence rate.
#' @param require_sbc Require passing SBC uniformity evidence.
#' @param require_empirical_reproduction Require empirical-reproduction evidence.
#' @return An `eye_validation_thresholds` object.
#' @export
validation_thresholds <- function(
    required_replications = 100L,
    max_failure_rate = 0.05,
    max_absolute_bias = 0.10,
    max_rmse = Inf,
    min_coverage = 0.90,
    max_coverage = 0.99,
    max_rhat = 1.01,
    min_ess_bulk = 400,
    max_divergence_rate = 0.01,
    require_sbc = TRUE,
    require_empirical_reproduction = TRUE) {
  structure(list(
    required_replications = .ve_scalar_int(required_replications, "required_replications", 1L),
    max_failure_rate = .ve_scalar_num(max_failure_rate, "max_failure_rate", 0, 1),
    max_absolute_bias = .ve_scalar_num(max_absolute_bias, "max_absolute_bias", 0, Inf, finite = FALSE),
    max_rmse = .ve_scalar_num(max_rmse, "max_rmse", 0, Inf, finite = FALSE),
    min_coverage = .ve_scalar_num(min_coverage, "min_coverage", 0, 1),
    max_coverage = .ve_scalar_num(max_coverage, "max_coverage", 0, 1),
    max_rhat = .ve_scalar_num(max_rhat, "max_rhat", 1, Inf),
    min_ess_bulk = .ve_scalar_num(min_ess_bulk, "min_ess_bulk", 0, Inf, finite = FALSE),
    max_divergence_rate = .ve_scalar_num(max_divergence_rate, "max_divergence_rate", 0, 1),
    require_sbc = isTRUE(require_sbc),
    require_empirical_reproduction = isTRUE(require_empirical_reproduction)
  ), class = "eye_validation_thresholds")
}

#' Audit whether a validation programme is complete
#'
#' @param x Validation collection.
#' @param thresholds Validation thresholds.
#' @param empirical_reproduction Optional empirical-reproduction evidence required when configured in `thresholds`.
#' @return An `eye_validation_completion_audit`.
#' @export
audit_validation_completion <- function(x, thresholds = validation_thresholds(), empirical_reproduction = NULL) {
  if (!inherits(x, "eye_validation_collection")) .ve_stop("Expected an `eye_validation_collection`.")
  if (!inherits(thresholds, "eye_validation_thresholds")) .ve_stop("`thresholds` must be created by `validation_thresholds()`.")
  expected <- if (!is.null(x$plan)) x$plan$jobs else data.frame()
  observed <- unique(x$jobs$job_id)
  missing_jobs <- if (nrow(expected)) expected[!expected$job_id %in% observed, , drop = FALSE] else data.frame()
  failure <- validation_failure_summary(x)
  recovery <- if (nrow(x$estimates)) validation_recovery_summary(x) else data.frame()
  sbc <- if (nrow(x$draws)) tryCatch(validation_sbc_summary(x), error = function(e) data.frame()) else data.frame()
  diag <- x$diagnostics
  divergence_rate <- if (nrow(diag) && "divergences" %in% names(diag)) mean(diag$divergences > 0, na.rm = TRUE) else NA_real_
  max_rhat <- if (nrow(diag) && "max_rhat" %in% names(diag) && any(is.finite(diag$max_rhat))) max(diag$max_rhat, na.rm = TRUE) else NA_real_
  min_ess <- if (nrow(diag) && "min_ess_bulk" %in% names(diag) && any(is.finite(diag$min_ess_bulk))) min(diag$min_ess_bulk, na.rm = TRUE) else NA_real_
  gates <- data.frame(
    gate = c("all_jobs_present", "replications", "failure_rate", "absolute_bias", "rmse", "coverage", "rhat", "ess_bulk", "divergences", "sbc", "empirical_reproduction"),
    required = c(TRUE, thresholds$required_replications, thresholds$max_failure_rate, thresholds$max_absolute_bias, thresholds$max_rmse, paste0(thresholds$min_coverage, "-", thresholds$max_coverage), thresholds$max_rhat, thresholds$min_ess_bulk, thresholds$max_divergence_rate, thresholds$require_sbc, thresholds$require_empirical_reproduction),
    observed = c(
      nrow(missing_jobs) == 0L,
      if (nrow(recovery)) min(recovery$replications, na.rm = TRUE) else 0,
      if (nrow(failure)) max(failure$failure_rate, na.rm = TRUE) else 1,
      if (nrow(recovery)) max(recovery$absolute_bias, na.rm = TRUE) else Inf,
      if (nrow(recovery)) max(recovery$rmse, na.rm = TRUE) else Inf,
      if (nrow(recovery) && any(is.finite(recovery$coverage))) paste0(round(min(recovery$coverage, na.rm = TRUE), 3), "-", round(max(recovery$coverage, na.rm = TRUE), 3)) else NA_character_,
      max_rhat, min_ess, divergence_rate,
      if (nrow(sbc) && any(is.finite(sbc$p_value))) min(sbc$p_value, na.rm = TRUE) else NA_real_,
      .ve_evidence_pass(empirical_reproduction, "empirical_reproduction")
    ),
    pass = c(
      nrow(missing_jobs) == 0L,
      nrow(recovery) > 0L && all(recovery$replications >= thresholds$required_replications),
      nrow(failure) > 0L && all(failure$failure_rate <= thresholds$max_failure_rate),
      nrow(recovery) > 0L && all(is.finite(recovery$absolute_bias)) && all(recovery$absolute_bias <= thresholds$max_absolute_bias),
      nrow(recovery) > 0L && all(is.finite(recovery$rmse)) && all(recovery$rmse <= thresholds$max_rmse),
      nrow(recovery) > 0L && all(is.finite(recovery$coverage)) && all(recovery$coverage >= thresholds$min_coverage & recovery$coverage <= thresholds$max_coverage),
      is.na(max_rhat) || max_rhat <= thresholds$max_rhat,
      is.na(min_ess) || min_ess >= thresholds$min_ess_bulk,
      is.na(divergence_rate) || divergence_rate <= thresholds$max_divergence_rate,
      !thresholds$require_sbc || (nrow(sbc) > 0L && all(is.finite(sbc$p_value)) && all(sbc$p_value >= 0.01)),
      !thresholds$require_empirical_reproduction || .ve_evidence_pass(empirical_reproduction, "empirical_reproduction")
    ),
    stringsAsFactors = FALSE
  )
  out <- list(
    status = if (all(gates$pass)) "complete" else "incomplete",
    gates = gates,
    missing_jobs = missing_jobs,
    failure = failure,
    recovery = recovery,
    sbc = sbc,
    thresholds = thresholds,
    audited_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  class(out) <- "eye_validation_completion_audit"
  out
}

#' @export
print.eye_validation_completion_audit <- function(x, ...) {
  cat("Validation completion audit: ", toupper(x$status), "\n", sep = "")
  print(x$gates, row.names = FALSE)
  if (nrow(x$missing_jobs)) cat("Missing jobs: ", nrow(x$missing_jobs), "\n", sep = "")
  invisible(x)
}

.ve_plot_engine <- function(engine) {
  engine <- match.arg(engine, c("auto", "ggplot2", "base"))
  if (engine == "auto") if (requireNamespace("ggplot2", quietly = TRUE)) "ggplot2" else "base" else engine
}

#' Plot parameter recovery
#'
#' @param x Validation collection or estimates data frame.
#' @param parameter Optional parameter subset.
#' @param engine Plot engine.
#' @param ... Additional plotting arguments.
#' @return Plot object or plotted data invisibly.
#' @export
plot_parameter_recovery <- function(x, parameter = NULL, engine = c("auto", "ggplot2", "base"), ...) {
  d <- if (inherits(x, "eye_validation_collection")) x$estimates else x
  if (!is.data.frame(d) || !all(c("truth", "estimate", "parameter") %in% names(d))) .ve_stop("Recovery estimates are unavailable.")
  d <- d[is.finite(d$truth) & is.finite(d$estimate), , drop = FALSE]
  if (!is.null(parameter)) d <- d[d$parameter %in% parameter, , drop = FALSE]
  if (!nrow(d)) .ve_stop("No recovery rows match the request.")
  engine <- .ve_plot_engine(engine)
  if (engine == "ggplot2") {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = truth, y = estimate)) +
      ggplot2::geom_point(alpha = 0.35) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = 2) +
      ggplot2::facet_wrap(~parameter, scales = "free") +
      ggplot2::labs(x = "True value", y = "Estimate", title = "Parameter recovery") +
      ggplot2::theme_minimal()
    return(p)
  }
  graphics::plot(d$truth, d$estimate, xlab = "True value", ylab = "Estimate", main = "Parameter recovery", ...)
  graphics::abline(0, 1, lty = 2)
  invisible(d)
}

#' Plot interval coverage
#'
#' @param x Validation collection or recovery summary.
#' @param target Nominal coverage target.
#' @param engine Plot engine.
#' @param ... Additional plotting arguments.
#' @export
plot_interval_coverage <- function(x, target = 0.95, engine = c("auto", "ggplot2", "base"), ...) {
  d <- if (inherits(x, "eye_validation_collection")) validation_recovery_summary(x) else x
  if (!is.data.frame(d) || !all(c("parameter", "coverage") %in% names(d))) .ve_stop("Coverage summary is unavailable.")
  d <- d[is.finite(d$coverage), , drop = FALSE]
  if (!nrow(d)) .ve_stop("No finite coverage values are available.")
  engine <- .ve_plot_engine(engine)
  if (engine == "ggplot2") {
    p <- ggplot2::ggplot(d, ggplot2::aes(x = parameter, y = coverage)) +
      ggplot2::geom_point() + ggplot2::geom_hline(yintercept = target, linetype = 2) +
      ggplot2::coord_cartesian(ylim = c(0, 1)) + ggplot2::labs(x = NULL, y = "Coverage", title = "Interval coverage") +
      ggplot2::theme_minimal() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
    return(p)
  }
  values <- d$coverage; names(values) <- d$parameter
  graphics::barplot(values, ylim = c(0, 1), las = 2, ylab = "Coverage", main = "Interval coverage", ...)
  graphics::abline(h = target, lty = 2)
  invisible(d)
}

#' Plot SBC rank histograms
#'
#' @param x Validation collection or draw data frame.
#' @param parameter Optional parameter.
#' @param bins Number of bins.
#' @param engine Plot engine.
#' @param ... Additional plotting arguments.
#' @export
plot_sbc_rank <- function(x, parameter = NULL, bins = 10L, engine = c("auto", "ggplot2", "base"), ...) {
  d <- if (inherits(x, "eye_validation_collection")) x$draws else x
  if (!is.data.frame(d) || !all(c("job_id", "parameter", "draw", "truth") %in% names(d))) .ve_stop("SBC draws are unavailable.")
  if (!is.null(parameter)) d <- d[d$parameter %in% parameter, , drop = FALSE]
  ranks <- .ve_bind_rows(lapply(split(seq_len(nrow(d)), interaction(d$job_id, d$parameter, drop = TRUE)), function(index) {
    z <- d[index, , drop = FALSE]
    if (!nrow(z)) return(NULL)
    data.frame(parameter = z$parameter[1L], scaled_rank = (sum(z$draw < z$truth[1L]) + 0.5 * sum(z$draw == z$truth[1L]) + 0.5) / (nrow(z) + 1), stringsAsFactors = FALSE)
  }))
  if (!nrow(ranks)) .ve_stop("No SBC ranks could be calculated.")
  engine <- .ve_plot_engine(engine)
  if (engine == "ggplot2") {
    return(ggplot2::ggplot(ranks, ggplot2::aes(x = scaled_rank)) + ggplot2::geom_histogram(bins = bins, boundary = 0) + ggplot2::facet_wrap(~parameter) + ggplot2::labs(x = "Scaled rank", y = "Replications", title = "Simulation-based calibration") + ggplot2::theme_minimal())
  }
  graphics::hist(ranks$scaled_rank, breaks = seq(0, 1, length.out = bins + 1L), xlab = "Scaled rank", main = "Simulation-based calibration", ...)
  invisible(ranks)
}

#' Plot validation failure rates
#'
#' @param x Validation collection or failure summary.
#' @param engine Plot engine.
#' @param ... Additional plotting arguments.
#' @export
plot_validation_failures <- function(x, engine = c("auto", "ggplot2", "base"), ...) {
  d <- if (inherits(x, "eye_validation_collection")) validation_failure_summary(x) else x
  if (!is.data.frame(d) || !"failure_rate" %in% names(d)) .ve_stop("Failure summary is unavailable.")
  d$label <- if ("scenario_id" %in% names(d)) d$scenario_id else seq_len(nrow(d))
  engine <- .ve_plot_engine(engine)
  if (engine == "ggplot2") {
    return(ggplot2::ggplot(d, ggplot2::aes(x = label, y = failure_rate)) + ggplot2::geom_col() + ggplot2::coord_cartesian(ylim = c(0, 1)) + ggplot2::labs(x = "Scenario", y = "Failure rate", title = "Validation failures") + ggplot2::theme_minimal() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)))
  }
  values <- d$failure_rate; names(values) <- d$label
  graphics::barplot(values, ylim = c(0, 1), las = 2, ylab = "Failure rate", main = "Validation failures", ...)
  invisible(d)
}

#' Plot validation runtime
#'
#' @param x Validation collection or job table.
#' @param engine Plot engine.
#' @param ... Additional plotting arguments.
#' @export
plot_validation_runtime <- function(x, engine = c("auto", "ggplot2", "base"), ...) {
  d <- if (inherits(x, "eye_validation_collection")) x$jobs else x
  if (!is.data.frame(d) || !"elapsed_seconds" %in% names(d)) .ve_stop("Runtime data are unavailable.")
  d <- d[is.finite(d$elapsed_seconds), , drop = FALSE]
  engine <- .ve_plot_engine(engine)
  if (engine == "ggplot2") {
    return(ggplot2::ggplot(d, ggplot2::aes(x = elapsed_seconds)) + ggplot2::geom_histogram(bins = max(10L, floor(sqrt(nrow(d))))) + ggplot2::labs(x = "Seconds per job", y = "Jobs", title = "Validation runtime") + ggplot2::theme_minimal())
  }
  graphics::hist(d$elapsed_seconds, xlab = "Seconds per job", main = "Validation runtime", ...)
  invisible(d)
}

#' Specify evidence gates for model promotion
#'
#' @param model_families Model families to audit.
#' @param require_completion Require complete Monte Carlo evidence.
#' @param require_sbc Require simulation-based calibration.
#' @param require_misspecification Require declared misspecification studies.
#' @param require_grouped_validation Require grouped out-of-sample validation.
#' @param require_engine_equivalence Require external-engine comparison.
#' @param require_empirical_reproduction Require an empirical reproduction.
#' @param require_preprocessing_sensitivity Require preprocessing/AOI sensitivity.
#' @param require_multi_vendor Require independent multi-vendor evidence.
#' @return An `eye_model_promotion_spec`.
#' @export
model_promotion_spec <- function(
    model_families = c("dynamic_irtree", "functional_pupil_irt", "theory_strategy_irt", "gaze_diffusion_irt"),
    require_completion = TRUE,
    require_sbc = TRUE,
    require_misspecification = TRUE,
    require_grouped_validation = TRUE,
    require_engine_equivalence = TRUE,
    require_empirical_reproduction = TRUE,
    require_preprocessing_sensitivity = TRUE,
    require_multi_vendor = FALSE) {
  model_families <- unique(as.character(model_families))
  if (!length(model_families) || anyNA(model_families) || any(!nzchar(model_families))) .ve_stop("`model_families` must contain non-empty names.")
  structure(list(
    model_families = model_families,
    require_completion = isTRUE(require_completion),
    require_sbc = isTRUE(require_sbc),
    require_misspecification = isTRUE(require_misspecification),
    require_grouped_validation = isTRUE(require_grouped_validation),
    require_engine_equivalence = isTRUE(require_engine_equivalence),
    require_empirical_reproduction = isTRUE(require_empirical_reproduction),
    require_preprocessing_sensitivity = isTRUE(require_preprocessing_sensitivity),
    require_multi_vendor = isTRUE(require_multi_vendor)
  ), class = "eye_model_promotion_spec")
}

.ve_evidence_pass <- function(x, type) {
  if (is.null(x)) return(FALSE)
  if (is.logical(x) && length(x) == 1L) return(isTRUE(x))
  if (type == "completion" && inherits(x, "eye_validation_completion_audit")) return(identical(x$status, "complete"))
  if (type == "grouped_validation" && inherits(x, c("eye_grouped_cv", "eye_crossed_grouped_cv"))) return(nrow(x$results) > 0L && all(is.finite(x$results$score)))
  if (type == "engine_equivalence" && inherits(x, "eye_engine_comparison")) return(any(!is.na(x$estimates$equivalent)) && all(x$estimates$equivalent[!is.na(x$estimates$equivalent)]))
  if (type == "empirical_reproduction" && inherits(x, "eye_empirical_reproduction")) return(nrow(x$comparison) > 0L && all(x$comparison$reproduced, na.rm = TRUE))
  if (type == "preprocessing_sensitivity" && inherits(x, "eye_multiverse")) return(length(x$specifications) >= 2L && nrow(x$results) > 0L)
  if (type == "multi_vendor" && inherits(x, "eye_vendor_validation")) return(nrow(x) > 0L && all(x$status == "pass"))
  if (is.data.frame(x) && "pass" %in% names(x)) {
    value <- suppressWarnings(as.logical(x$pass))
    return(nrow(x) > 0L && any(!is.na(value)) && all(value[!is.na(value)]))
  }
  if (is.list(x) && !is.null(x$status)) return(tolower(as.character(x$status)[1L]) %in% c("pass", "passed", "complete", "success"))
  FALSE
}

#' Audit promotion readiness for advanced model families
#'
#' @param evidence Named list by model family. Each family may contain
#'   `completion`, `sbc`, `misspecification`, `grouped_validation`,
#'   `engine_equivalence`, `empirical_reproduction`,
#'   `preprocessing_sensitivity`, and `multi_vendor` evidence.
#' @param spec Promotion specification.
#' @return An `eye_model_promotion_audit`.
#' @export
audit_model_promotion <- function(evidence, spec = model_promotion_spec()) {
  if (!is.list(evidence)) .ve_stop("`evidence` must be a named list.")
  if (!inherits(spec, "eye_model_promotion_spec")) .ve_stop("`spec` must be created by `model_promotion_spec()`.")
  gates <- c(
    completion = spec$require_completion,
    sbc = spec$require_sbc,
    misspecification = spec$require_misspecification,
    grouped_validation = spec$require_grouped_validation,
    engine_equivalence = spec$require_engine_equivalence,
    empirical_reproduction = spec$require_empirical_reproduction,
    preprocessing_sensitivity = spec$require_preprocessing_sensitivity,
    multi_vendor = spec$require_multi_vendor
  )
  rows <- list(); k <- 0L
  for (family in spec$model_families) {
    family_evidence <- evidence[[family]]
    if (is.null(family_evidence)) family_evidence <- list()
    for (gate in names(gates)) {
      k <- k + 1L
      required <- isTRUE(gates[[gate]])
      passed <- if (required) .ve_evidence_pass(family_evidence[[gate]], gate) else TRUE
      rows[[k]] <- data.frame(
        model_family = family,
        gate = gate,
        required = required,
        evidence_supplied = !is.null(family_evidence[[gate]]),
        pass = passed,
        stringsAsFactors = FALSE
      )
    }
  }
  table <- do.call(rbind, rows)
  family_summary <- .ve_group_summary(table, "model_family", function(z) data.frame(
    required_gates = sum(z$required),
    passed_required_gates = sum(z$required & z$pass),
    status = if (all(z$pass[z$required])) "promotable" else "experimental",
    stringsAsFactors = FALSE
  ))
  out <- list(gates = table, models = family_summary, spec = spec, audited_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  class(out) <- "eye_model_promotion_audit"
  out
}

#' @export
print.eye_model_promotion_audit <- function(x, ...) {
  cat("Advanced-model promotion audit\n")
  print(x$models, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_model_promotion_audit <- function(x, ...) {
  z <- aggregate(pass ~ model_family, data = x$gates[x$gates$required, , drop = FALSE], FUN = mean)
  values <- z$pass; names(values) <- z$model_family
  graphics::barplot(values, ylim = c(0, 1), las = 2, ylab = "Proportion of required gates passed", main = "Model promotion readiness", ...)
  invisible(z)
}

.ve_markdown_table <- function(x, digits = 4L) {
  if (!is.data.frame(x) || !nrow(x)) return("_No rows available._")
  x[] <- lapply(x, function(column) {
    if (is.numeric(column)) format(round(column, digits), trim = TRUE, scientific = FALSE) else as.character(column)
  })
  header <- paste0("| ", paste(names(x), collapse = " | "), " |")
  separator <- paste0("|", paste(rep("---", ncol(x)), collapse = "|"), "|")
  rows <- apply(x, 1L, function(row) paste0("| ", paste(gsub("\\|", "\\\\|", row), collapse = " | "), " |"))
  c(header, separator, rows)
}

#' Write a validation release report
#'
#' @param x Validation collection.
#' @param path Markdown output file.
#' @param completion Optional completion audit.
#' @param promotion Optional model-promotion audit.
#' @param title Report title.
#' @param include_session Include session information.
#' @return Normalized report path.
#' @export
write_validation_release_report <- function(
    x,
    path,
    completion = NULL,
    promotion = NULL,
    title = "eyeprocess validation release report",
    include_session = TRUE) {
  if (!inherits(x, "eye_validation_collection")) .ve_stop("Expected an `eye_validation_collection`.")
  if (is.null(completion)) completion <- audit_validation_completion(x)
  recovery <- if (nrow(x$estimates)) validation_recovery_summary(x) else data.frame()
  failure <- if (nrow(x$jobs)) validation_failure_summary(x) else data.frame()
  runtime <- if (nrow(x$jobs)) validation_runtime_summary(x) else data.frame()
  calibration <- if (nrow(x$predictions) && all(c("observed", "predicted") %in% names(x$predictions))) validation_calibration_summary(x) else data.frame()
  sbc <- if (nrow(x$draws)) tryCatch(validation_sbc_summary(x), error = function(e) data.frame()) else data.frame()
  lines <- c(
    paste0("# ", title), "",
    paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)), "",
    paste0("Completion status: **", toupper(completion$status), "**"), "",
    "## Completion gates", "", .ve_markdown_table(completion$gates), "",
    "## Job failures and convergence", "", .ve_markdown_table(failure), "",
    "## Parameter recovery", "", .ve_markdown_table(recovery), "",
    "## Runtime", "", .ve_markdown_table(runtime), ""
  )
  if (nrow(calibration)) lines <- c(lines, "## Prediction calibration", "", .ve_markdown_table(calibration), "")
  if (nrow(sbc)) lines <- c(lines, "## Simulation-based calibration", "", .ve_markdown_table(sbc), "")
  if (!is.null(promotion) && inherits(promotion, "eye_model_promotion_audit")) {
    lines <- c(lines, "## Model promotion audit", "", .ve_markdown_table(promotion$models), "", .ve_markdown_table(promotion$gates), "")
  }
  lines <- c(lines,
    "## Interpretation boundary", "",
    "Executable software, successful unit tests, and completed simulations are not by themselves evidence that a model identifies a named cognitive process. Promotion requires the prespecified recovery, calibration, misspecification, grouped-validation, engine-equivalence, sensitivity, and empirical-reproduction gates.", ""
  )
  if (isTRUE(include_session)) lines <- c(lines, "## Session", "", paste0("- R: ", R.version.string), paste0("- Platform: ", R.version$platform), "")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Write a model-promotion report
#'
#' @param x Model-promotion audit.
#' @param path Markdown output file.
#' @return Normalized report path.
#' @export
write_model_promotion_report <- function(x, path) {
  if (!inherits(x, "eye_model_promotion_audit")) .ve_stop("Expected an `eye_model_promotion_audit`.")
  lines <- c(
    "# Advanced-model promotion audit", "",
    paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)), "",
    "## Model status", "", .ve_markdown_table(x$models), "",
    "## Evidence gates", "", .ve_markdown_table(x$gates), "",
    "Models remain experimental whenever any required evidence gate is absent or fails."
  )
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Remove obsolete or corrupt validation checkpoints
#'
#' @param path Validation output directory.
#' @param statuses Statuses to remove.
#' @param dry_run Report without deleting.
#' @return Data frame of selected checkpoints.
#' @export
prune_validation_checkpoints <- function(path, statuses = c("corrupt", "locked"), dry_run = TRUE) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- list.files(file.path(path, "checkpoints"), pattern = "^job-.*\\.rds$", full.names = TRUE)
  rows <- lapply(files, function(file) {
    result <- tryCatch(readRDS(file), error = identity)
    status <- if (inherits(result, "error")) "corrupt" else as.character(result$status)[1L]
    data.frame(file = file, status = status, remove = status %in% statuses, stringsAsFactors = FALSE)
  })
  out <- .ve_bind_rows(rows)
  if (!isTRUE(dry_run) && nrow(out)) unlink(out$file[out$remove], force = TRUE)
  out
}

if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "coverage", "definition", "elapsed_seconds", "estimate", "failure_rate",
    "label", "modal_share", "parameter", "scaled_rank", "state_probability",
    "strategy", "truth"
  ))
}
