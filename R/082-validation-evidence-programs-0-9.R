# eyeprocess 0.9 Milestone #2: validation evidence orchestration

.ep09m2_assert_scalar <- function(x, name, type = c("numeric", "character", "logical"), finite = FALSE) {
  type <- match.arg(type)
  if (length(x) != 1L || is.na(x)) stop(name, " must be a non-missing scalar.", call. = FALSE)
  ok <- switch(type,
    numeric = is.numeric(x),
    character = is.character(x),
    logical = is.logical(x)
  )
  if (!ok) stop(name, " must be ", type, ".", call. = FALSE)
  if (isTRUE(finite) && !is.finite(x)) stop(name, " must be finite.", call. = FALSE)
  invisible(TRUE)
}

.ep09m2_as_df <- function(x, name = "object") {
  if (!is.data.frame(x)) stop(name, " must be a data.frame.", call. = FALSE)
  x
}

.ep09m2_req_cols <- function(x, cols, name = "data") {
  miss <- setdiff(cols, names(x))
  if (length(miss)) stop(name, " is missing required columns: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.ep09m2_finite_mean <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

.ep09m2_finite_sd <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 2L) return(NA_real_)
  stats::sd(x)
}

.ep09m2_safe_quantile <- function(x, probs) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  as.numeric(stats::quantile(x, probs = probs, names = FALSE, na.rm = TRUE, type = 8))
}

.ep09m2_hash <- function(x) {
  if (exists("object_hash", mode = "function")) return(object_hash(x))
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(x, tf, version = 3)
  unname(tools::md5sum(tf))
}

#' Declare an eyeprocess validation-evidence plan
#'
#' Creates a deterministic validation plan. The plan describes software-validation
#' scenarios and does not constitute evidence for the construct validity of any
#' gaze, pupil, response-time, or psychometric measure.
#' @param families Validation-evidence families to include.
#' @param sample_size Validation sample size or vector of sample sizes.
#' @param n_items Number of items.
#' @param missing_rate Proportion of responses or observations set missing.
#' @param noise_level Declared simulation noise regime.
#' @param specification Whether the validation scenario is correctly specified or deliberately misspecified.
#' @param replications Number of simulation or validation replications.
#' @param seed Random-number seed for reproducible execution.
#' @param label Human-readable label.
#' @return An object of class "eye_validation_evidence_plan", stored as a named list, with components "families", "sample_size", "n_items", "missing_rate", "noise_level", "specification", "replications", "seed", "label". It contains declare an eyeprocess validation-evidence plan and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_validation_plan <- function(
    families = c("recovery", "sbc", "stress", "reliability", "negative_control"),
    sample_size = c(250L, 750L),
    n_items = c(12L, 24L),
    missing_rate = c(0, 0.15),
    noise_level = c("reference", "elevated"),
    specification = c("correct", "misspecified"),
    replications = 20L,
    seed = 20260811L,
    label = "eyeprocess-0.9-m2") {
  families <- unique(as.character(families))
  allowed <- c("recovery", "sbc", "stress", "reliability", "negative_control")
  if (!length(families) || anyNA(families) || any(!families %in% allowed))
    stop("families must be drawn from: ", paste(allowed, collapse = ", "), call. = FALSE)
  sample_size <- unique(as.integer(sample_size))
  n_items <- unique(as.integer(n_items))
  if (!length(sample_size) || anyNA(sample_size) || any(sample_size < 20L)) stop("sample_size must contain integers >= 20.", call. = FALSE)
  if (!length(n_items) || anyNA(n_items) || any(n_items < 3L)) stop("n_items must contain integers >= 3.", call. = FALSE)
  missing_rate <- unique(as.numeric(missing_rate))
  if (!length(missing_rate) || any(!is.finite(missing_rate)) || any(missing_rate < 0 | missing_rate >= 1))
    stop("missing_rate must lie in [0, 1).", call. = FALSE)
  noise_level <- unique(as.character(noise_level))
  specification <- unique(as.character(specification))
  if (!length(noise_level) || anyNA(noise_level)) stop("noise_level must be non-empty.", call. = FALSE)
  if (!all(specification %in% c("correct", "misspecified"))) stop("specification must be 'correct' and/or 'misspecified'.", call. = FALSE)
  replications <- as.integer(replications)
  seed <- as.integer(seed)
  if (length(replications) != 1L || is.na(replications) || replications < 1L) stop("replications must be >= 1.", call. = FALSE)
  if (length(seed) != 1L || is.na(seed) || seed < 1L) stop("seed must be a positive integer.", call. = FALSE)
  .ep09m2_assert_scalar(label, "label", "character")
  structure(list(
    families = families,
    sample_size = sample_size,
    n_items = n_items,
    missing_rate = missing_rate,
    noise_level = noise_level,
    specification = specification,
    replications = replications,
    seed = seed,
    label = label
  ), class = "eye_validation_evidence_plan")
}

#' Validate a validation-evidence plan
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @return A logical value or vector indicating a validation-evidence plan.
#' @export
validate_eyeprocess_validation_plan <- function(x) {
  if (!inherits(x, "eye_validation_evidence_plan")) stop("x must inherit from eye_validation_evidence_plan.", call. = FALSE)
  required <- c("families", "sample_size", "n_items", "missing_rate", "noise_level", "specification", "replications", "seed", "label")
  miss <- setdiff(required, names(x))
  if (length(miss)) stop("validation plan is missing fields: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

#' Expand a validation-evidence plan to a scenario table
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @return An R object containing expand a validation-evidence plan to a scenario table. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
expand_eyeprocess_validation_plan <- function(x) {
  validate_eyeprocess_validation_plan(x)
  grid <- expand.grid(
    family = x$families,
    sample_size = x$sample_size,
    n_items = x$n_items,
    missing_rate = x$missing_rate,
    noise_level = x$noise_level,
    specification = x$specification,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )
  grid$scenario_id <- sprintf("M2S%04d", seq_len(nrow(grid)))
  grid$replications <- x$replications
  grid$master_seed <- x$seed
  grid$scenario_seed <- vapply(seq_len(nrow(grid)), function(i) eyeprocess_validation_seed(x$seed, i), integer(1))
  grid <- grid[, c("scenario_id", "family", "sample_size", "n_items", "missing_rate", "noise_level", "specification", "replications", "master_seed", "scenario_seed")]
  attr(grid, "label") <- x$label
  attr(grid, "plan_hash") <- .ep09m2_hash(x)
  grid
}

#' Derive a deterministic bounded validation seed
#' @param master_seed Master random-number seed.
#' @param index Deterministic substream index.
#' @param stream Named random-number stream.
#' @return A numeric value or vector containing a deterministic bounded validation seed.
#' @export
eyeprocess_validation_seed <- function(master_seed, index, stream = 0L) {
  master_seed <- as.integer(master_seed)
  index <- as.integer(index)
  stream <- as.integer(stream)
  if (length(master_seed) != 1L || length(index) != 1L || length(stream) != 1L ||
      anyNA(c(master_seed, index, stream)) || master_seed < 1L || index < 1L || stream < 0L)
    stop("master_seed and index must be positive scalar integers; stream must be a non-negative scalar integer.", call. = FALSE)
  modulus <- 2147483646
  value <- (as.double(master_seed) + 104729 * as.double(index) + 1009 * as.double(stream)) %% modulus
  as.integer(value + 1)
}

#' Define a validation acceptance rule
#' @param metric Metric name or metric column.
#' @param direction Direction vector used to project multidimensional information.
#' @param threshold Decision or diagnostic threshold.
#' @param upper Optional upper threshold for interval-style acceptance rules.
#' @param tolerance Numerical or decision tolerance.
#' @return An object of class "eye_validation_acceptance_rule", stored as a named list, with components "metric", "direction", "threshold", "upper", "tolerance". It contains define a validation acceptance rule and associated metadata or diagnostics needed to interpret the result.
#' @export
validation_acceptance_rule <- function(metric, direction = c("max", "min", "between", "equals"), threshold, upper = NULL, tolerance = 0) {
  .ep09m2_assert_scalar(metric, "metric", "character")
  direction <- match.arg(direction)
  if (!length(threshold) || anyNA(threshold)) stop("threshold must be supplied.", call. = FALSE)
  threshold <- suppressWarnings(as.numeric(threshold))
  if (direction == "between") {
    upper <- suppressWarnings(as.numeric(upper))
    if (length(threshold) != 1L || is.null(upper) || length(upper) != 1L || !is.finite(threshold) || !is.finite(upper) || threshold > upper)
      stop("between rules require finite lower threshold <= finite upper.", call. = FALSE)
  } else if (length(threshold) != 1L || !is.finite(threshold)) stop("threshold must be a finite scalar for this rule.", call. = FALSE)
  tolerance <- as.numeric(tolerance)
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) stop("tolerance must be a finite non-negative scalar.", call. = FALSE)
  structure(list(metric = metric, direction = direction, threshold = threshold, upper = upper, tolerance = tolerance),
            class = "eye_validation_acceptance_rule")
}

#' Evaluate a validation acceptance rule
#' @param value Observed metric value to evaluate.
#' @param rule Validation acceptance rule to apply.
#' @return A logical value or vector indicating a validation acceptance rule.
#' @export
evaluate_validation_acceptance <- function(value, rule) {
  if (!inherits(rule, "eye_validation_acceptance_rule")) stop("rule must be created by validation_acceptance_rule().", call. = FALSE)
  value <- as.numeric(value)
  if (length(value) != 1L || !is.finite(value)) return(NA)
  tol <- rule$tolerance
  switch(rule$direction,
    max = value <= as.numeric(rule$threshold) + tol,
    min = value >= as.numeric(rule$threshold) - tol,
    between = value >= as.numeric(rule$threshold) - tol && value <= as.numeric(rule$upper) + tol,
    equals = abs(value - as.numeric(rule$threshold)) <= tol,
    stop("Unknown rule direction.", call. = FALSE)
  )
}

#' Evaluate a table against named validation rules
#' @param summary Validation summary table.
#' @param rules Collection of validation acceptance rules.
#' @param id_cols Columns identifying validation scenarios.
#' @return A tabular R object containing a table against named validation rules; rows represent analysis units and columns contain the returned quantities.
#' @export
validation_acceptance_matrix <- function(summary, rules, id_cols = character()) {
  summary <- .ep09m2_as_df(summary, "summary")
  if (!is.list(rules) || !length(rules) || !all(vapply(rules, inherits, logical(1), what = "eye_validation_acceptance_rule")))
    stop("rules must be a non-empty list of validation_acceptance_rule objects.", call. = FALSE)
  id_cols <- as.character(id_cols)
  .ep09m2_req_cols(summary, unique(c(id_cols, vapply(rules, `[[`, character(1), "metric"))), "summary")
  out <- lapply(seq_len(nrow(summary)), function(i) {
    row <- summary[i, , drop = FALSE]
    checks <- lapply(seq_along(rules), function(j) {
      rule <- rules[[j]]
      value <- row[[rule$metric]][[1L]]
      data.frame(
        rule_id = if (!is.null(names(rules)) && nzchar(names(rules)[j])) names(rules)[j] else paste0("rule_", j),
        metric = rule$metric,
        value = suppressWarnings(as.numeric(value)),
        direction = rule$direction,
        threshold = paste(c(rule$threshold, rule$upper), collapse = ":"),
        pass = evaluate_validation_acceptance(value, rule),
        stringsAsFactors = FALSE
      )
    })
    z <- do.call(rbind, checks)
    if (length(id_cols)) z <- cbind(row[rep(1L, nrow(z)), id_cols, drop = FALSE], z, row.names = NULL)
    z
  })
  do.call(rbind, out)
}

#' Summarise an acceptance matrix
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param by Grouping variables or aggregation level.
#' @return A tabular R object containing an acceptance matrix; rows represent analysis units and columns contain the returned quantities.
#' @export
summarise_validation_acceptance <- function(x, by = character()) {
  x <- .ep09m2_as_df(x, "x")
  .ep09m2_req_cols(x, c(by, "pass"), "x")
  if (!length(by)) {
    return(data.frame(n = nrow(x), n_evaluable = sum(!is.na(x$pass)), n_pass = sum(x$pass %in% TRUE), pass_fraction = if (sum(!is.na(x$pass))) mean(x$pass, na.rm = TRUE) else NA_real_))
  }
  key <- interaction(x[by], drop = TRUE, lex.order = TRUE)
  rows <- split(seq_len(nrow(x)), key)
  do.call(rbind, lapply(rows, function(ii) {
    z <- x[ii, , drop = FALSE]
    head <- z[1L, by, drop = FALSE]
    data.frame(head, n = nrow(z), n_evaluable = sum(!is.na(z$pass)), n_pass = sum(z$pass %in% TRUE),
               pass_fraction = if (sum(!is.na(z$pass))) mean(z$pass, na.rm = TRUE) else NA_real_, row.names = NULL)
  }))
}

#' Estimate Monte Carlo uncertainty for validation summaries
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param metric Metric name or metric column.
#' @param by Grouping variables or aggregation level.
#' @return A tabular R object containing monte Carlo uncertainty for validation summaries; rows represent analysis units and columns contain the returned quantities.
#' @export
validation_mcse_profile <- function(x, metric, by = character()) {
  x <- .ep09m2_as_df(x, "x")
  .ep09m2_req_cols(x, c(by, metric), "x")
  calc <- function(z) {
    v <- as.numeric(z[[metric]])
    v <- v[is.finite(v)]
    n <- length(v)
    data.frame(n = n, mean = if (n) mean(v) else NA_real_, sd = if (n > 1L) stats::sd(v) else NA_real_,
               mcse_mean = if (n > 1L) stats::sd(v) / sqrt(n) else NA_real_, stringsAsFactors = FALSE)
  }
  if (!length(by)) return(calc(x))
  key <- interaction(x[by], drop = TRUE, lex.order = TRUE)
  rows <- split(seq_len(nrow(x)), key)
  do.call(rbind, lapply(rows, function(ii) cbind(x[ii[1L], by, drop = FALSE], calc(x[ii, , drop = FALSE]), row.names = NULL)))
}

#' Compute a replication budget from a target MCSE
#' @param pilot_sd Pilot estimate of the metric standard deviation.
#' @param target_mcse Target Monte Carlo standard error.
#' @param minimum Minimum permitted replication count.
#' @param maximum Maximum permitted replication count.
#' @return A numeric value or vector containing a replication budget from a target MCSE.
#' @export
validation_replication_budget <- function(pilot_sd, target_mcse, minimum = 20L, maximum = 10000L) {
  pilot_sd <- as.numeric(pilot_sd); target_mcse <- as.numeric(target_mcse)
  minimum <- as.integer(minimum); maximum <- as.integer(maximum)
  if (length(pilot_sd) != 1L || !is.finite(pilot_sd) || pilot_sd < 0) stop("pilot_sd must be finite and non-negative.", call. = FALSE)
  if (length(target_mcse) != 1L || !is.finite(target_mcse) || target_mcse <= 0) stop("target_mcse must be positive.", call. = FALSE)
  if (minimum < 1L || maximum < minimum) stop("invalid minimum/maximum replication limits.", call. = FALSE)
  raw <- if (pilot_sd == 0) minimum else ceiling((pilot_sd / target_mcse)^2)
  as.integer(min(maximum, max(minimum, raw)))
}

#' Create a scenario manifest for frozen validation work
#' @param plan Validation or stress-evidence plan object.
#' @param source_commit Source-control commit associated with the evidence.
#' @param generated_at Generation timestamp stored in the manifest.
#' @return An object of class "eye_validation_scenario_manifest", stored as a named list, with components "label", "plan_hash", "scenarios", "source_commit", "generated_at", "scientific_scope". It contains a scenario manifest for frozen validation work and associated metadata or diagnostics needed to interpret the result.
#' @export
validation_scenario_manifest <- function(plan, source_commit = NA_character_, generated_at = Sys.time()) {
  validate_eyeprocess_validation_plan(plan)
  grid <- expand_eyeprocess_validation_plan(plan)
  structure(list(
    label = plan$label,
    plan_hash = .ep09m2_hash(plan),
    scenarios = grid,
    source_commit = as.character(source_commit),
    generated_at = format(as.POSIXct(generated_at), tz = "UTC", usetz = TRUE),
    scientific_scope = "software validation; not construct-validity evidence"
  ), class = "eye_validation_scenario_manifest")
}

#' Write a validation scenario manifest
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param path File path for reading or writing.
#' @return A character string or vector giving the path or identifier for a validation scenario manifest.
#' @export
write_validation_scenario_manifest <- function(x, path) {
  if (!inherits(x, "eye_validation_scenario_manifest")) stop("x must be a validation scenario manifest.", call. = FALSE)
  saveRDS(x, path, version = 3)
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Read a validation scenario manifest
#' @param path File path for reading or writing.
#' @return An R object containing a validation scenario manifest. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
read_validation_scenario_manifest <- function(path) {
  x <- readRDS(path)
  if (!inherits(x, "eye_validation_scenario_manifest")) stop("File is not an eyeprocess validation scenario manifest.", call. = FALSE)
  x
}

#' Grade the completeness of validation evidence
#' @param components Named evidence components.
#' @param required Required evidence components or requirements.
#' @return An object of class "eye_validation_evidence_grade", stored as a named list, with components "grade", "required", "present", "coverage". It contains grade the completeness of validation evidence and associated metadata or diagnostics needed to interpret the result.
#' @export
eyeprocess_validation_evidence_grade <- function(components, required = c("design", "execution", "summary", "provenance", "hash")) {
  components <- as.character(components)
  required <- unique(as.character(required))
  if (!length(required) || anyNA(required)) stop("required must be non-empty.", call. = FALSE)
  present <- required %in% components
  grade <- if (all(present)) "complete" else if (sum(present) >= ceiling(length(required) * 0.6)) "partial" else "insufficient"
  structure(list(grade = grade, required = required, present = setNames(present, required), coverage = mean(present)),
            class = "eye_validation_evidence_grade")
}

#' @export
print.eye_validation_evidence_plan <- function(x, ...) {
  cat("eyeprocess validation-evidence plan\n")
  cat("  label       :", x$label, "\n")
  cat("  families    :", paste(x$families, collapse = ", "), "\n")
  cat("  scenarios   :", nrow(expand_eyeprocess_validation_plan(x)), "\n")
  cat("  replications:", x$replications, "\n")
  invisible(x)
}

#' @export
print.eye_validation_evidence_grade <- function(x, ...) {
  cat("eyeprocess validation-evidence grade\n")
  cat("  grade   :", x$grade, "\n")
  cat("  coverage:", sprintf("%.1f%%", 100 * x$coverage), "\n")
  invisible(x)
}
