# eyeprocess 0.9 Milestone #2: evidence execution plans and immutable freeze objects

#' Declare the Milestone #2 measurement-quality stress evidence plan
#' @param missing_gaze Severity level for synthetic gaze missingness.
#' @param pupil_dropout Severity level for synthetic pupil dropout.
#' @param calibration_offset Magnitude of synthetic calibration offset.
#' @param sampling_jitter Magnitude of synthetic sampling-time jitter.
#' @param aoi_label_noise Rate of synthetic AOI-label corruption.
#' @param device_shift Magnitude of synthetic device shift.
#' @param trial_imbalance Severity of synthetic trial imbalance.
#' @param seed Random-number seed for reproducible execution.
#' @export
eyeprocess_stress_evidence_plan <- function(
    missing_gaze = c(0, .05, .15, .30),
    pupil_dropout = c(0, .05, .15),
    calibration_offset = c(0, .01, .03, .06),
    sampling_jitter = c(0, .05, .15),
    aoi_label_noise = c(0, .02, .10),
    device_shift = c(0, .02, .05),
    trial_imbalance = c(0, .10, .25),
    seed = 20260811L) {
  vals <- list(missing_gaze = missing_gaze, pupil_dropout = pupil_dropout, calibration_offset = calibration_offset,
               sampling_jitter = sampling_jitter, aoi_label_noise = aoi_label_noise, device_shift = device_shift, trial_imbalance = trial_imbalance)
  proportion_fields <- c("missing_gaze", "pupil_dropout", "aoi_label_noise", "trial_imbalance")
  for (nm in names(vals)) {
    z <- as.numeric(vals[[nm]])
    if (!length(z) || any(!is.finite(z)) || any(z < 0)) stop(nm, " must contain finite non-negative values.", call. = FALSE)
    if (nm %in% proportion_fields && any(z >= 1)) stop(nm, " must lie in [0,1).", call. = FALSE)
    vals[[nm]] <- unique(z)
  }
  seed <- as.integer(seed); if (length(seed) != 1L || seed < 1L || is.na(seed)) stop("seed must be a positive scalar integer.", call. = FALSE)
  structure(c(vals, list(seed = seed)), class = "eye_stress_evidence_plan")
}

#' Expand a stress evidence plan into one-factor-at-a-time scenarios
#' @param plan Validation or stress-evidence plan object.
#' @export
expand_eyeprocess_stress_evidence_plan <- function(plan) {
  if (!inherits(plan, "eye_stress_evidence_plan")) stop("plan must be an eye_stress_evidence_plan.", call. = FALSE)
  families <- setdiff(names(plan), "seed"); out <- list(); k <- 0L
  for (f in families) for (v in plan[[f]]) {
    k <- k + 1L; out[[k]] <- data.frame(scenario_id = sprintf("STRESS%03d", k), corruption = f, severity = v, seed = eyeprocess_validation_seed(plan$seed, k), stringsAsFactors = FALSE)
  }
  do.call(rbind, out)
}

#' Declare reliability evidence targets
#' @param metrics Reliability metrics requested by the evidence plan.
#' @param bootstrap Number of bootstrap replicates or bootstrap configuration.
#' @param seed Random-number seed for reproducible execution.
#' @export
eyeprocess_reliability_evidence_plan <- function(metrics = c("split_half", "icc", "temporal_stability", "bland_altman"), bootstrap = 200L, seed = 20260811L) {
  metrics <- unique(as.character(metrics)); allowed <- c("split_half", "icc", "temporal_stability", "bland_altman")
  if (!length(metrics) || any(!metrics %in% allowed)) stop("unsupported reliability metric.", call. = FALSE)
  bootstrap <- as.integer(bootstrap); seed <- as.integer(seed)
  if (length(bootstrap) != 1L || is.na(bootstrap) || bootstrap < 0L || length(seed) != 1L || is.na(seed) || seed < 1L) stop("invalid bootstrap/seed.", call. = FALSE)
  structure(list(metrics = metrics, bootstrap = bootstrap, seed = seed,
                 guardrail = "Reliability is repeatability evidence, not construct validity."), class = "eye_reliability_evidence_plan")
}

#' Declare negative-control evidence targets
#' @param controls Negative-control methods requested by the evidence plan.
#' @param replications Number of simulation or validation replications.
#' @param seed Random-number seed for reproducible execution.
#' @export
eyeprocess_negative_control_evidence_plan <- function(controls = c("permutation", "temporal_shift", "placebo_window", "known_leakage"), replications = 100L, seed = 20260811L) {
  controls <- unique(as.character(controls)); allowed <- c("permutation", "temporal_shift", "placebo_window", "known_leakage")
  if (!length(controls) || any(!controls %in% allowed)) stop("unsupported negative control.", call. = FALSE)
  replications <- as.integer(replications); seed <- as.integer(seed)
  if (length(replications) != 1L || is.na(replications) || replications < 1L || length(seed) != 1L || is.na(seed) || seed < 1L) stop("replications/seed must be positive scalar integers.", call. = FALSE)
  structure(list(controls = controls, replications = replications, seed = seed,
                 guardrail = "Negative controls diagnose analysis behavior; they do not label analyst conduct or participant state."), class = "eye_negative_control_evidence_plan")
}

#' Build a machine-readable validation claim/evidence matrix
#' @param claim_id Unique identifier for the claim.
#' @param claim Text of the software-validation claim.
#' @param evidence_id Identifier of evidence supporting or testing the claim.
#' @param evidence_type Type or class of evidence.
#' @param status Evidence, model, or governance status.
#' @param boundary Explicit interpretation or scope boundary for the claim.
#' @export
eyeprocess_validation_claim_matrix <- function(claim_id, claim, evidence_id, evidence_type, status = "qualified", boundary = NA_character_) {
  fields <- list(claim_id = claim_id, claim = claim, evidence_id = evidence_id, evidence_type = evidence_type, status = status, boundary = boundary)
  lengths <- vapply(fields, length, integer(1))
  if (any(lengths == 0L)) stop("claim/evidence fields must be non-empty.", call. = FALSE)
  n <- max(lengths)
  if (any(!lengths %in% c(1L, n))) stop("claim/evidence fields must have length 1 or a common maximum length.", call. = FALSE)
  recycle <- function(x) rep(x, length.out = n)
  status <- recycle(as.character(status)); allowed <- c("qualified", "supported", "pending", "not_supported")
  if (anyNA(status) || any(!status %in% allowed)) stop("invalid claim status.", call. = FALSE)
  out <- data.frame(claim_id = recycle(as.character(claim_id)), claim = recycle(as.character(claim)), evidence_id = recycle(as.character(evidence_id)),
                    evidence_type = recycle(as.character(evidence_type)), status = status, boundary = recycle(as.character(boundary)), stringsAsFactors = FALSE)
  if (anyNA(out$claim_id) || any(!nzchar(out$claim_id)) || anyDuplicated(out$claim_id)) stop("claim_id must be unique, non-missing, and non-empty.", call. = FALSE)
  if (anyNA(out$claim) || any(!nzchar(out$claim)) || anyNA(out$evidence_id) || any(!nzchar(out$evidence_id)) || anyNA(out$evidence_type) || any(!nzchar(out$evidence_type)))
    stop("claim, evidence_id, and evidence_type must be non-missing and non-empty.", call. = FALSE)
  out
}

#' Create an evidence manifest from files and in-memory objects
#' @param files File paths to include in the evidence manifest.
#' @param objects Objects to include in the evidence manifest.
#' @param source_commit Source-control commit associated with the evidence.
#' @param label Human-readable label.
#' @export
eyeprocess_validation_evidence_manifest <- function(files = character(), objects = list(), source_commit = NA_character_, label = "eyeprocess-0.9-m2") {
  files <- as.character(files); missing <- files[!file.exists(files)]
  if (length(missing)) stop("Evidence files not found: ", paste(missing, collapse = ", "), call. = FALSE)
  file_tab <- if (length(files)) data.frame(path = normalizePath(files, winslash = "/", mustWork = TRUE), md5 = unname(tools::md5sum(files)), stringsAsFactors = FALSE) else data.frame(path = character(), md5 = character())
  object_tab <- if (length(objects)) data.frame(name = if (is.null(names(objects))) paste0("object_", seq_along(objects)) else names(objects), hash = vapply(objects, .ep09m2_hash, character(1)), stringsAsFactors = FALSE) else data.frame(name = character(), hash = character())
  structure(list(label = label, source_commit = as.character(source_commit), files = file_tab, objects = object_tab, generated_at = format(Sys.time(), tz = "UTC", usetz = TRUE)), class = "eye_validation_evidence_manifest")
}

#' Freeze a complete Milestone #2 evidence bundle
#' @param design Validation or simulation design object.
#' @param recovery Parameter-recovery evidence object or table.
#' @param sbc Simulation-based-calibration evidence object or table.
#' @param stress Measurement-stress evidence object or table.
#' @param reliability Reliability or repeatability evidence object or table.
#' @param negative_controls Negative-control evidence object or table.
#' @param irt IRT-specific evidence object or table.
#' @param claims Claim-evidence mapping table.
#' @param provenance Provenance metadata or provenance object.
#' @param source_commit Source-control commit associated with the evidence.
#' @export
freeze_eyeprocess_validation_evidence <- function(design, recovery = NULL, sbc = NULL, stress = NULL, reliability = NULL, negative_controls = NULL, irt = NULL, claims = NULL, provenance = NULL, source_commit = NA_character_) {
  components <- list(design = design, recovery = recovery, sbc = sbc, stress = stress, reliability = reliability,
                     negative_controls = negative_controls, irt = irt, claims = claims, provenance = provenance)
  presence <- !vapply(components, is.null, logical(1))
  obj <- structure(
    list(
      components = components,
      presence = presence,
      source_commit = as.character(source_commit),
      frozen_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      scientific_scope = "software validation and measurement-behavior evidence; not construct-validity certification"
    ),
    class = "eye_validation_evidence_freeze"
  )
  # Hash the same canonical classed object state that the verifier reconstructs
  # after removing the stored hash field.
  obj$hash <- .ep09m2_hash(obj)
  obj
}

#' Verify the integrity hash of a frozen evidence bundle
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @export
verify_eyeprocess_validation_evidence <- function(x) {
  if (!inherits(x, "eye_validation_evidence_freeze")) stop("x must be a frozen validation evidence bundle.", call. = FALSE)
  stored <- x$hash; y <- x; y$hash <- NULL
  identical(stored, .ep09m2_hash(y))
}

#' Write a frozen evidence bundle
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param path File path for reading or writing.
#' @export
write_eyeprocess_validation_evidence <- function(x, path) {
  if (!inherits(x, "eye_validation_evidence_freeze")) stop("x must be a frozen validation evidence bundle.", call. = FALSE)
  if (!verify_eyeprocess_validation_evidence(x)) stop("evidence hash verification failed before writing.", call. = FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(x, path, version = 3); invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}

#' Read and verify a frozen evidence bundle
#' @param path File path for reading or writing.
#' @param verify Whether integrity verification is performed when reading.
#' @export
read_eyeprocess_validation_evidence <- function(path, verify = TRUE) {
  x <- readRDS(path)
  if (!inherits(x, "eye_validation_evidence_freeze")) stop("file is not an eyeprocess validation evidence freeze.", call. = FALSE)
  if (isTRUE(verify) && !verify_eyeprocess_validation_evidence(x)) stop("frozen evidence hash verification failed.", call. = FALSE)
  x
}

#' Evaluate readiness of a Milestone #2 validation evidence bundle
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @param required Required evidence components or requirements.
#' @export
eyeprocess_validation_readiness <- function(x, required = c("design", "recovery", "stress", "reliability", "negative_controls", "claims", "provenance")) {
  if (!inherits(x, "eye_validation_evidence_freeze")) stop("x must be a frozen validation evidence bundle.", call. = FALSE)
  required <- unique(as.character(required)); present <- x$presence[required]; present[is.na(present)] <- FALSE
  structure(list(ready = all(present), table = data.frame(requirement = required, satisfied = unname(present), stringsAsFactors = FALSE),
                 hash_valid = verify_eyeprocess_validation_evidence(x), source_commit = x$source_commit), class = "eye_validation_readiness")
}

#' Apply a conservative software-release evidence gate
#' @param readiness Validation-readiness result.
#' @param acceptance Acceptance-rule results.
#' @param require_hash Whether a verified integrity hash is required.
#' @export
eyeprocess_validation_release_gate <- function(readiness, acceptance = NULL, require_hash = TRUE) {
  if (!inherits(readiness, "eye_validation_readiness")) stop("readiness must come from eyeprocess_validation_readiness().", call. = FALSE)
  acceptance_ok <- TRUE
  if (!is.null(acceptance)) {
    acceptance <- .ep09m2_as_df(acceptance, "acceptance"); .ep09m2_req_cols(acceptance, "pass", "acceptance")
    acceptance_ok <- nrow(acceptance) > 0L && all(acceptance$pass %in% TRUE)
  }
  pass <- isTRUE(readiness$ready) && acceptance_ok && (!isTRUE(require_hash) || isTRUE(readiness$hash_valid))
  structure(list(pass = pass, readiness = readiness$ready, acceptance = acceptance_ok, hash = readiness$hash_valid,
                 interpretation = "A passing gate supports software-release readiness only; it is not a scientific validity certificate."), class = "eye_validation_release_gate")
}

#' @export
print.eye_validation_evidence_freeze <- function(x, ...) {
  cat("eyeprocess frozen validation evidence\n")
  cat("  hash  :", x$hash, "\n")
  cat("  source:", x$source_commit, "\n")
  cat("  components:", paste(names(x$presence)[x$presence], collapse = ", "), "\n")
  invisible(x)
}

#' @export
print.eye_validation_readiness <- function(x, ...) {
  cat("eyeprocess validation readiness\n")
  cat("  ready     :", x$ready, "\n")
  cat("  hash valid:", x$hash_valid, "\n")
  invisible(x)
}

#' Execute a declared measurement-stress evidence plan
#'
#' `corruptors` is a named list of functions accepting `(data, severity, seed)`.
#' `metric_fun` must return a named finite/numeric vector (NA is allowed for
#' metrics that are undefined in a scenario). The executor records software
#' behavior under declared corruptions and does not define universal data-
#' quality thresholds.
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param plan Validation or stress-evidence plan object.
#' @param corruptors Named list of corruption functions used by the stress programme.
#' @param metric_fun Function used to compute the stress-programme evaluation metric.
#' @export
run_eyeprocess_stress_evidence <- function(data, plan, corruptors, metric_fun) {
  if (!inherits(plan, "eye_stress_evidence_plan")) stop("plan must be an eye_stress_evidence_plan.", call. = FALSE)
  if (!is.list(corruptors) || is.null(names(corruptors)) || any(!nzchar(names(corruptors))) || anyDuplicated(names(corruptors)) ||
      any(!vapply(corruptors, is.function, logical(1)))) stop("corruptors must be a uniquely named list of functions.", call. = FALSE)
  if (!is.function(metric_fun)) stop("metric_fun must be a function.", call. = FALSE)
  scenarios <- expand_eyeprocess_stress_evidence_plan(plan)
  missing_corruptors <- setdiff(unique(scenarios$corruption), names(corruptors))
  if (length(missing_corruptors)) stop("Missing corruptors for: ", paste(missing_corruptors, collapse = ", "), call. = FALSE)
  baseline_raw <- metric_fun(data)
  baseline_names <- names(baseline_raw)
  if (is.null(baseline_names) || !length(baseline_raw) || any(!nzchar(baseline_names)) || anyDuplicated(baseline_names))
    stop("metric_fun must return a non-empty uniquely named vector.", call. = FALSE)
  baseline <- suppressWarnings(as.numeric(baseline_raw)); names(baseline) <- baseline_names
  if (any(is.infinite(baseline))) stop("metric_fun cannot return infinite values.", call. = FALSE)
  rows <- list(); failures <- list(); k <- 0L; f <- 0L
  for (i in seq_len(nrow(scenarios))) {
    sc <- scenarios[i, , drop = FALSE]
    corrupted <- try(corruptors[[sc$corruption]](data, sc$severity, sc$seed), silent = TRUE)
    if (inherits(corrupted, "try-error")) {
      f <- f + 1L
      failures[[f]] <- data.frame(scenario_id = sc$scenario_id, corruption = sc$corruption, severity = sc$severity,
                                  seed = sc$seed, error = as.character(corrupted), stringsAsFactors = FALSE)
      next
    }
    value_raw <- try(metric_fun(corrupted), silent = TRUE)
    if (inherits(value_raw, "try-error") || is.null(names(value_raw)) || !setequal(names(value_raw), names(baseline))) {
      f <- f + 1L
      failures[[f]] <- data.frame(scenario_id = sc$scenario_id, corruption = sc$corruption, severity = sc$severity,
                                  seed = sc$seed, error = if (inherits(value_raw, "try-error")) as.character(value_raw) else "metric_fun names changed after corruption", stringsAsFactors = FALSE)
      next
    }
    value <- suppressWarnings(as.numeric(value_raw[names(baseline)]))
    if (any(is.infinite(value))) stop("metric_fun cannot return infinite values.", call. = FALSE)
    for (j in seq_along(baseline)) {
      k <- k + 1L
      b <- baseline[[j]]; v <- value[[j]]
      rows[[k]] <- data.frame(
        scenario_id = sc$scenario_id, corruption = sc$corruption, severity = sc$severity, seed = sc$seed,
        metric = names(baseline)[[j]], baseline = b, value = v,
        delta = if (is.finite(b) && is.finite(v)) v - b else NA_real_,
        relative_change = if (is.finite(b) && b != 0 && is.finite(v)) (v - b) / abs(b) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  results <- if (length(rows)) do.call(rbind, rows) else data.frame()
  failure_tab <- if (length(failures)) do.call(rbind, failures) else data.frame(scenario_id=character(),corruption=character(),severity=numeric(),seed=integer(),error=character())
  structure(list(plan = plan, scenarios = scenarios, baseline = baseline, results = results, failures = failure_tab,
                 guardrail = "Stress evidence describes software/measurement behavior under declared synthetic corruptions; thresholds remain study-specific."),
            class = "eye_stress_evidence_result")
}

#' Summarise executed measurement-stress evidence
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @export
summarise_eyeprocess_stress_evidence <- function(x) {
  if (!inherits(x, "eye_stress_evidence_result")) stop("x must be an eye_stress_evidence_result.", call. = FALSE)
  if (!nrow(x$results)) return(data.frame())
  keys <- interaction(x$results$corruption, x$results$metric, drop = TRUE, lex.order = TRUE)
  rows <- split(seq_len(nrow(x$results)), keys)
  do.call(rbind, lapply(rows, function(ii) {
    z <- x$results[ii, , drop = FALSE]
    finite_delta <- z$delta[is.finite(z$delta)]
    data.frame(corruption = z$corruption[[1L]], metric = z$metric[[1L]], n_scenarios = nrow(z),
               min_severity = min(z$severity), max_severity = max(z$severity),
               mean_delta = if (length(finite_delta)) mean(finite_delta) else NA_real_,
               max_abs_delta = if (length(finite_delta)) max(abs(finite_delta)) else NA_real_,
               stringsAsFactors = FALSE)
  }))
}

#' @export
print.eye_stress_evidence_result <- function(x, ...) {
  cat("eyeprocess executed measurement-stress evidence\n")
  cat("  scenarios:", nrow(x$scenarios), "\n")
  cat("  result rows:", nrow(x$results), "\n")
  cat("  failures:", nrow(x$failures), "\n")
  invisible(x)
}
