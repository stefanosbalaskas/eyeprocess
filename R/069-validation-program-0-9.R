# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Empirical validation design, simulation, recovery, and frozen references.

.ep09_as_df <- function(x, name = deparse(substitute(x))) {
  if (is.data.frame(x)) return(x)
  out <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(out)) stop(name, " must be coercible to a data.frame.", call. = FALSE)
  out
}

.ep09_req_cols <- function(data, cols, label = "data") {
  miss <- setdiff(cols, names(data))
  if (length(miss)) stop(label, " is missing required column(s): ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

.ep09_num <- function(x) suppressWarnings(as.numeric(as.character(x)))
.ep09_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(.ep09_num(x), na.rm = TRUE)
.ep09_sd <- function(x) if (sum(is.finite(.ep09_num(x))) < 2L) NA_real_ else stats::sd(.ep09_num(x), na.rm = TRUE)
.ep09_rmse <- function(est, truth) {
  d <- .ep09_num(est) - .ep09_num(truth)
  if (!any(is.finite(d))) return(NA_real_)
  sqrt(mean(d^2, na.rm = TRUE))
}
.ep09_bias <- function(est, truth) {
  d <- .ep09_num(est) - .ep09_num(truth)
  if (!any(is.finite(d))) return(NA_real_)
  mean(d, na.rm = TRUE)
}
.ep09_quantile <- function(x, p) {
  x <- .ep09_num(x); x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  as.numeric(stats::quantile(x, probs = p, names = FALSE, na.rm = TRUE, type = 8))
}
.ep09_rbind_fill <- function(xs) {
  xs <- Filter(function(z) !is.null(z) && NROW(z), xs)
  if (!length(xs)) return(data.frame())
  nms <- unique(unlist(lapply(xs, names), use.names = FALSE))
  xs <- lapply(xs, function(z) {
    z <- .ep09_as_df(z)
    miss <- setdiff(nms, names(z))
    for (m in miss) z[[m]] <- NA
    z[nms]
  })
  rownames_out <- NULL
  out <- do.call(rbind, xs)
  rownames(out) <- rownames_out
  out
}

.ep09_hash_object <- function(x) {
  tf <- tempfile(fileext = ".rds")
  on.exit(unlink(tf), add = TRUE)
  saveRDS(x, tf, version = 3)
  unname(tools::md5sum(tf))
}

.ep09_capture <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(expr, error = function(e) e),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  list(
    value = value,
    error = if (inherits(value, "error")) conditionMessage(value) else NA_character_,
    warnings = unique(warnings)
  )
}

#' Define an empirical process-validation design
#'
#' The design is intentionally explicit. It records measurement conditions under
#' which recovery, uncertainty, convergence, and failure behavior will be
#' evaluated. It does not imply that every combination is appropriate for every
#' estimator.
#'
#' @param n_persons Participant counts.
#' @param n_trials Trial/item counts per participant.
#' @param missingness Proportion of generic process observations made missing.
#' @param sampling_rate_hz Nominal sampling rates.
#' @param aoi_error AOI uncertainty regimes.
#' @param calibration_error Calibration-error regimes in user-defined units.
#' @param pupil_dropout Pupil-specific dropout proportions.
#' @param heterogeneity Participant-heterogeneity regimes.
#' @param model_misspecification Logical regimes indicating deliberate mismatch.
#' @param replications Monte Carlo replications per condition.
#' @param seed Master simulation seed.
#' @param label Optional design label.
#' @return An `eye_process_validation_design` object.
#' @export
process_validation_design <- function(
    n_persons = c(50L, 150L, 500L),
    n_trials = c(10L, 30L, 80L),
    missingness = c(0, .05, .15, .30),
    sampling_rate_hz = c(60, 120, 300, 1000),
    aoi_error = c("low", "moderate", "severe"),
    calibration_error = c(0, .5, 1),
    pupil_dropout = c(0, .10, .30),
    heterogeneity = c("low", "moderate"),
    model_misspecification = c(FALSE, TRUE),
    replications = 100L,
    seed = 1L,
    label = "process_validation") {
  x <- list(
    n_persons = as.integer(n_persons),
    n_trials = as.integer(n_trials),
    missingness = as.numeric(missingness),
    sampling_rate_hz = as.numeric(sampling_rate_hz),
    aoi_error = as.character(aoi_error),
    calibration_error = as.numeric(calibration_error),
    pupil_dropout = as.numeric(pupil_dropout),
    heterogeneity = as.character(heterogeneity),
    model_misspecification = as.logical(model_misspecification),
    replications = as.integer(replications)[1L],
    seed = as.integer(seed)[1L],
    label = as.character(label)[1L],
    status = "validation_design"
  )
  class(x) <- "eye_process_validation_design"
  validate_process_validation_design(x)
  x
}

#' Validate a process-validation design
#' @param x Validation design.
#' @return A logical value or vector indicating a process-validation design.
#' @export
validate_process_validation_design <- function(x) {
  if (!inherits(x, "eye_process_validation_design")) stop("x must be an eye_process_validation_design.", call. = FALSE)
  if (!length(x$n_persons) || any(!is.finite(x$n_persons)) || any(x$n_persons < 2L))
    stop("n_persons must contain integers >= 2.", call. = FALSE)
  if (!length(x$n_trials) || any(!is.finite(x$n_trials)) || any(x$n_trials < 2L))
    stop("n_trials must contain integers >= 2.", call. = FALSE)
  if (!length(x$missingness) || any(!is.finite(x$missingness)) || any(x$missingness < 0 | x$missingness >= 1))
    stop("missingness must lie in [0, 1).", call. = FALSE)
  if (!length(x$pupil_dropout) || any(!is.finite(x$pupil_dropout)) || any(x$pupil_dropout < 0 | x$pupil_dropout >= 1))
    stop("pupil_dropout must lie in [0, 1).", call. = FALSE)
  if (!length(x$sampling_rate_hz) || any(!is.finite(x$sampling_rate_hz)) || any(x$sampling_rate_hz <= 0))
    stop("sampling_rate_hz must be positive.", call. = FALSE)
  if (!length(x$calibration_error) || any(!is.finite(x$calibration_error)) || any(x$calibration_error < 0))
    stop("calibration_error must contain finite non-negative values.", call. = FALSE)
  if (!length(x$model_misspecification) || any(is.na(x$model_misspecification)))
    stop("model_misspecification must contain non-missing logical values.", call. = FALSE)
  if (length(x$seed) != 1L || !is.finite(x$seed) || x$seed < 0) stop("seed must be a finite non-negative integer.", call. = FALSE)
  if (!length(x$aoi_error) || any(is.na(x$aoi_error)) || any(!nzchar(x$aoi_error))) stop("aoi_error cannot be empty.", call. = FALSE)
  if (!length(x$heterogeneity) || any(is.na(x$heterogeneity)) || any(!nzchar(x$heterogeneity))) stop("heterogeneity cannot be empty.", call. = FALSE)
  if (length(x$replications) != 1L || !is.finite(x$replications) || x$replications < 1L)
    stop("replications must be a positive integer.", call. = FALSE)
  invisible(TRUE)
}

#' Expand a process-validation design into explicit conditions
#' @param x Validation design.
#' @param max_conditions Optional hard cap for accidental combinatorial explosion.
#' @return A tabular R object containing expand a process-validation design into explicit conditions; rows represent analysis units and columns contain the returned quantities.
#' @export
expand_process_validation_design <- function(x, max_conditions = 250000L) {
  validate_process_validation_design(x)
  if (!is.numeric(max_conditions) || length(max_conditions) != 1L || is.na(max_conditions) || max_conditions < 0)
    stop("max_conditions must be a non-negative scalar or Inf.", call. = FALSE)
  g <- expand.grid(
    n_persons = x$n_persons,
    n_trials = x$n_trials,
    missingness = x$missingness,
    sampling_rate_hz = x$sampling_rate_hz,
    aoi_error = x$aoi_error,
    calibration_error = x$calibration_error,
    pupil_dropout = x$pupil_dropout,
    heterogeneity = x$heterogeneity,
    model_misspecification = x$model_misspecification,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  if (nrow(g) > max_conditions) {
    stop("Validation design expands to ", nrow(g), " conditions; max_conditions=", max_conditions, ".", call. = FALSE)
  }
  g$condition_id <- sprintf("C%06d", seq_len(nrow(g)))
  g$replications <- x$replications
  g$design_label <- x$label
  out <- g[, c("condition_id", setdiff(names(g), "condition_id")), drop = FALSE]
  attr(out, "master_seed") <- x$seed
  out
}

#' Return stable validation condition identifiers
#' @param x Validation design or expanded condition table.
#' @return A character value or vector containing return stable validation condition identifiers.
#' @export
validation_condition_id <- function(x) {
  if (inherits(x, "eye_process_validation_design")) x <- expand_process_validation_design(x)
  x <- .ep09_as_df(x)
  if (!"condition_id" %in% names(x)) stop("x has no condition_id column.", call. = FALSE)
  as.character(x$condition_id)
}

#' Simulate a generic multimodal validation dataset with known truth
#'
#' This simulator is a neutral software-validation fixture, not a substantive
#' psychological data-generating model. The generated process channels should
#' not be interpreted as mental-state measurements.
#'
#' @param condition One-row validation condition.
#' @param replication Replication index.
#' @param seed Optional seed override.
#' @param beta Known effect of `x` on the generic process outcome.
#' @return List with `data` and `truth`.
#' @export
simulate_process_validation_data <- function(condition, replication = 1L, seed = NULL, beta = .35) {
  condition <- .ep09_as_df(condition)
  if (nrow(condition) != 1L) stop("condition must contain exactly one row.", call. = FALSE)
  .ep09_req_cols(condition, c("n_persons", "n_trials", "missingness", "sampling_rate_hz",
                              "aoi_error", "calibration_error", "pupil_dropout", "heterogeneity",
                              "model_misspecification"), "condition")
  if (is.null(seed)) seed <- 100000L + as.integer(replication)
  if (length(seed) != 1L || !is.finite(seed) || seed < 0) stop("seed must be a finite non-negative scalar.", call. = FALSE)
  if (length(beta) != 1L || !is.finite(beta)) stop("beta must be a finite scalar.", call. = FALSE)
  seed_value <- as.integer((as.double(seed) %% (.Machine$integer.max - 1)) + 1)
  set.seed(seed_value)
  n <- as.integer(condition$n_persons[[1L]])
  t <- as.integer(condition$n_trials[[1L]])
  hetero_sd <- switch(as.character(condition$heterogeneity[[1L]]), low = .25, moderate = .60, high = 1, .60)
  aoi_sd <- switch(as.character(condition$aoi_error[[1L]]), low = .01, moderate = .03, severe = .08, .03)
  pid <- rep(seq_len(n), each = t)
  trial <- rep(seq_len(t), times = n)
  x <- stats::rnorm(n * t)
  u <- rep(stats::rnorm(n, sd = hetero_sd), each = t)
  eps <- stats::rnorm(n * t, sd = 1)
  omitted <- .60 * x + sqrt(1 - .60^2) * stats::rnorm(n * t)
  misspecified <- isTRUE(as.logical(condition$model_misspecification[[1L]]))
  y <- beta * x + u + eps + if (misspecified) .35 * omitted else 0
  pupil <- 3.5 + .12 * x + .15 * u + stats::rnorm(n * t, sd = .20)
  gaze_x <- stats::plogis(.3 * x + stats::rnorm(n * t, sd = .6)) + stats::rnorm(n * t, sd = aoi_sd)
  gaze_y <- stats::plogis(-.2 * x + stats::rnorm(n * t, sd = .6)) + stats::rnorm(n * t, sd = aoi_sd)
  cal <- as.numeric(condition$calibration_error[[1L]])
  if (is.finite(cal) && cal != 0) {
    gaze_x <- gaze_x + stats::rnorm(n * t, sd = cal / 100)
    gaze_y <- gaze_y + stats::rnorm(n * t, sd = cal / 100)
  }
  miss <- stats::runif(n * t) < as.numeric(condition$missingness[[1L]])
  drop <- stats::runif(n * t) < as.numeric(condition$pupil_dropout[[1L]])
  y[miss] <- NA_real_
  gaze_x[miss] <- NA_real_; gaze_y[miss] <- NA_real_
  pupil[drop] <- NA_real_
  interval <- 1000 / as.numeric(condition$sampling_rate_hz[[1L]])
  timestamp_ms <- (trial - 1) * interval + stats::rnorm(n * t, sd = interval * .02)
  d <- data.frame(
    person_id = pid,
    trial_id = trial,
    timestamp_ms = timestamp_ms,
    x = x,
    omitted_structure = omitted,
    process_value = y,
    pupil = pupil,
    gaze_x = gaze_x,
    gaze_y = gaze_y,
    valid_gaze = is.finite(gaze_x) & is.finite(gaze_y),
    stringsAsFactors = FALSE
  )
  truth <- data.frame(parameter = "beta_x", truth = beta, stringsAsFactors = FALSE)
  list(data = d, truth = truth, condition = condition, replication = as.integer(replication))
}

.ep09_default_validation_fit <- function(simulated, condition = NULL) {
  d <- simulated$data
  stats::lm(process_value ~ x, data = d)
}

.ep09_default_validation_extract <- function(fit, simulated, condition = NULL) {
  cf <- summary(fit)$coefficients
  est <- if ("x" %in% rownames(cf)) cf["x", "Estimate"] else NA_real_
  se <- if ("x" %in% rownames(cf)) cf["x", "Std. Error"] else NA_real_
  truth <- simulated$truth$truth[match("beta_x", simulated$truth$parameter)]
  data.frame(
    parameter = "beta_x",
    truth = truth,
    estimate = est,
    se = se,
    lower = est - 1.96 * se,
    upper = est + 1.96 * se,
    converged = is.finite(est),
    stringsAsFactors = FALSE
  )
}

#' Run an empirical process-validation programme
#'
#' @param design Validation design or expanded condition table.
#' @param simulate_fun Function `(condition, replication, seed)` returning a simulation object.
#' @param fit_fun Function `(simulated, condition)` returning a fitted object.
#' @param extract_fun Function `(fit, simulated, condition)` returning one or more rows with estimates.
#' @param max_conditions Optional cap on conditions actually run.
#' @param progress Print compact progress messages.
#' @return An `eye_process_validation_result` object.
#' @export
run_process_validation <- function(
    design,
    simulate_fun = simulate_process_validation_data,
    fit_fun = .ep09_default_validation_fit,
    extract_fun = .ep09_default_validation_extract,
    max_conditions = Inf,
    progress = interactive()) {
  if (inherits(design, "eye_process_validation_design")) {
    master_seed <- design$seed
    cond <- expand_process_validation_design(design)
  } else {
    master_seed <- attr(design, "master_seed", exact = TRUE)
    cond <- .ep09_as_df(design)
    if (is.null(master_seed) || !is.finite(master_seed)) master_seed <- 1L
    .ep09_req_cols(cond, c("condition_id", "replications"), "design")
  }
  if (!is.numeric(max_conditions) || length(max_conditions) != 1L || is.na(max_conditions) || max_conditions < 0)
    stop("max_conditions must be a non-negative scalar or Inf.", call. = FALSE)
  if (is.finite(max_conditions) && nrow(cond) > max_conditions) cond <- cond[seq_len(as.integer(max_conditions)), , drop = FALSE]
  rows <- list(); failures <- list(); warnings <- list(); k <- 0L; fk <- 0L; wk <- 0L
  for (i in seq_len(nrow(cond))) {
    cnd <- cond[i, , drop = FALSE]
    reps <- as.integer(cnd$replications[[1L]])
    if (!is.finite(reps) || reps < 1L) next
    for (r in seq_len(reps)) {
      # Use double arithmetic before wrapping into the valid positive integer
      # seed range, avoiding overflow for large validation grids.
      seed_i <- as.integer(((as.double(master_seed) + as.double(i) * 100000 + r) %% (.Machine$integer.max - 1)) + 1)
      if (isTRUE(progress)) message("validation ", cnd$condition_id[[1L]], " replication ", r, "/", reps)
      sim_cap <- .ep09_capture(simulate_fun(cnd, r, seed_i))
      if (!is.na(sim_cap$error)) {
        fk <- fk + 1L
        failures[[fk]] <- data.frame(condition_id = cnd$condition_id[[1L]], replication = r,
                                     stage = "simulate", error = sim_cap$error, stringsAsFactors = FALSE)
        next
      }
      if (length(sim_cap$warnings)) {
        wk <- wk + 1L
        warnings[[wk]] <- data.frame(condition_id = cnd$condition_id[[1L]], replication = r,
                                     stage = "simulate", warning = paste(sim_cap$warnings, collapse = " | "), stringsAsFactors = FALSE)
      }
      fit_cap <- .ep09_capture(fit_fun(sim_cap$value, cnd))
      if (!is.na(fit_cap$error)) {
        fk <- fk + 1L
        failures[[fk]] <- data.frame(condition_id = cnd$condition_id[[1L]], replication = r,
                                     stage = "fit", error = fit_cap$error, stringsAsFactors = FALSE)
        next
      }
      if (length(fit_cap$warnings)) {
        wk <- wk + 1L
        warnings[[wk]] <- data.frame(condition_id = cnd$condition_id[[1L]], replication = r,
                                     stage = "fit", warning = paste(fit_cap$warnings, collapse = " | "), stringsAsFactors = FALSE)
      }
      ext_cap <- .ep09_capture(extract_fun(fit_cap$value, sim_cap$value, cnd))
      if (!is.na(ext_cap$error)) {
        fk <- fk + 1L
        failures[[fk]] <- data.frame(condition_id = cnd$condition_id[[1L]], replication = r,
                                     stage = "extract", error = ext_cap$error, stringsAsFactors = FALSE)
        next
      }
      if (length(ext_cap$warnings)) {
        wk <- wk + 1L
        warnings[[wk]] <- data.frame(condition_id = cnd$condition_id[[1L]], replication = r,
                                     stage = "extract", warning = paste(ext_cap$warnings, collapse = " | "), stringsAsFactors = FALSE)
      }
      ext <- .ep09_as_df(ext_cap$value, "extract_fun result")
      if (!nrow(ext)) next
      k <- k + 1L
      ext$condition_id <- cnd$condition_id[[1L]]
      ext$replication <- r
      ext$seed <- seed_i
      for (nm in setdiff(names(cnd), c("condition_id", "replications"))) ext[[nm]] <- cnd[[nm]][[1L]]
      rows[[k]] <- ext
    }
  }
  estimates <- .ep09_rbind_fill(rows)
  failure_tab <- .ep09_rbind_fill(failures)
  warning_tab <- .ep09_rbind_fill(warnings)
  structure(list(
    design = cond,
    estimates = estimates,
    failures = failure_tab,
    warnings = warning_tab,
    created_at = as.character(Sys.time()),
    design_hash = .ep09_hash_object(cond),
    status = "empirical_validation_result",
    caveat = paste(
      "Recovery and robustness are conditional on the supplied data-generating process, estimator, and extraction rules.",
      "Passing a simulation study does not establish validity outside the simulated conditions."
    )
  ), class = "eye_process_validation_result")
}

#' Summarise a process-validation result
#' @param x Validation result.
#' @param by Optional grouping variables in addition to parameter.
#' @return A tabular R object containing a process-validation result; rows represent analysis units and columns contain the returned quantities.
#' @export
summarise_process_validation <- function(x, by = NULL) {
  if (!inherits(x, "eye_process_validation_result")) stop("x must be an eye_process_validation_result.", call. = FALSE)
  d <- x$estimates
  if (!nrow(d)) return(data.frame())
  .ep09_req_cols(d, c("parameter", "estimate", "truth"), "x$estimates")
  groups <- unique(c("parameter", by))
  groups <- groups[groups %in% names(d)]
  key <- interaction(d[groups], drop = TRUE, lex.order = TRUE)
  spl <- split(seq_len(nrow(d)), key)
  out <- lapply(spl, function(idx) {
    z <- d[idx, , drop = FALSE]
    headvals <- z[1L, groups, drop = FALSE]
    coverage <- if (all(c("lower", "upper") %in% names(z))) {
      cv <- z$lower <= z$truth & z$upper >= z$truth
      if (any(!is.na(cv))) mean(cv, na.rm = TRUE) else NA_real_
    } else NA_real_
    conv <- if ("converged" %in% names(z)) {
      cc <- as.logical(z$converged); if (any(!is.na(cc))) mean(cc, na.rm = TRUE) else NA_real_
    } else mean(is.finite(z$estimate))
    ae <- abs(.ep09_num(z$estimate) - .ep09_num(z$truth)); ae <- ae[is.finite(ae)]
    cbind(headvals, data.frame(
      n = nrow(z),
      bias = .ep09_bias(z$estimate, z$truth),
      rmse = .ep09_rmse(z$estimate, z$truth),
      mae = if (length(ae)) mean(ae) else NA_real_,
      coverage = coverage,
      convergence_rate = conv,
      estimate_sd = .ep09_sd(z$estimate),
      stringsAsFactors = FALSE
    ))
  })
  .ep09_rbind_fill(out)
}

#' Parameter-recovery table
#' @param x Validation result.
#' @param by Optional grouping variables.
#' @return A tabular R object containing parameter-recovery table; rows represent analysis units and columns contain the returned quantities.
#' @export
validation_recovery_table <- function(x, by = NULL) {
  s <- summarise_process_validation(x, by = by)
  keep <- intersect(c("parameter", by, "n", "bias", "rmse", "mae", "estimate_sd"), names(s))
  s[keep]
}

#' Interval-coverage table
#' @param x Validation result.
#' @param nominal Nominal coverage used for deviation reporting.
#' @param by Optional grouping variables.
#' @return A tabular R object containing interval-coverage table; rows represent analysis units and columns contain the returned quantities.
#' @export
validation_coverage_table <- function(x, nominal = .95, by = NULL) {
  s <- summarise_process_validation(x, by = by)
  if (!nrow(s)) return(s)
  s$nominal <- nominal
  s$coverage_error <- s$coverage - nominal
  keep <- intersect(c("parameter", by, "n", "coverage", "nominal", "coverage_error"), names(s))
  s[keep]
}

#' Failure profile for a validation programme
#' @param x Validation result.
#' @return A data frame containing failure profile for a validation programme. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
validation_failure_profile <- function(x) {
  if (!inherits(x, "eye_process_validation_result")) stop("x must be an eye_process_validation_result.", call. = FALSE)
  total_attempts <- sum(as.integer(x$design$replications), na.rm = TRUE)
  if (!nrow(x$failures)) {
    return(data.frame(stage = character(), failures = integer(), total_attempts = integer(), failure_rate = numeric()))
  }
  tab <- as.data.frame(table(x$failures$stage), stringsAsFactors = FALSE)
  names(tab) <- c("stage", "failures")
  tab$total_attempts <- total_attempts
  tab$failure_rate <- tab$failures / total_attempts
  tab
}

#' Monte Carlo standard-error diagnostics for validation summaries
#' @param x Validation result.
#' @param by Optional grouping variables.
#' @return A tabular R object containing monte Carlo standard-error diagnostics for validation summaries; rows represent analysis units and columns contain the returned quantities.
#' @export
validation_summary_mcse <- function(x, by = NULL) {
  d <- x$estimates
  if (!nrow(d)) return(data.frame())
  groups <- unique(c("parameter", by)); groups <- groups[groups %in% names(d)]
  key <- interaction(d[groups], drop = TRUE, lex.order = TRUE)
  out <- lapply(split(seq_len(nrow(d)), key), function(idx) {
    z <- d[idx, , drop = FALSE]
    err <- .ep09_num(z$estimate) - .ep09_num(z$truth)
    n <- sum(is.finite(err))
    headvals <- z[1L, groups, drop = FALSE]
    cbind(headvals, data.frame(
      n = n,
      mcse_bias = if (n > 1L) stats::sd(err, na.rm = TRUE) / sqrt(n) else NA_real_,
      mcse_mean_estimate = if (n > 1L) stats::sd(z$estimate, na.rm = TRUE) / sqrt(n) else NA_real_,
      stringsAsFactors = FALSE
    ))
  })
  .ep09_rbind_fill(out)
}

#' Rank validation conditions by a transparent robustness score
#' @param x Validation result.
#' @param weights Named weights for rmse, absolute bias, coverage error, and failure rate.
#' @return A tabular R object containing rank validation conditions by a transparent robustness score; rows represent analysis units and columns contain the returned quantities.
#' @export
validation_condition_ranking <- function(
    x,
    weights = c(rmse = 1, abs_bias = 1, coverage_error = 1, failure_rate = 1)) {
  required_weights <- c("rmse", "abs_bias", "coverage_error", "failure_rate")
  if (!is.numeric(weights) || is.null(names(weights)) || !all(required_weights %in% names(weights)) ||
      any(!is.finite(weights[required_weights])))
    stop("weights must be a finite named numeric vector containing rmse, abs_bias, coverage_error, and failure_rate.", call. = FALSE)
  s <- summarise_process_validation(x, by = "condition_id")
  if (!nrow(s)) return(s)
  f <- if (nrow(x$failures)) aggregate(rep(1, nrow(x$failures)), list(condition_id = x$failures$condition_id), sum) else data.frame(condition_id = character(), x = numeric())
  names(f)[names(f) == "x"] <- "failures"
  s <- merge(s, f, by = "condition_id", all.x = TRUE)
  s$failures[is.na(s$failures)] <- 0
  reps <- x$design$replications[match(s$condition_id, x$design$condition_id)]
  s$failure_rate <- s$failures / reps
  s$abs_bias <- abs(s$bias)
  s$coverage_error <- abs(s$coverage - .95)
  scale01 <- function(v) {
    v <- .ep09_num(v); finite <- v[is.finite(v)]
    if (!length(finite)) return(rep(0, length(v)))
    rng <- range(finite)
    if (diff(rng) == 0) return(rep(0, length(v)))
    out <- (v - rng[1L]) / diff(rng); out[!is.finite(out)] <- 0; out
  }
  w <- weights[required_weights]
  denom <- sum(abs(w)); if (denom == 0) denom <- 1
  penalty <- (w[["rmse"]] * scale01(s$rmse) +
              w[["abs_bias"]] * scale01(s$abs_bias) +
              w[["coverage_error"]] * scale01(s$coverage_error) +
              w[["failure_rate"]] * scale01(s$failure_rate)) / denom
  s$robustness_score <- 1 - penalty
  s[order(-s$robustness_score), , drop = FALSE]
}

#' Overall validation robustness score
#' @param x Validation result.
#' @return A single numeric robustness score: the mean finite condition-level robustness score, or `NA_real_` when no finite score is available.
#' @export
validation_robustness_score <- function(x) {
  r <- validation_condition_ranking(x)
  if (!nrow(r)) return(NA_real_)
  z <- r$robustness_score[is.finite(r$robustness_score)]
  if (length(z)) mean(z) else NA_real_
}

#' Freeze a compact validation reference for regression testing
#' @param x Validation result.
#' @param path Optional RDS path.
#' @param digits Numeric rounding applied before hashing.
#' @return An object of class "eye_validation_reference", stored as a named list, with components "summary", "failure_profile", "design_hash", "summary_hash", "created_at", "status". It contains freeze a compact validation reference for regression testing and associated metadata or diagnostics needed to interpret the result.
#' @export
freeze_validation_reference <- function(x, path = NULL, digits = 8L) {
  if (!inherits(x, "eye_process_validation_result")) stop("x must be an eye_process_validation_result.", call. = FALSE)
  if (length(digits) != 1L || !is.finite(digits) || digits < 0 || digits != as.integer(digits))
    stop("digits must be a non-negative integer.", call. = FALSE)
  s <- summarise_process_validation(x, by = "condition_id")
  num <- vapply(s, is.numeric, logical(1))
  s[num] <- lapply(s[num], round, digits = digits)
  ref <- structure(list(
    summary = s,
    failure_profile = validation_failure_profile(x),
    design_hash = x$design_hash,
    summary_hash = .ep09_hash_object(s),
    created_at = as.character(Sys.time()),
    status = "frozen_validation_reference"
  ), class = "eye_validation_reference")
  if (!is.null(path)) saveRDS(ref, path, version = 3)
  ref
}

#' Compare a validation result with a frozen reference
#' @param x Validation result.
#' @param reference Frozen reference object or RDS path.
#' @param tolerance Numeric tolerance for matched summary values.
#' @return An object of class "eye_validation_reference_comparison", stored as a named list, with components "table", "tolerance", "pass", "reference_hash", "current_hash". It contains a validation result with a frozen reference and associated metadata or diagnostics needed to interpret the result.
#' @export
validate_against_reference <- function(x, reference, tolerance = 1e-6) {
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) stop("tolerance must be a finite non-negative scalar.", call. = FALSE)
  if (is.character(reference) && length(reference) == 1L) reference <- readRDS(reference)
  if (!inherits(reference, "eye_validation_reference")) stop("reference must be an eye_validation_reference or RDS path.", call. = FALSE)
  cur <- summarise_process_validation(x, by = "condition_id")
  ref <- reference$summary
  keys <- intersect(c("condition_id", "parameter"), intersect(names(cur), names(ref)))
  if (!length(keys)) stop("No common keys between current and frozen summaries.", call. = FALSE)
  ref$.present_reference <- TRUE
  cur$.present_current <- TRUE
  m <- merge(ref, cur, by = keys, suffixes = c("_reference", "_current"), all = TRUE)
  matched <- !is.na(m$.present_reference) & !is.na(m$.present_current)
  metrics <- intersect(c("bias", "rmse", "coverage", "convergence_rate"), names(reference$summary))
  for (metric in metrics) {
    a <- m[[paste0(metric, "_reference")]]; b <- m[[paste0(metric, "_current")]]
    m[[paste0(metric, "_delta")]] <- .ep09_num(b) - .ep09_num(a)
  }
  delta_cols <- grep("_delta$", names(m), value = TRUE)
  numeric_ok <- length(delta_cols) && all(vapply(m[delta_cols], function(v) all(is.na(v[matched]) | abs(v[matched]) <= tolerance), logical(1)))
  pass <- nrow(m) > 0L && all(matched) && numeric_ok
  structure(list(table = m, tolerance = tolerance, pass = pass,
                 reference_hash = reference$summary_hash,
                 current_hash = .ep09_hash_object(cur)),
            class = "eye_validation_reference_comparison")
}

#' Create a model-by-evidence validation matrix
#' @param ... Named validation results, bundles, or arbitrary evidence objects.
#' @return A tabular R object containing a model-by-evidence validation matrix; rows represent analysis units and columns contain the returned quantities.
#' @export
validation_evidence_matrix <- function(...) {
  xs <- list(...)
  if (!length(xs)) return(data.frame())
  if (is.null(names(xs)) || any(!nzchar(names(xs)))) stop("All evidence objects must be named.", call. = FALSE)
  evidence_types <- c("recovery", "bias", "rmse", "coverage", "convergence", "failure", "sensitivity", "negative_controls", "external_validation", "provenance")
  rows <- lapply(names(xs), function(nm) {
    x <- xs[[nm]]
    available <- rep(FALSE, length(evidence_types)); names(available) <- evidence_types
    if (inherits(x, "eye_process_validation_result")) {
      available[c("recovery", "bias", "rmse", "coverage", "convergence", "failure")] <- TRUE
    }
    if (inherits(x, "eye_validation_bundle")) {
      ev <- names(x$evidence)
      available[intersect(names(available), ev)] <- TRUE
    }
    data.frame(model = nm, t(available), check.names = FALSE, stringsAsFactors = FALSE)
  })
  .ep09_rbind_fill(rows)
}

#' @export
print.eye_process_validation_design <- function(x, ...) {
  g <- expand_process_validation_design(x)
  cat("eyeprocess empirical validation design\n")
  cat("  label       :", x$label, "\n")
  cat("  conditions  :", nrow(g), "\n")
  cat("  replications:", x$replications, "per condition\n")
  cat("  total fits  :", nrow(g) * x$replications, "\n")
  invisible(x)
}

#' @export
print.eye_process_validation_result <- function(x, ...) {
  cat("eyeprocess empirical validation result\n")
  cat("  conditions :", nrow(x$design), "\n")
  cat("  estimates  :", nrow(x$estimates), "\n")
  cat("  failures   :", nrow(x$failures), "\n")
  cat("  warnings   :", nrow(x$warnings), "\n")
  invisible(x)
}

#' @export
summary.eye_process_validation_result <- function(object, ...) {
  summarise_process_validation(object, ...)
}
