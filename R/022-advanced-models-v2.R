# Advanced psychometric process model programme -------------------------------

.ep_sequence_transitions <- function(x, source = "samples", collapse_consecutive = TRUE) {
  seqs <- scanpath_sequence(x, source = source, collapse_consecutive = collapse_consecutive)
  if (!nrow(seqs)) return(data.frame())
  trials <- trial_table(x)
  responses <- x$responses
  rows <- list(); k <- 0L
  for (i in seq_len(nrow(seqs))) {
    states <- strsplit(seqs$sequence[[i]], " > ", fixed = TRUE)[[1L]]
    if (length(states) < 2L) next
    rec <- seqs$recording_id[[i]]; tr <- seqs$trial_id[[i]]
    trial_row <- trials[trials$recording_id == rec & trials$trial_id == tr, , drop = FALSE]
    response_row <- responses[responses$recording_id == rec & responses$trial_id == tr, , drop = FALSE]
    k <- k + 1L
    rows[[k]] <- data.frame(
      recording_id = rec, trial_id = tr,
      participant_id = if (nrow(trial_row)) trial_row$participant_id[[1L]] else if (nrow(response_row)) response_row$participant_id[[1L]] else NA_character_,
      item_id = if (nrow(trial_row)) trial_row$item_id[[1L]] else if (nrow(response_row)) response_row$item_id[[1L]] else NA_character_,
      score = if (nrow(response_row)) response_row$score[[1L]] else NA_real_,
      step = seq_len(length(states) - 1L), from = head(states, -1L), to = tail(states, -1L),
      stringsAsFactors = FALSE
    )
  }
  if (length(rows)) do.call(rbind, rows) else data.frame()
}

#' Specify a dynamic gaze-state IRTree
#'
#' @param source Sequence source.
#' @param collapse_consecutive Collapse consecutive identical states.
#' @param include_response Include item response as a transition predictor.
#' @param include_person Include person fixed effects.
#' @param include_item Include item fixed effects.
#' @return An `eye_dynamic_irtree_spec`.
#' @export
dynamic_irtree_spec <- function(
    source = c("samples", "visits", "fixations"),
    collapse_consecutive = TRUE,
    include_response = TRUE,
    include_person = FALSE,
    include_item = TRUE) {
  structure(list(
    source = match.arg(source), collapse_consecutive = isTRUE(collapse_consecutive),
    include_response = isTRUE(include_response), include_person = isTRUE(include_person),
    include_item = isTRUE(include_item)
  ), class = "eye_dynamic_irtree_spec")
}

#' Fit a dynamic gaze-state response-tree model
#'
#' Fits one-vs-rest transition logits for each observed destination state. The
#' model preserves transition order, person/item structure, and optional item
#' response predictors. It is an auditable dynamic baseline rather than a claim
#' that observed AOI states are latent cognitive states.
#'
#' @param x An `eye_dataset`.
#' @param spec Dynamic IRTree specification.
#' @param min_transitions Minimum transitions required per destination state.
#' @return An `eye_dynamic_irtree` object.
#' @export
fit_dynamic_irtree <- function(x, spec = dynamic_irtree_spec(), min_transitions = 10L) {
  .assert_eye_dataset(x)
  min_transitions <- as.integer(min_transitions)
  if (length(min_transitions) != 1L || is.na(min_transitions) || min_transitions < 1L) {
    .eye_stop("`min_transitions` must be a positive integer.")
  }
  if (!inherits(spec, "eye_dynamic_irtree_spec")) .eye_stop("`spec` must be created by `dynamic_irtree_spec()`.")
  d <- .ep_sequence_transitions(x, spec$source, spec$collapse_consecutive)
  if (!nrow(d)) .eye_stop("No AOI transitions are available.")
  states <- sort(unique(c(d$from, d$to)))
  fits <- setNames(vector("list", length(states)), states)
  fit_warnings <- setNames(vector("list", length(states)), states)
  coefficients <- list()
  rhs <- c("from", "stats::poly(step, 2, raw = TRUE)")
  if (spec$include_response && any(is.finite(d$score))) rhs <- c(rhs, "score")
  if (spec$include_item && any(!is.na(d$item_id))) rhs <- c(rhs, "item_id")
  if (spec$include_person && any(!is.na(d$participant_id))) rhs <- c(rhs, "participant_id")
  for (state in states) {
    z <- d; z$.destination <- as.integer(z$to == state)
    if (sum(z$.destination) < min_transitions || sum(1 - z$.destination) < min_transitions) next
    formula <- stats::as.formula(paste(".destination ~", paste(rhs, collapse = " + ")))
    state_warnings <- character()
    fits[[state]] <- withCallingHandlers(
      stats::glm(formula, family = stats::binomial(), data = z),
      warning = function(w) {
        state_warnings <<- c(state_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    fit_warnings[[state]] <- unique(state_warnings)
    co <- stats::coef(summary(fits[[state]]))
    coefficients[[state]] <- data.frame(destination = state, term = rownames(co), estimate = co[, 1L], std_error = co[, 2L], statistic = co[, 3L], p_value = co[, 4L], row.names = NULL)
  }
  keep <- !vapply(fits, is.null, logical(1))
  fits <- fits[keep]
  fit_warnings <- fit_warnings[keep]
  if (!length(fits)) .eye_stop("No destination state had sufficient transition support.")
  out <- list(
    spec = spec, transitions = d, states = states, fits = fits,
    coefficients = do.call(rbind, coefficients), fit_warnings = fit_warnings,
    warning = "AOI states and fitted transition components are observational; cognitive-state labels require external validation."
  )
  class(out) <- "eye_dynamic_irtree"
  out
}

#' @export
print.eye_dynamic_irtree <- function(x, ...) {
  cat("Dynamic gaze-state IRTree baseline\n")
  cat("Transitions: ", nrow(x$transitions), "\n", sep = "")
  cat("States:      ", paste(x$states, collapse = ", "), "\n", sep = "")
  cat("Models:      ", length(x$fits), "\n", sep = "")
  invisible(x)
}

#' @export
plot.eye_dynamic_irtree <- function(x, ...) {
  tab <- table(x$transitions$from, x$transitions$to)
  graphics::image(t(tab[nrow(tab):1, , drop = FALSE]), axes = FALSE, main = "Observed transition counts", ...)
  graphics::axis(1, at = seq(0, 1, length.out = ncol(tab)), labels = colnames(tab), las = 2)
  graphics::axis(2, at = seq(0, 1, length.out = nrow(tab)), labels = rev(rownames(tab)), las = 2)
  invisible(tab)
}

#' Specify a functional pupil-IRT model
#' @param df Natural-spline degrees of freedom.
#' @param response Response field.
#' @param engine Two-stage GLM/multilevel engine or joint `brms` engine.
#' @param include_response_time Include a response-time submodel when joint.
#' @return An `eye_functional_pupil_irt_spec`.
#' @export
functional_pupil_irt_spec <- function(df = 5L, response = "score", engine = c("two_stage_glm", "two_stage_lme4", "brms"), include_response_time = TRUE) {
  df <- as.integer(df)
  if (length(df) != 1L || is.na(df) || df < 1L) .eye_stop("`df` must be a positive integer.")
  if (length(response) != 1L || is.na(response) || !nzchar(response)) .eye_stop("`response` must be a non-empty column name.")
  structure(list(df = df, response = response, engine = match.arg(engine), include_response_time = isTRUE(include_response_time)), class = "eye_functional_pupil_irt_spec")
}

#' Fit a functional pupil-informed IRT workflow
#'
#' @param x An `eye_dataset`.
#' @param spec Functional pupil-IRT specification.
#' @param ... Passed to the selected model engine.
#' @return An `eye_functional_pupil_irt` object.
#' @export
fit_joint_functional_pupil_irt <- function(x, spec = functional_pupil_irt_spec(), ...) {
  .assert_eye_dataset(x)
  if (!inherits(spec, "eye_functional_pupil_irt_spec")) .eye_stop("`spec` must be created by `functional_pupil_irt_spec()`.")
  y <- functional_pupil_features(x, df = spec$df, append = TRUE, prefix = "functional_pupil")
  feature_names <- sort(unique(y$features$feature_name[grepl("^functional_pupil_", y$features$feature_name)]))
  if (!length(feature_names)) .eye_stop("No functional pupil coefficients could be derived.")
  formula <- stats::as.formula(paste(spec$response, "~", paste(feature_names, collapse = " + ")))
  if (spec$engine == "two_stage_glm") {
    fit <- fit_explanatory_irt(y, formula, engine = "glm", participant_random = FALSE, item_random = FALSE, ...)
  } else if (spec$engine == "two_stage_lme4") {
    fit <- fit_explanatory_irt(y, formula, engine = "lme4", participant_random = TRUE, item_random = TRUE, ...)
  } else {
    d <- model_data(y, include_features = TRUE)
    process_formulas <- lapply(feature_names, function(nm) stats::as.formula(paste(nm, "~ 1 + (1 | participant_id) + (1 | item_id)")))
    accuracy_formula <- stats::as.formula(paste(spec$response, "~ 1 + (1 | participant_id) + (1 | item_id)"))
    rt_formula <- if (spec$include_response_time) log_response_time ~ 1 + (1 | participant_id) + (1 | item_id) else log_response_time ~ 1
    fit <- fit_joint_process_model(y, accuracy_formula, rt_formula, process_formulas, engine = "brms", ...)
  }
  out <- list(data = y, model = fit, spec = spec, feature_names = feature_names,
              warning = "Spline coefficients are measurement summaries; joint substantive interpretation requires latency, luminance, gaze-position, autocorrelation, and preprocessing sensitivity analyses.")
  class(out) <- "eye_functional_pupil_irt"
  out
}

#' @export
print.eye_functional_pupil_irt <- function(x, ...) {
  cat("Functional pupil-informed IRT workflow\n")
  cat("Engine:   ", x$spec$engine, "\n", sep = "")
  cat("Basis df: ", x$spec$df, "\n", sep = "")
  cat("Features: ", paste(x$feature_names, collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
plot.eye_functional_pupil_irt <- function(x, ...) {
  d <- features_wide(x$data, id_cols = c("participant_id", "trial_id", "item_id"))
  features <- intersect(x$feature_names, names(d))
  if (!length(features)) .eye_stop("Functional pupil features are unavailable for plotting.")
  graphics::matplot(as.matrix(d[features]), type = "l", lty = 1, xlab = "Trial row", ylab = "Basis coefficient", main = "Functional pupil coefficients", ...)
  invisible(d[features])
}

#' Specify theory-defined process strategies
#'
#' @param prototypes Matrix/data frame with strategies in rows and features in columns.
#' @param feature_sd Optional feature standard deviations used in distance likelihoods.
#' @param prior Optional strategy prior probabilities.
#' @return An `eye_theory_strategy_spec`.
#' @export
theory_strategy_spec <- function(prototypes, feature_sd = NULL, prior = NULL) {
  prototypes <- as.matrix(prototypes)
  if (nrow(prototypes) < 2L || ncol(prototypes) < 1L) {
    .eye_stop("`prototypes` must contain at least two strategies and one feature.")
  }
  if (!is.numeric(prototypes) || any(!is.finite(prototypes))) {
    .eye_stop("Strategy prototypes must contain finite numeric values.")
  }
  if (is.null(rownames(prototypes))) rownames(prototypes) <- paste0("strategy_", seq_len(nrow(prototypes)))
  if (is.null(colnames(prototypes)) || any(!nzchar(colnames(prototypes)))) {
    .eye_stop("Strategy prototypes must have non-empty feature column names.")
  }
  if (anyDuplicated(rownames(prototypes)) || anyDuplicated(colnames(prototypes))) {
    .eye_stop("Strategy and feature names must be unique.")
  }
  if (is.null(feature_sd)) feature_sd <- rep(1, ncol(prototypes))
  if (length(feature_sd) == 1L) feature_sd <- rep(feature_sd, ncol(prototypes))
  if (length(feature_sd) != ncol(prototypes) || any(!is.finite(feature_sd)) || any(feature_sd <= 0)) {
    .eye_stop("`feature_sd` must contain positive finite values matching the prototype features.")
  }
  if (is.null(prior)) prior <- rep(1 / nrow(prototypes), nrow(prototypes))
  if (length(prior) != nrow(prototypes) || any(!is.finite(prior)) || any(prior <= 0)) {
    .eye_stop("Strategy priors must be positive finite values matching the number of prototypes.")
  }
  prior <- prior / sum(prior)
  structure(list(prototypes = prototypes, feature_sd = as.numeric(feature_sd), prior = prior), class = "eye_theory_strategy_spec")
}

.ep_softmax <- function(x) {
  x <- sweep(x, 1L, apply(x, 1L, max), "-")
  ex <- exp(x)
  sweep(ex, 1L, rowSums(ex), "/")
}

#' Fit a theory-defined strategy-informed IRT model
#'
#' Strategy prototypes are declared before estimation. Posterior strategy
#' probabilities are computed from Gaussian feature-distance likelihoods and
#' entered into a response model with person and item effects.
#'
#' @param x An `eye_dataset` or model data frame.
#' @param spec Theory strategy specification.
#' @param response Response column.
#' @param participant Participant column.
#' @param item Item column.
#' @return An `eye_theory_strategy_irt` object.
#' @export
fit_theory_strategy_irt <- function(x, spec, response = "score", participant = "participant_id", item = "item_id") {
  if (!inherits(spec, "eye_theory_strategy_spec")) .eye_stop("`spec` must be created by `theory_strategy_spec()`.")
  d <- if (is_eye_dataset(x)) model_data(x, include_features = TRUE) else as.data.frame(x)
  features <- colnames(spec$prototypes)
  .assert_columns(d, c(response, participant, item, features))
  X <- as.matrix(d[features])
  complete <- stats::complete.cases(X)
  log_post <- matrix(NA_real_, nrow(d), nrow(spec$prototypes), dimnames = list(NULL, rownames(spec$prototypes)))
  for (k in seq_len(nrow(spec$prototypes))) {
    z <- sweep(X, 2L, spec$prototypes[k, ], "-")
    z <- sweep(z, 2L, spec$feature_sd, "/")
    log_post[, k] <- -0.5 * rowSums(z^2) + log(spec$prior[k])
  }
  posterior <- matrix(NA_real_, nrow(d), ncol(log_post), dimnames = dimnames(log_post))
  posterior[complete, ] <- .ep_softmax(log_post[complete, , drop = FALSE])
  assignments <- rep(NA_character_, nrow(d))
  if (any(complete)) {
    assignments[complete] <- colnames(posterior)[
      max.col(posterior[complete, , drop = FALSE], ties.method = "first")
    ]
  }
  model_frame <- d
  for (k in seq_len(ncol(posterior) - 1L)) model_frame[[paste0("p_strategy_", k)]] <- posterior[, k]
  strategy_terms <- paste0("p_strategy_", seq_len(max(0L, ncol(posterior) - 1L)))
  rhs <- c(strategy_terms, participant, item)
  formula <- stats::as.formula(paste(response, "~", paste(rhs, collapse = " + ")))
  fit <- stats::glm(formula, family = stats::binomial(), data = model_frame)
  out <- list(spec = spec, posterior = posterior, assignment = assignments, response_model = fit, data = model_frame,
              warning = "Prototype-defined classes require recovery studies and external construct validation; posterior assignment is not proof of cognitive strategy.")
  class(out) <- "eye_theory_strategy_irt"
  out
}

#' @export
print.eye_theory_strategy_irt <- function(x, ...) {
  cat("Theory-defined strategy-informed IRT model\n")
  print(table(x$assignment, useNA = "ifany"))
  invisible(x)
}

#' @export
plot.eye_theory_strategy_irt <- function(x, ...) {
  graphics::matplot(x$posterior, type = "h", lty = 1, xlab = "Trial row", ylab = "Posterior probability", main = "Strategy posterior probabilities", ...)
  graphics::legend("topright", legend = colnames(x$posterior), lty = seq_len(ncol(x$posterior)), bty = "n")
  invisible(x$posterior)
}

#' Specify a gaze-informed diffusion workflow
#' @param engine Approximate EZ regression, `diffIRT`, or Bayesian Wiener model.
#' @param gaze_features Process predictors.
#' @param response Accuracy field.
#' @param response_time Response-time field.
#' @return An `eye_gaze_diffusion_spec`.
#' @export
gaze_diffusion_spec <- function(engine = c("ez_regression", "diffIRT", "brms"), gaze_features = character(), response = "score", response_time = "response_time") {
  structure(list(engine = match.arg(engine), gaze_features = as.character(gaze_features), response = response, response_time = response_time), class = "eye_gaze_diffusion_spec")
}

.ep_diffusion_model_data <- function(x, spec) {
  d <- x$responses
  if (!nrow(d)) .eye_stop("No responses are available for diffusion modelling.")

  if (length(spec$gaze_features) && nrow(x$features)) {
    id_cols <- c("recording_id", "participant_id", "trial_id", "item_id")
    fw <- features_wide(x, id_cols = id_cols)
    keys <- intersect(id_cols, intersect(names(d), names(fw)))
    available_features <- intersect(spec$gaze_features, names(fw))
    if (length(available_features)) {
      fw <- fw[unique(c(keys, available_features))]
      d <- merge(d, fw, by = keys, all.x = TRUE, sort = FALSE)
    }
  }

  d$participant_id <- factor(d$participant_id)
  d$item_id <- factor(d$item_id)
  d
}

#' Fit a gaze-informed diffusion/accumulation workflow
#'
#' @param x An `eye_dataset`.
#' @param spec Diffusion specification.
#' @param ... Passed to the selected engine.
#' @return An `eye_gaze_diffusion_irt` object.
#' @export
fit_gaze_diffusion_irt <- function(x, spec = gaze_diffusion_spec(), ...) {
  .assert_eye_dataset(x)
  if (!inherits(spec, "eye_gaze_diffusion_spec")) .eye_stop("`spec` must be created by `gaze_diffusion_spec()`.")
  if (spec$engine == "diffIRT") {
    baseline <- fit_diffirt_adapter(x, ...)
    out <- list(spec = spec, baseline = baseline, gaze_model = NULL,
                warning = "The diffIRT baseline does not make gaze a momentary accumulation input; it is retained as a benchmark.")
  } else if (spec$engine == "brms") {
    .require_namespace("brms", "for Wiener diffusion models")
    d <- .ep_diffusion_model_data(x, spec)
    .assert_columns(d, c(spec$response, spec$response_time, spec$gaze_features, "participant_id", "item_id"))
    rhs <- paste(c(spec$gaze_features, "(1 | participant_id)", "(1 | item_id)"), collapse = " + ")
    f <- stats::as.formula(paste0(spec$response_time, " | dec(", spec$response, ") ~ ", rhs))
    fit <- brms::brm(f, data = d, family = brms::wiener(), ...)
    out <- list(spec = spec, baseline = NULL, gaze_model = fit,
                warning = "Wiener parameter interpretation depends on coding, priors, identification, and the temporal definition of gaze predictors.")
  } else {
    d <- .ep_diffusion_model_data(x, spec)
    .assert_columns(d, c(spec$response, spec$response_time, "item_id", spec$gaze_features))
    ez <- estimate_ez_diffusion(d, accuracy = spec$response, response_time = spec$response_time, by = "item_id")
    item_gaze <- if (length(spec$gaze_features)) {
      stats::aggregate(d[spec$gaze_features], list(item_id = d$item_id), mean, na.rm = TRUE)
    } else data.frame(item_id = unique(d$item_id))
    z <- merge(ez, item_gaze, by = "item_id", all.x = TRUE)
    fits <- list()
    if (length(spec$gaze_features) && nrow(z) > length(spec$gaze_features) + 2L) {
      rhs <- paste(spec$gaze_features, collapse = " + ")
      for (parameter in c("drift_rate", "boundary_separation", "nondecision_time")) {
        fits[[parameter]] <- stats::lm(stats::as.formula(paste(parameter, "~", rhs)), data = z)
      }
    }
    out <- list(spec = spec, baseline = ez, gaze_model = fits, data = z,
                warning = "EZ regression is an item-level approximation and does not estimate a fixation-dependent accumulation process.")
  }
  class(out) <- "eye_gaze_diffusion_irt"
  out
}

#' @export
print.eye_gaze_diffusion_irt <- function(x, ...) {
  cat("Gaze-informed diffusion workflow\n")
  cat("Engine: ", x$spec$engine, "\n", sep = "")
  cat(x$warning, "\n")
  invisible(x)
}

#' @export
plot.eye_gaze_diffusion_irt <- function(x, parameter = "drift_rate", ...) {
  if (x$spec$engine != "ez_regression") .eye_stop("The default plot is available for the EZ-regression engine.")
  d <- x$data
  if (!parameter %in% names(d)) .eye_stop("Unknown diffusion parameter: ", parameter)
  graphics::plot(seq_len(nrow(d)), d[[parameter]], type = "b", xlab = "Item", ylab = parameter, main = "Diffusion parameter by item", ...)
  graphics::axis(1, at = seq_len(nrow(d)), labels = d$item_id, las = 2)
  invisible(d)
}

#' Construct the advanced-model validation grid
#'
#' By default this returns a one-factor-at-a-time screening design around a
#' declared reference scenario. This preserves every factor and level from the
#' research programme without accidentally launching hundreds of thousands of
#' Monte Carlo scenarios. Set `full_factorial = TRUE` only when the computing
#' plan explicitly supports the complete Cartesian design.
#'
#' @param quick Whether to return a compact smoke-test design.
#' @param full_factorial Whether to return the complete Cartesian design.
#' @return A scenario data frame for `run_model_validation()` or custom
#'   Monte Carlo programmes.
#' @export
advanced_validation_grid <- function(quick = FALSE, full_factorial = FALSE) {
  levels <- if (isTRUE(quick)) {
    list(
      n_person = c(80L, 150L), n_item = c(10L, 20L),
      ability_speed_correlation = c(-0.3, 0.3),
      gaze_effect = c(0, 0.35), feature_reliability = c(0.5, 0.8),
      missing_process = c(0, 0.2), state_misclassification = c(0, 0.10),
      pupil_ar1 = c(0.3, 0.6), luminance_effect = c(0, 0.3),
      dif_effect = c(0, 0.4), local_dependence = c(0, 0.3)
    )
  } else {
    list(
      n_person = c(200L, 500L, 1000L, 2000L),
      n_item = c(10L, 20L, 40L, 80L),
      ability_speed_correlation = c(-0.5, 0, 0.5),
      gaze_effect = c(0, 0.15, 0.35, 0.60),
      feature_reliability = c(0.4, 0.7, 0.9),
      missing_process = c(0, 0.10, 0.30, 0.50),
      state_misclassification = c(0, 0.05, 0.15),
      pupil_ar1 = c(0.2, 0.6, 0.9),
      luminance_effect = c(0, 0.30, 0.60),
      dif_effect = c(0, 0.30, 0.60),
      local_dependence = c(0, 0.30, 0.60)
    )
  }
  if (isTRUE(full_factorial)) {
    return(do.call(
      expand.grid,
      c(levels, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
    ))
  }
  reference_index <- c(
    n_person = min(2L, length(levels$n_person)),
    n_item = min(2L, length(levels$n_item)),
    ability_speed_correlation = min(2L, length(levels$ability_speed_correlation)),
    gaze_effect = min(2L, length(levels$gaze_effect)),
    feature_reliability = min(2L, length(levels$feature_reliability)),
    missing_process = 1L,
    state_misclassification = 1L,
    pupil_ar1 = min(2L, length(levels$pupil_ar1)),
    luminance_effect = 1L,
    dif_effect = 1L,
    local_dependence = 1L
  )
  reference <- as.data.frame(
    lapply(names(levels), function(nm) levels[[nm]][reference_index[[nm]]]),
    stringsAsFactors = FALSE
  )
  names(reference) <- names(levels)
  rows <- list(reference)
  for (nm in names(levels)) {
    for (value in levels[[nm]]) {
      candidate <- reference
      candidate[[nm]] <- value
      rows[[length(rows) + 1L]] <- candidate
    }
  }
  out <- unique(do.call(rbind, rows))
  rownames(out) <- NULL
  out
}

#' Simulate advanced response-process data
#'
#' Generates accuracy, response time, process features, theory-defined strategy,
#' dynamic AOI states, pupil trajectories, measurement error, missing process
#' data, DIF, and local dependence for validation studies.
#'
#' @param n_person Number of persons.
#' @param n_item Number of items.
#' @param n_time Pupil time bins.
#' @param n_states Number of AOI states.
#' @param ability_speed_correlation Correlation between ability and speed.
#' @param gaze_effect Process-feature coefficient in the response model.
#' @param feature_reliability Reliability of observed gaze features.
#' @param missing_process Fraction of process observations set missing.
#' @param state_misclassification Probability of AOI-state misclassification.
#' @param pupil_ar1 AR(1) coefficient for pupil noise.
#' @param luminance_effect Effect of simulated luminance on pupil size.
#' @param dif_effect Logit-scale DIF effect for the focal group on flagged items.
#' @param local_dependence Shared testlet-effect standard deviation.
#' @param seed Random seed.
#' @return A list with trial data, state data, pupil data, and truth.
#' @export
simulate_advanced_process_data <- function(
    n_person = 100L,
    n_item = 20L,
    n_time = 30L,
    n_states = 3L,
    ability_speed_correlation = -0.30,
    gaze_effect = 0.35,
    feature_reliability = 0.70,
    missing_process = 0,
    state_misclassification = 0,
    pupil_ar1 = 0.60,
    luminance_effect = 0,
    dif_effect = 0,
    local_dependence = 0,
    seed = 1L) {
  n_person <- as.integer(n_person); n_item <- as.integer(n_item)
  n_time <- as.integer(n_time); n_states <- as.integer(n_states)
  if (any(is.na(c(n_person, n_item, n_time, n_states))) ||
      n_person < 2L || n_item < 2L || n_time < 4L ||
      n_states < 2L || n_states > length(LETTERS)) {
    .eye_stop("Simulation sizes must satisfy n_person >= 2, n_item >= 2, n_time >= 4, and 2 <= n_states <= 26.")
  }
  bounded <- c(
    ability_speed_correlation = ability_speed_correlation,
    feature_reliability = feature_reliability,
    missing_process = missing_process,
    state_misclassification = state_misclassification,
    pupil_ar1 = pupil_ar1
  )
  if (any(!is.finite(bounded)) || abs(ability_speed_correlation) >= 1 ||
      feature_reliability <= 0 || feature_reliability > 1 ||
      missing_process < 0 || missing_process >= 1 ||
      state_misclassification < 0 || state_misclassification >= 1 ||
      abs(pupil_ar1) >= 1) {
    .eye_stop("Correlation, reliability, missingness, state-error, and AR(1) settings are outside their valid ranges.")
  }
  effects <- c(gaze_effect, luminance_effect, dif_effect, local_dependence)
  if (any(!is.finite(effects)) || local_dependence < 0) {
    .eye_stop("Simulation effects must be finite and `local_dependence` must be non-negative.")
  }

  set.seed(seed)
  persons <- paste0("P", seq_len(n_person))
  items <- paste0("I", seq_len(n_item))
  states <- LETTERS[seq_len(n_states)]
  theta <- stats::rnorm(n_person)
  speed <- ability_speed_correlation * theta +
    sqrt(1 - ability_speed_correlation^2) * stats::rnorm(n_person)
  difficulty <- stats::rnorm(n_item)
  discrimination <- exp(stats::rnorm(n_item, 0, 0.15))
  person_group <- rep(c("reference", "focal"), length.out = n_person)
  dif_item <- rep(c(FALSE, TRUE), length.out = n_item)
  testlet <- rep(seq_len(ceiling(n_item / 4)), each = 4L, length.out = n_item)

  grid <- expand.grid(participant_id = persons, item_id = items, stringsAsFactors = FALSE)
  person_index <- match(grid$participant_id, persons)
  item_index <- match(grid$item_id, items)
  strategy <- stats::rbinom(
    nrow(grid), 1, stats::plogis(0.5 * theta[person_index])
  ) + 1L

  latent_gaze_1 <- stats::rnorm(nrow(grid), ifelse(strategy == 1, -0.7, 0.7), 0.6)
  latent_gaze_2 <- stats::rnorm(nrow(grid), ifelse(strategy == 1, 0.7, -0.7), 0.6)
  attenuation <- sqrt(feature_reliability)
  measurement_noise <- sqrt(1 - feature_reliability)
  gaze_1 <- attenuation * as.numeric(scale(latent_gaze_1)) + measurement_noise * stats::rnorm(nrow(grid))
  gaze_2 <- attenuation * as.numeric(scale(latent_gaze_2)) + measurement_noise * stats::rnorm(nrow(grid))
  testlet_effects <- matrix(
    stats::rnorm(n_person * max(testlet), 0, local_dependence),
    nrow = n_person, ncol = max(testlet)
  )
  dif_term <- dif_effect * (person_group[person_index] == "focal") * dif_item[item_index]
  local_term <- testlet_effects[cbind(person_index, testlet[item_index])]
  eta <- discrimination[item_index] * (theta[person_index] - difficulty[item_index]) +
    gaze_effect * gaze_1 + dif_term + local_term
  grid$score <- stats::rbinom(nrow(grid), 1, stats::plogis(eta))
  grid$response_time <- exp(
    1 + 0.25 * difficulty[item_index] - 0.25 * speed[person_index] +
      0.12 * gaze_2 + stats::rnorm(nrow(grid), 0, 0.20)
  )
  grid$gaze_1 <- gaze_1
  grid$gaze_2 <- gaze_2
  grid$strategy <- strategy
  grid$group <- person_group[person_index]
  grid$dif_item <- dif_item[item_index]
  grid$testlet <- testlet[item_index]
  grid$luminance <- stats::runif(nrow(grid), -1, 1)
  if (missing_process > 0) {
    missing <- stats::runif(nrow(grid)) < missing_process
    grid$gaze_1[missing] <- NA_real_
    grid$gaze_2[missing] <- NA_real_
  }

  state_rows <- lapply(seq_len(nrow(grid)), function(i) {
    length_i <- sample(5:12, 1L)
    state <- sample(states, 1L)
    sequence_observed <- sequence_true <- character(length_i)
    for (time_index in seq_len(length_i)) {
      probabilities <- rep(0.15, n_states)
      probabilities[match(state, states)] <- 0.50
      target <- if (grid$strategy[i] == 1L) 1L else n_states
      probabilities[target] <- probabilities[target] + 0.25
      probabilities <- probabilities / sum(probabilities)
      state <- sample(states, 1L, prob = probabilities)
      sequence_true[time_index] <- state
      sequence_observed[time_index] <- if (stats::runif(1) < state_misclassification) {
        sample(setdiff(states, state), 1L)
      } else state
    }
    data.frame(
      participant_id = grid$participant_id[i], item_id = grid$item_id[i],
      step = seq_along(sequence_true), true_state = sequence_true,
      state = sequence_observed, stringsAsFactors = FALSE
    )
  })

  pupil_rows <- lapply(seq_len(nrow(grid)), function(i) {
    time <- seq(0, 1, length.out = n_time)
    innovation <- stats::rnorm(n_time, 0, 0.08)
    noise <- numeric(n_time)
    noise[1L] <- innovation[1L] / sqrt(1 - pupil_ar1^2)
    if (n_time > 1L) {
      for (time_index in 2:n_time) noise[time_index] <- pupil_ar1 * noise[time_index - 1L] + innovation[time_index]
    }
    task_response <- 0.30 * sin(base::pi * time) +
      0.20 * grid$score[i] * exp(-((time - 0.60) / 0.18)^2)
    signal <- task_response + luminance_effect * grid$luminance[i] + noise
    data.frame(
      participant_id = grid$participant_id[i], item_id = grid$item_id[i],
      time = time, pupil = signal, luminance = grid$luminance[i],
      stringsAsFactors = FALSE
    )
  })

  list(
    trials = grid,
    states = do.call(rbind, state_rows),
    pupil = do.call(rbind, pupil_rows),
    truth = list(
      theta = setNames(theta, persons), speed = setNames(speed, persons),
      difficulty = setNames(difficulty, items),
      discrimination = setNames(discrimination, items),
      ability_speed_correlation = ability_speed_correlation,
      gaze_effect = gaze_effect,
      feature_reliability = feature_reliability,
      missing_process = missing_process,
      state_misclassification = state_misclassification,
      pupil_ar1 = pupil_ar1,
      luminance_effect = luminance_effect,
      dif_effect = dif_effect,
      local_dependence = local_dependence
    )
  )
}
