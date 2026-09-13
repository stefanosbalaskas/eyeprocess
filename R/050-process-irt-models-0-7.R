# eyeprocess 0.7.0.9000 -------------------------------------------------------
# Process-aware IRT reference models.
#
# IMPORTANT: Several functions below are auditable reference estimators. They
# are not claimed to reproduce every estimation detail of the cited models.
# Exact/fully Bayesian engines remain available through `engine = "brms"` or
# explicit external callbacks and must pass the package validation contract.

.ep07_model_frame <- function(data, cols) {
  data <- .ep07_as_data_frame(data, "data")
  .ep07_req_cols(data, cols, "data")
  data
}

.ep07_ranef_vector <- function(fit, group) {
  if (!requireNamespace("lme4", quietly = TRUE)) return(NULL)
  re <- try(lme4::ranef(fit)[[group]], silent = TRUE)
  if (inherits(re, "try-error") || is.null(re) || !ncol(re)) return(NULL)
  stats::setNames(re[[1L]], rownames(re))
}

.ep07_fixed_or_factor_glm <- function(formula, data, family) {
  stats::glm(formula, data = data, family = family)
}

.ep07_random_formula <- function(outcome, person, item, extras = character(), transform = NULL) {
  lhs <- if (is.null(transform)) outcome else sprintf("%s(%s)", transform, outcome)
  rhs <- c(sprintf("(1 | %s)", person), sprintf("(1 | %s)", item),
           sprintf("(1 | %s)", extras))
  stats::as.formula(sprintf("%s ~ 1 + %s", lhs, paste(rhs, collapse = " + ")))
}

#' Joint response, response-time, and gaze-process IRT
#'
#' The reference engine fits crossed person/item submodels for accuracy,
#' log-response-time, and a gaze process, then returns person- and item-side
#' latent-score covariance summaries. The `brms` engine uses multivariate
#' formulas with shared group-level IDs so person/item random effects can be
#' correlated across channels.
#'
#' @param data Long person-by-item data.
#' @param response Binary response variable.
#' @param rt Positive response-time variable.
#' @param gaze Gaze process variable, usually fixation count or dwell count.
#' @param person,item Person and item identifiers.
#' @param gaze_family Poisson or negative-binomial reference channel.
#' @param engine `reference` or `brms`.
#' @param iter,chains,cores Passed to brms.
#' @param seed Random seed.
#' @param ... Additional arguments to the selected engine.
#' @return An `eye_joint_gaze_rt_irt` object.
#' @export
fit_joint_gaze_rt_irt <- function(data,
                                  response = "response", rt = "rt",
                                  gaze = "fixation_count",
                                  person = "participant_id", item = "item_id",
                                  gaze_family = c("negative_binomial", "poisson"),
                                  engine = c("reference", "brms"),
                                  iter = 2000, chains = 4, cores = 1,
                                  seed = 1, ...) {
  gaze_family <- match.arg(gaze_family)
  engine <- match.arg(engine)
  data <- .ep07_model_frame(data, c(response, rt, gaze, person, item))
  d <- data[stats::complete.cases(data[c(response, rt, gaze, person, item)]), , drop = FALSE]
  d[[response]] <- as.numeric(d[[response]])
  if (!all(d[[response]] %in% c(0, 1))) stop("`response` must be coded 0/1 for fit_joint_gaze_rt_irt().", call. = FALSE)
  if (any(d[[rt]] <= 0, na.rm = TRUE)) stop("Response times must be strictly positive.", call. = FALSE)
  if (any(d[[gaze]] < 0, na.rm = TRUE)) stop("Gaze counts must be non-negative.", call. = FALSE)
  d[[person]] <- factor(d[[person]])
  d[[item]] <- factor(d[[item]])

  if (engine == "brms") {
    if (!requireNamespace("brms", quietly = TRUE)) stop("Install optional package `brms` for engine='brms'.", call. = FALSE)
    fr <- stats::as.formula(sprintf("%s ~ 1 + (1|p|%s) + (1|i|%s)", response, person, item))
    ft <- stats::as.formula(sprintf("log(%s) ~ 1 + (1|p|%s) + (1|i|%s)", rt, person, item))
    fg <- stats::as.formula(sprintf("%s ~ 1 + (1|p|%s) + (1|i|%s)", gaze, person, item))
    gf <- if (gaze_family == "negative_binomial") brms::negbinomial() else stats::poisson()
    model <- brms::brm(
      brms::bf(fr, family = brms::bernoulli()) +
        brms::bf(ft, family = stats::gaussian()) +
        brms::bf(fg, family = gf) + brms::set_rescor(FALSE),
      data = d, iter = iter, chains = chains, cores = cores, seed = seed, ...
    )
    return(structure(list(
      engine = engine, model = model, data_n = nrow(d), columns = list(response = response, rt = rt, gaze = gaze,
                                                                          person = person, item = item),
      gaze_family = gaze_family,
      interpretation = "Joint response/RT/gaze multivariate hierarchical model; psychological labels require external construct validation."
    ), class = "eye_joint_gaze_rt_irt"))
  }

  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("The reference engine requires optional package `lme4`.", call. = FALSE)
  }
  f_resp <- .ep07_random_formula(response, person, item)
  f_rt <- .ep07_random_formula(rt, person, item, transform = "log")
  f_gaze <- .ep07_random_formula(gaze, person, item)
  response_fit <- lme4::glmer(f_resp, data = d, family = stats::binomial())
  rt_fit <- lme4::lmer(f_rt, data = d)
  gaze_fit <- if (gaze_family == "negative_binomial") {
    lme4::glmer.nb(f_gaze, data = d)
  } else {
    lme4::glmer(f_gaze, data = d, family = stats::poisson())
  }

  person_scores <- Reduce(function(x, y) merge(x, y, by = "id", all = TRUE), list(
    data.frame(id = names(.ep07_ranef_vector(response_fit, person)), ability = .ep07_ranef_vector(response_fit, person)),
    data.frame(id = names(.ep07_ranef_vector(rt_fit, person)), speed_logtime = .ep07_ranef_vector(rt_fit, person)),
    data.frame(id = names(.ep07_ranef_vector(gaze_fit, person)), gaze_propensity = .ep07_ranef_vector(gaze_fit, person))
  ))
  item_scores <- Reduce(function(x, y) merge(x, y, by = "id", all = TRUE), list(
    data.frame(id = names(.ep07_ranef_vector(response_fit, item)), response_intercept = .ep07_ranef_vector(response_fit, item)),
    data.frame(id = names(.ep07_ranef_vector(rt_fit, item)), time_intensity = .ep07_ranef_vector(rt_fit, item)),
    data.frame(id = names(.ep07_ranef_vector(gaze_fit, item)), gaze_intensity = .ep07_ranef_vector(gaze_fit, item))
  ))
  person_cov <- stats::cov(person_scores[-1L], use = "pairwise.complete.obs")
  item_cov <- stats::cov(item_scores[-1L], use = "pairwise.complete.obs")
  structure(list(
    engine = engine, response_model = response_fit, rt_model = rt_fit,
    gaze_model = gaze_fit, person_scores = person_scores, item_scores = item_scores,
    person_covariance = person_cov, item_covariance = item_cov,
    data_n = nrow(d), gaze_family = gaze_family,
    columns = list(response = response, rt = rt, gaze = gaze, person = person, item = item),
    status = "reference-estimator",
    interpretation = paste(
      "Crossed hierarchical reference implementation inspired by the three-way joint measurement model.",
      "Its empirical random-effect score covariance is not identical to a fully joint latent covariance estimator; use engine='brms' or a validated external engine for confirmatory inference."
    )
  ), class = "eye_joint_gaze_rt_irt")
}

#' Speed-accuracy-engagement IRT convenience wrapper
#' @param data Input data frame or compatible tabular object.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @param engine Estimation engine.
#' @return An object of class "eye_joint_gaze_rt_irt", stored as a named list, with components "engine", "response_model", "rt_model", "gaze_model", "person_scores", "item_scores", "person_covariance", "item_covariance", "data_n", "gaze_family", "columns", "status", and additional components. It contains speed-accuracy-engagement IRT convenience wrapper and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_speed_accuracy_engagement_irt <- function(data, ..., engine = c("reference", "brms")) {
  engine <- match.arg(engine)
  fit <- fit_joint_gaze_rt_irt(data = data, engine = engine, ...)
  class(fit) <- c("eye_speed_accuracy_engagement_irt", class(fit))
  fit
}

#' Joint graded-response, RT, and process reference model
#'
#' Extends the 2026 graded-response/RT direction with an optional gaze/process
#' channel. The bundled reference engine uses proportional-odds plus crossed RT
#' and process submodels; it is explicitly experimental rather than a claim to
#' reproduce the published SAEM estimator.
#' @param data Input data frame or compatible tabular object.
#' @param response Response variable or response-column name.
#' @param rt Response-time variable or column name.
#' @param process Process variable or column name.
#' @param person Person or participant identifier column.
#' @param item Item identifier, name, or item column.
#' @param engine Estimation engine.
#' @param process_family Distributional family for the process channel.
#' @param iter Number of estimation iterations.
#' @param chains Number of Bayesian chains.
#' @param cores Number of processor cores.
#' @param seed Random-number seed.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return An object of class "eye_joint_graded_rt_process_irt", stored as a named list, with components "engine", "response_model", "rt_model", "process_model", "data_n", "status", "note". It contains joint graded-response, RT, and process reference model and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_joint_graded_rt_process_irt <- function(data,
                                            response = "response", rt = "rt",
                                            process = "fixation_count",
                                            person = "participant_id", item = "item_id",
                                            engine = c("reference", "brms"),
                                            process_family = c("negative_binomial", "poisson", "gaussian"),
                                            iter = 2000, chains = 4, cores = 1,
                                            seed = 1, ...) {
  engine <- match.arg(engine)
  process_family <- match.arg(process_family)
  data <- .ep07_model_frame(data, c(response, rt, process, person, item))
  d <- data[stats::complete.cases(data[c(response, rt, process, person, item)]), , drop = FALSE]
  if (any(d[[rt]] <= 0)) stop("RT must be positive.", call. = FALSE)
  d[[response]] <- ordered(d[[response]])
  d[[person]] <- factor(d[[person]]); d[[item]] <- factor(d[[item]])

  if (engine == "brms") {
    if (!requireNamespace("brms", quietly = TRUE)) stop("Install optional package `brms`.", call. = FALSE)
    fr <- stats::as.formula(sprintf("%s ~ 1 + (1|p|%s) + (1|i|%s)", response, person, item))
    ft <- stats::as.formula(sprintf("log(%s) ~ 1 + %s + (1|p|%s) + (1|i|%s)", rt, response, person, item))
    fp <- stats::as.formula(sprintf("%s ~ 1 + (1|p|%s) + (1|i|%s)", process, person, item))
    pf <- switch(process_family,
                 negative_binomial = brms::negbinomial(),
                 poisson = stats::poisson(),
                 gaussian = stats::gaussian())
    model <- brms::brm(
      brms::bf(fr, family = brms::cumulative("logit")) +
        brms::bf(ft, family = stats::gaussian()) +
        brms::bf(fp, family = pf) + brms::set_rescor(FALSE),
      data = d, iter = iter, chains = chains, cores = cores, seed = seed, ...
    )
    return(structure(list(engine = engine, model = model, data_n = nrow(d),
                          status = "experimental"), class = "eye_joint_graded_rt_process_irt"))
  }

  if (!requireNamespace("MASS", quietly = TRUE) || !requireNamespace("lme4", quietly = TRUE)) {
    stop("The reference engine requires optional packages `MASS` and `lme4`.", call. = FALSE)
  }
  response_formula <- stats::as.formula(sprintf("%s ~ factor(%s)", response, item))
  response_fit <- MASS::polr(response_formula, data = d, Hess = TRUE, method = "logistic")
  rt_formula <- stats::as.formula(sprintf("log(%s) ~ %s + (1|%s) + (1|%s)", rt, response, person, item))
  rt_fit <- lme4::lmer(rt_formula, data = d)
  process_formula <- .ep07_random_formula(process, person, item)
  process_fit <- switch(process_family,
                        negative_binomial = lme4::glmer.nb(process_formula, data = d),
                        poisson = lme4::glmer(process_formula, data = d, family = stats::poisson()),
                        gaussian = lme4::lmer(process_formula, data = d))
  structure(list(engine = engine, response_model = response_fit, rt_model = rt_fit,
                 process_model = process_fit, data_n = nrow(d), status = "experimental-reference",
                 note = "Reference decomposition; not the published graded-response/RT SAEM estimator."),
            class = "eye_joint_graded_rt_process_irt")
}

.ep07_person_accuracy_proxy <- function(d, response, person, correct = NULL) {
  y <- if (is.null(correct)) as.numeric(d[[response]]) else as.numeric(as.character(d[[response]]) == as.character(correct))
  p <- tapply(y, d[[person]], mean, na.rm = TRUE)
  n <- tapply(!is.na(y), d[[person]], sum)
  adj <- (p * n + 0.5) / (n + 1)
  stats::qlogis(pmin(pmax(adj, 1e-6), 1 - 1e-6))
}

#' Nominal/distractor IRT with option-level gaze
#'
#' The bundled estimator is a transparent two-stage process-augmented nominal
#' model: participant ability may be supplied, or a shrinkage logit accuracy
#' proxy is estimated; option-level gaze proportions then enter a multinomial
#' response model. This is intended for validation and exploratory distractor
#' research, not as a replacement for a fully latent nominal-response model.
#'
#' @param option_gaze Character vector naming one gaze column per response
#'   option. Names should correspond to option labels when possible.
#' @param ability Optional existing ability score column.
#' @param correct_option Optional scalar or column name identifying correct option.
#' @param data Input data frame or compatible tabular object.
#' @param response_option Column identifying the selected response option.
#' @param person Person or participant identifier column.
#' @param item Item identifier, name, or item column.
#' @param add_item_effects Whether item effects are included.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return An object of class "eye_nominal_gaze_irt", stored as a named list, with components "model", "baseline_model", "data", "option_gaze", "gaze_proportion_columns", "ability", "person", "item", "logLik_gain", "status", "note". It contains nominal/distractor IRT with option-level gaze and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_nominal_gaze_irt <- function(data,
                                 response_option = "response_option",
                                 option_gaze,
                                 person = "participant_id", item = "item_id",
                                 ability = NULL, correct_option = NULL,
                                 add_item_effects = TRUE, ...) {
  if (!requireNamespace("nnet", quietly = TRUE)) stop("Install optional package `nnet`.", call. = FALSE)
  cols <- c(response_option, option_gaze, person, item, ability)
  cols <- cols[!is.na(cols) & nzchar(cols)]
  d <- .ep07_model_frame(data, cols)
  if (length(option_gaze) < 2L) stop("Supply at least two option-level gaze columns.", call. = FALSE)
  d[[response_option]] <- factor(d[[response_option]])
  d[[person]] <- factor(d[[person]]); d[[item]] <- factor(d[[item]])
  gaze <- as.matrix(d[option_gaze])
  gaze[!is.finite(gaze)] <- 0
  rs <- rowSums(gaze)
  prop <- gaze / pmax(rs, .Machine$double.eps)
  colnames(prop) <- paste0("gaze_prop_", make.names(option_gaze, unique = TRUE))
  d <- cbind(d, as.data.frame(prop, check.names = FALSE))

  if (is.null(ability)) {
    if (is.null(correct_option)) {
      # Use modal response per item as a deliberately neutral scoring proxy.
      modal <- tapply(as.character(d[[response_option]]), d[[item]], function(z) names(sort(table(z), decreasing = TRUE))[1L])
      correct_vec <- unname(modal[as.character(d[[item]])])
    } else if (length(correct_option) == 1L && correct_option %in% names(d)) {
      correct_vec <- d[[correct_option]]
    } else if (length(correct_option) == 1L) {
      correct_vec <- rep(correct_option, nrow(d))
    } else {
      correct_vec <- correct_option
    }
    correct <- as.numeric(as.character(d[[response_option]]) == as.character(correct_vec))
    p <- tapply(correct, d[[person]], mean, na.rm = TRUE)
    n <- tapply(!is.na(correct), d[[person]], sum)
    theta <- stats::qlogis(pmin(pmax((p * n + 0.5) / (n + 1), 1e-6), 1 - 1e-6))
    d$.ability_proxy <- unname(theta[as.character(d[[person]])])
    ability_term <- ".ability_proxy"
  } else {
    ability_term <- ability
  }
  rhs <- c(ability_term, colnames(prop))
  if (isTRUE(add_item_effects)) rhs <- c(rhs, sprintf("factor(%s)", item))
  f <- stats::as.formula(sprintf("%s ~ %s", response_option, paste(rhs, collapse = " + ")))
  fit <- nnet::multinom(f, data = d, trace = FALSE, ...)
  baseline_formula <- stats::as.formula(sprintf("%s ~ %s%s", response_option, ability_term,
                                                 if (add_item_effects) paste0(" + factor(", item, ")") else ""))
  baseline <- nnet::multinom(baseline_formula, data = d, trace = FALSE)
  ll_full <- as.numeric(stats::logLik(fit)); ll_base <- as.numeric(stats::logLik(baseline))
  structure(list(
    model = fit, baseline_model = baseline, data = d,
    option_gaze = option_gaze, gaze_proportion_columns = colnames(prop),
    ability = ability_term, person = person, item = item,
    logLik_gain = ll_full - ll_base,
    status = "experimental-two-stage",
    note = "Option gaze is process evidence; the fitted model does not prove a cognitive meaning for gaze allocation."
  ), class = "eye_nominal_gaze_irt")
}

#' Quantify option-process information from a nominal gaze model
#' @param object A fitted eyeprocess model or audit object.
#' @return An object of class "eye_option_process_information", "data.frame", stored as a data frame, containing quantify option-process information from a nominal gaze model and associated metadata needed to interpret the result.
#' @export
option_process_information <- function(object) {
  if (!inherits(object, "eye_nominal_gaze_irt")) stop("`object` must be an eye_nominal_gaze_irt.", call. = FALSE)
  p_full <- stats::predict(object$model, type = "probs")
  p_base <- stats::predict(object$baseline_model, type = "probs")
  if (is.vector(p_full)) p_full <- cbind(1 - p_full, p_full)
  if (is.vector(p_base)) p_base <- cbind(1 - p_base, p_base)
  entropy <- function(p) {
    p <- pmax(pmin(as.matrix(p), 1), .Machine$double.eps)
    -rowSums(p * log(p))
  }
  gain <- entropy(p_base) - entropy(p_full)
  structure(data.frame(row = seq_along(gain), entropy_reduction = gain,
                       full_entropy = entropy(p_full), baseline_entropy = entropy(p_base)),
            class = c("eye_option_process_information", "data.frame"))
}

#' Build a distractor process map
#' @param object A fitted eyeprocess model or audit object.
#' @return A data frame containing a distractor process map. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
distractor_process_map <- function(object) {
  if (!inherits(object, "eye_nominal_gaze_irt")) stop("`object` must be an eye_nominal_gaze_irt.", call. = FALSE)
  cf <- stats::coef(object$model)
  if (is.vector(cf)) cf <- matrix(cf, nrow = 1L, dimnames = list(levels(object$data[[deparse(object$model$terms[[2]])]])[-1L], names(cf)))
  gaze_cols <- grep("^gaze_prop_", colnames(cf), value = TRUE)
  if (!length(gaze_cols)) return(data.frame())
  out <- as.data.frame(as.table(cf[, gaze_cols, drop = FALSE]), stringsAsFactors = FALSE)
  names(out) <- c("response_category", "gaze_channel", "coefficient")
  out
}

#' Audit distractor attention patterns
#' @param data Input data frame or compatible tabular object.
#' @param response_option Column identifying the selected response option.
#' @param option_gaze Option-level gaze variables.
#' @param chosen_suffix Suffix identifying the selected option indicator.
#' @return A data frame containing distractor attention patterns. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
audit_distractor_attention <- function(data, response_option = "response_option",
                                       option_gaze, chosen_suffix = NULL) {
  d <- .ep07_model_frame(data, c(response_option, option_gaze))
  cats <- as.character(d[[response_option]])
  map_names <- names(option_gaze)
  if (is.null(map_names) || any(!nzchar(map_names))) map_names <- option_gaze
  chosen <- numeric(nrow(d)); unchosen <- numeric(nrow(d))
  for (i in seq_len(nrow(d))) {
    idx <- match(cats[i], map_names)
    z <- as.numeric(d[i, option_gaze, drop = TRUE])
    z[!is.finite(z)] <- 0
    chosen[i] <- if (is.na(idx)) NA_real_ else z[idx]
    unchosen[i] <- if (is.na(idx) || length(z) < 2L) NA_real_ else mean(z[-idx])
  }
  diff <- chosen - unchosen
  data.frame(n = sum(is.finite(diff)), mean_chosen = mean(chosen, na.rm = TRUE),
             mean_unchosen = mean(unchosen, na.rm = TRUE),
             mean_difference = mean(diff, na.rm = TRUE),
             median_difference = stats::median(diff, na.rm = TRUE),
             stringsAsFactors = FALSE)
}

#' Classify item missingness using exposure and response evidence
#'
#' @param data Input data frame or compatible tabular object.
#' @param response Response variable or response-column name.
#' @param reached Indicator that the item was reached.
#' @param inspected Indicator that the item or response area was inspected.
#' @param started Indicator that responding was initiated.
#' @return An object of class "factor", stored as an R object, containing classify item missingness using exposure and response evidence and associated metadata needed to interpret the result.
#' @export
classify_item_missingness <- function(data, response = "response", reached = "reached",
                                      inspected = NULL, started = NULL) {
  cols <- c(response, reached, inspected, started)
  cols <- cols[!is.null(cols) & !is.na(cols) & nzchar(cols)]
  d <- .ep07_model_frame(data, cols)
  y_missing <- is.na(d[[response]])
  reached_v <- as.logical(d[[reached]])
  inspected_v <- if (is.null(inspected)) rep(NA, nrow(d)) else as.logical(d[[inspected]])
  started_v <- if (is.null(started)) rep(NA, nrow(d)) else as.logical(d[[started]])
  state <- rep("answered", nrow(d))
  state[y_missing & !reached_v] <- "not_reached"
  state[y_missing & reached_v] <- "omitted_after_reach"
  state[y_missing & reached_v & !is.na(inspected_v) & !inspected_v] <- "reached_not_inspected"
  state[y_missing & reached_v & !is.na(inspected_v) & inspected_v] <- "inspected_omission"
  state[y_missing & reached_v & !is.na(started_v) & started_v] <- "started_unanswered"
  factor(state, levels = c("answered", "not_reached", "reached_not_inspected",
                           "inspected_omission", "started_unanswered", "omitted_after_reach"))
}

#' Estimate visual exposure probability
#' @param data Input data frame or compatible tabular object.
#' @param exposed Value supplied to `exposed`; see Details for its model-specific role.
#' @param predictors Predictor variables used by the model.
#' @param family Statistical family used by the channel or model.
#' @return An object of class "eye_visual_exposure_model", stored as a named list, with components "model", "fitted_probability", "exposed", "predictors". It contains visual exposure probability and associated metadata or diagnostics needed to interpret the result.
#' @export
estimate_visual_exposure_probability <- function(data, exposed = "reached",
                                                 predictors, family = stats::binomial()) {
  cols <- c(exposed, predictors)
  d <- .ep07_model_frame(data, cols)
  rhs <- if (length(predictors)) paste(predictors, collapse = " + ") else "1"
  f <- stats::as.formula(sprintf("%s ~ %s", exposed, rhs))
  fit <- stats::glm(f, data = d, family = family)
  structure(list(model = fit, fitted_probability = stats::fitted(fit), exposed = exposed,
                 predictors = predictors), class = "eye_visual_exposure_model")
}

#' Response/RT/omission survival IRT reference model
#'
#' Fits a response model among reached/answered observations and cause-specific
#' survival models for omission and not-reached processes. The function keeps
#' the missingness mechanisms distinct by construction.
#'
#' @param data Input data frame or compatible tabular object.
#' @param response Response variable or response-column name.
#' @param response_time Response-time variable or column name.
#' @param omission_time Time associated with an omitted response.
#' @param reached Indicator that the item was reached.
#' @param person Person or participant identifier column.
#' @param item Item identifier, name, or item column.
#' @param gaze_exposure Gaze-based exposure measure.
#' @param first_fixation_latency Latency to first fixation.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return An object of class "eye_omission_survival_irt", stored as a named list, with components "response_model", "omission_model", "not_reached_model", "classified_data", "state_counts", "status", "note". It contains response/RT/omission survival IRT reference model and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_omission_survival_irt <- function(data,
                                      response = "response",
                                      response_time = "response_time",
                                      omission_time = NULL,
                                      reached = "reached",
                                      person = "participant_id", item = "item_id",
                                      gaze_exposure = NULL,
                                      first_fixation_latency = NULL,
                                      ...) {
  if (!requireNamespace("survival", quietly = TRUE)) stop("Install optional package `survival`.", call. = FALSE)
  cols <- c(response, response_time, omission_time, reached, person, item,
            gaze_exposure, first_fixation_latency)
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  d <- .ep07_model_frame(data, cols)
  d$.missing_state <- classify_item_missingness(d, response = response, reached = reached,
                                                inspected = if (!is.null(gaze_exposure)) gaze_exposure else NULL)
  d$.time <- as.numeric(d[[response_time]])
  if (!is.null(omission_time)) {
    use_omit <- is.na(d[[response]]) & is.finite(d[[omission_time]])
    d$.time[use_omit] <- as.numeric(d[[omission_time]][use_omit])
  }
  finite_times <- d$.time[is.finite(d$.time) & d$.time > 0]
  fallback <- if (length(finite_times)) max(finite_times) else 1
  d$.time[!is.finite(d$.time) | d$.time <= 0] <- fallback
  d$.omission_event <- as.integer(d$.missing_state %in% c("omitted_after_reach", "inspected_omission", "started_unanswered"))
  d$.not_reached_event <- as.integer(d$.missing_state == "not_reached")

  covars <- c(sprintf("factor(%s)", item))
  if (!is.null(gaze_exposure)) covars <- c(covars, gaze_exposure)
  if (!is.null(first_fixation_latency)) covars <- c(covars, first_fixation_latency)
  sf_omit <- stats::as.formula(sprintf("survival::Surv(.time, .omission_event) ~ %s + cluster(%s)",
                                        paste(covars, collapse = " + "), person))
  sf_nr <- stats::as.formula(sprintf("survival::Surv(.time, .not_reached_event) ~ %s + cluster(%s)",
                                      paste(covars, collapse = " + "), person))
  omission_fit <- survival::coxph(sf_omit, data = d, ...)
  not_reached_fit <- survival::coxph(sf_nr, data = d, ...)

  answered <- d[!is.na(d[[response]]) & as.logical(d[[reached]]), , drop = FALSE]
  response_fit <- NULL
  if (nrow(answered) && all(answered[[response]] %in% c(0, 1))) {
    if (requireNamespace("lme4", quietly = TRUE)) {
      rf <- .ep07_random_formula(response, person, item)
      response_fit <- lme4::glmer(rf, data = answered, family = stats::binomial())
    } else {
      rf <- stats::as.formula(sprintf("%s ~ factor(%s) + factor(%s)", response, person, item))
      response_fit <- stats::glm(rf, data = answered, family = stats::binomial())
    }
  }
  structure(list(
    response_model = response_fit, omission_model = omission_fit,
    not_reached_model = not_reached_fit, classified_data = d,
    state_counts = table(d$.missing_state, useNA = "ifany"),
    status = "experimental-reference",
    note = "Cause-specific survival reference. A fully joint latent response/RT/omission model requires a validated dedicated engine."
  ), class = "eye_omission_survival_irt")
}

#' Many-facet process IRT reference model
#'
#' Fits crossed random effects for available person, item, device, session,
#' site, algorithm, and AOI-definition facets. For a binary response this is a
#' generalized many-facet reference model; it is not marketed as a FACETS
#' software replica.
#'
#' @param data Input data frame or compatible tabular object.
#' @param response Response variable or response-column name.
#' @param process Process variable or column name.
#' @param person Person or participant identifier column.
#' @param item Item identifier, name, or item column.
#' @param device Device identifier or device facet.
#' @param session Session identifier or session facet.
#' @param site Site identifier or site facet.
#' @param algorithm Algorithm identifier or algorithm facet.
#' @param aoi_definition Value supplied to `aoi_definition`; see Details for its model-specific role.
#' @param process_family Distributional family for the process channel.
#' @return An object of class "eye_manyfacet_process_irt", stored as a named list, with components "response_model", "process_model", "facets", "process_family", "status". It contains many-facet process IRT reference model and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_manyfacet_process_irt <- function(data,
                                      response = "response",
                                      process = NULL,
                                      person = "participant_id", item = "item_id",
                                      device = NULL, session = NULL, site = NULL,
                                      algorithm = NULL, aoi_definition = NULL,
                                      process_family = c("gaussian", "poisson", "negative_binomial")) {
  if (!requireNamespace("lme4", quietly = TRUE)) stop("Install optional package `lme4`.", call. = FALSE)
  process_family <- match.arg(process_family)
  facets <- c(person = person, item = item, device = device, session = session,
              site = site, algorithm = algorithm, aoi_definition = aoi_definition)
  facets <- facets[!is.na(facets) & nzchar(facets)]
  cols <- c(response, process, unname(facets))
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  d <- .ep07_model_frame(data, cols)
  for (nm in unname(facets)) d[[nm]] <- factor(d[[nm]])
  rhs <- paste(sprintf("(1|%s)", unname(facets)), collapse = " + ")
  rf <- stats::as.formula(sprintf("%s ~ 1 + %s", response, rhs))
  if (all(stats::na.omit(d[[response]]) %in% c(0, 1))) {
    response_fit <- lme4::glmer(rf, data = d, family = stats::binomial())
  } else {
    response_fit <- lme4::lmer(rf, data = d)
  }
  process_fit <- NULL
  if (!is.null(process)) {
    pf <- stats::as.formula(sprintf("%s ~ 1 + %s", process, rhs))
    process_fit <- switch(process_family,
                          gaussian = lme4::lmer(pf, data = d),
                          poisson = lme4::glmer(pf, data = d, family = stats::poisson()),
                          negative_binomial = lme4::glmer.nb(pf, data = d))
  }
  structure(list(response_model = response_fit, process_model = process_fit,
                 facets = facets, process_family = process_family,
                 status = "reference-estimator"), class = "eye_manyfacet_process_irt")
}

#' Extract facet effects from a many-facet process model
#' @param object A fitted eyeprocess model or audit object.
#' @param channel Measurement channel to inspect.
#' @return A named list with components "random_effects", "variance_components", containing facet effects from a many-facet process model and associated metadata or diagnostics.
#' @export
facet_effects <- function(object, channel = c("response", "process")) {
  if (!inherits(object, "eye_manyfacet_process_irt")) stop("`object` must be an eye_manyfacet_process_irt.", call. = FALSE)
  channel <- match.arg(channel)
  fit <- if (channel == "response") object$response_model else object$process_model
  if (is.null(fit)) stop(sprintf("No %s model was fitted.", channel), call. = FALSE)
  re <- lme4::ranef(fit)
  vc <- as.data.frame(lme4::VarCorr(fit))
  list(random_effects = re, variance_components = vc)
}

#' Audit process measurement invariance across facets
#' @param object A fitted eyeprocess model or audit object.
#' @param channel Measurement channel to inspect.
#' @param relative_sd_threshold Value supplied to `relative_sd_threshold`; see Details for its model-specific role.
#' @return An object of class "eye_process_measurement_invariance", stored as a named list, with components "pass", "threshold", "components", "channel", "note". It contains process measurement invariance across facets and associated metadata or diagnostics needed to interpret the result.
#' @export
audit_process_measurement_invariance <- function(object, channel = c("process", "response"),
                                                 relative_sd_threshold = 0.25) {
  channel <- match.arg(channel)
  ef <- facet_effects(object, channel = channel)
  vc <- ef$variance_components
  vc <- vc[is.na(vc$var2), , drop = FALSE]
  residual <- vc$sdcor[vc$grp == "Residual"]
  base <- if (length(residual) && is.finite(residual[1L])) residual[1L] else max(vc$sdcor, na.rm = TRUE)
  vc$relative_sd <- vc$sdcor / pmax(base, .Machine$double.eps)
  vc$flag <- vc$grp != "Residual" & is.finite(vc$relative_sd) & vc$relative_sd > relative_sd_threshold
  structure(list(pass = !any(vc$flag), threshold = relative_sd_threshold,
                 components = vc, channel = channel,
                 note = "Facet variance is evidence of transportability differences, not proof of vendor bias or causal device effects."),
            class = "eye_process_measurement_invariance")
}

.ep07_segment_loglik <- function(y = NULL, rt = NULL, gaze = NULL) {
  ll <- 0; k <- 0L
  if (!is.null(y)) {
    y <- y[!is.na(y)]
    if (length(y)) {
      p <- pmin(pmax(mean(y), 1e-6), 1 - 1e-6)
      ll <- ll + sum(stats::dbinom(y, 1, p, log = TRUE)); k <- k + 1L
    }
  }
  for (z in list(rt, gaze)) {
    if (!is.null(z)) {
      z <- z[is.finite(z)]
      if (length(z) >= 2L) {
        s <- max(stats::sd(z), 1e-6)
        ll <- ll + sum(stats::dnorm(z, mean(z), s, log = TRUE)); k <- k + 2L
      }
    }
  }
  c(logLik = ll, parameters = k)
}

.ep07_best_split <- function(y, rt, gaze, min_segment) {
  n <- max(length(y), length(rt), length(gaze))
  null <- .ep07_segment_loglik(y, rt, gaze)
  null_bic <- -2 * null["logLik"] + null["parameters"] * log(max(n, 2))
  candidates <- seq.int(min_segment, n - min_segment)
  if (!length(candidates)) return(list(index = NA_integer_, delta_sic = 0, null_sic = null_bic, split_sic = null_bic))
  scores <- vapply(candidates, function(k) {
    a <- .ep07_segment_loglik(if (!is.null(y)) y[seq_len(k)] else NULL,
                              if (!is.null(rt)) rt[seq_len(k)] else NULL,
                              if (!is.null(gaze)) gaze[seq_len(k)] else NULL)
    b <- .ep07_segment_loglik(if (!is.null(y)) y[(k + 1L):n] else NULL,
                              if (!is.null(rt)) rt[(k + 1L):n] else NULL,
                              if (!is.null(gaze)) gaze[(k + 1L):n] else NULL)
    -2 * (a["logLik"] + b["logLik"]) + (a["parameters"] + b["parameters"] + 1L) * log(max(n, 2))
  }, numeric(1))
  j <- which.min(scores)
  list(index = candidates[j], delta_sic = unname(null_bic - scores[j]), null_sic = unname(null_bic), split_sic = unname(scores[j]))
}

#' Detect IRT/process change points using an SIC-inspired multichannel score
#'
#' This implementation is a transparent package reference inspired by the 2026
#' SIC-CPA literature. It combines Bernoulli response likelihood with normal
#' log-RT and standardized gaze likelihoods. It is not a line-for-line
#' reproduction of the article's estimator.
#'
#' @param data Input data frame or compatible tabular object.
#' @param person Person or participant identifier column.
#' @param order Within-sequence ordering variable.
#' @param response Response variable or response-column name.
#' @param rt Response-time variable or column name.
#' @param gaze Gaze/process variable or column name.
#' @param min_segment Minimum segment length.
#' @param min_delta_sic Minimum information-criterion improvement.
#' @param max_changes Maximum number of change points.
#' @return An object of class "eye_irt_changepoints", stored as a named list, with components "results", "channels", "method", "min_segment", "min_delta_sic", "max_changes". It contains iRT/process change points using an SIC-inspired multichannel score and associated metadata or diagnostics needed to interpret the result.
#' @export
detect_irt_changepoints <- function(data,
                                    person = "participant_id", order = "item_order",
                                    response = "response", rt = "rt", gaze = NULL,
                                    min_segment = 5L, min_delta_sic = 2,
                                    max_changes = 2L) {
  cols <- c(person, order, response, rt, gaze)
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  d <- .ep07_model_frame(data, cols)
  if (min_segment < 2L) stop("`min_segment` must be at least 2.", call. = FALSE)
  split_d <- split(d, d[[person]])
  rows <- list()
  for (pid in names(split_d)) {
    z <- split_d[[pid]]
    z <- z[order(z[[order]]), , drop = FALSE]
    y <- if (!is.null(response)) as.numeric(z[[response]]) else NULL
    r <- if (!is.null(rt)) log(pmax(as.numeric(z[[rt]]), .Machine$double.eps)) else NULL
    g <- if (!is.null(gaze)) as.numeric(scale(z[[gaze]])) else NULL
    best <- .ep07_best_split(y, r, g, min_segment)
    rows[[length(rows) + 1L]] <- data.frame(
      participant_id = pid,
      changepoint_index = best$index,
      changepoint_order = if (is.na(best$index)) NA else z[[order]][best$index],
      delta_sic = best$delta_sic,
      detected = is.finite(best$delta_sic) && best$delta_sic >= min_delta_sic,
      n = nrow(z), stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  structure(list(results = out, channels = c(response = response, rt = rt, gaze = gaze),
                 method = "sic-inspired-reference", min_segment = min_segment,
                 min_delta_sic = min_delta_sic, max_changes = max_changes),
            class = "eye_irt_changepoints")
}

#' Fit a change-point RT IRT workflow
#' @param data Input data frame or compatible tabular object.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @param refit Whether the model is refitted after segmentation.
#' @return An object of class "eye_changepoint_rt_irt", stored as a named list, with components "changepoints", "refit_requested", "status". It contains a change-point RT IRT workflow and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_changepoint_rt_irt <- function(data, ..., refit = TRUE) {
  cp <- detect_irt_changepoints(data, ..., gaze = NULL)
  structure(list(changepoints = cp, refit_requested = isTRUE(refit), status = "experimental-reference"),
            class = "eye_changepoint_rt_irt")
}

#' Fit a multimodal change-point IRT workflow
#' @param data Input data frame or compatible tabular object.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @param gaze Gaze/process variable or column name.
#' @param refit Whether the model is refitted after segmentation.
#' @return An object of class "eye_changepoint_multimodal_irt", stored as a named list, with components "changepoints", "refit_requested", "status". It contains a multimodal change-point IRT workflow and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_changepoint_multimodal_irt <- function(data, ..., gaze = "fixation_count", refit = TRUE) {
  cp <- detect_irt_changepoints(data, ..., gaze = gaze)
  structure(list(changepoints = cp, refit_requested = isTRUE(refit), status = "experimental-reference"),
            class = "eye_changepoint_multimodal_irt")
}

#' Iteratively detect, clean, and recalibrate after process change points
#'
#' @param fitter Function accepting a data frame and returning a calibration fit.
#' @param policy `flag`, `exclude_post_change`, or `add_regime`.
#' @param data Input data frame or compatible tabular object.
#' @param person Person or participant identifier column.
#' @param order Within-sequence ordering variable.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return An object of class "eye_changepoint_recalibration", stored as a named list, with components "changepoints", "data", "fit", "policy". It contains iteratively detect, clean, and recalibrate after process change points and associated metadata or diagnostics needed to interpret the result.
#' @export
recalibrate_after_changepoint <- function(data, fitter,
                                          person = "participant_id", order = "item_order",
                                          policy = c("flag", "exclude_post_change", "add_regime"), ...) {
  policy <- match.arg(policy)
  if (!is.function(fitter)) stop("`fitter` must be a function.", call. = FALSE)
  cp <- detect_irt_changepoints(data, person = person, order = order, ...)
  d <- .ep07_as_data_frame(data, "data")
  map <- cp$results
  cp_index <- stats::setNames(map$changepoint_order, map$participant_id)
  d$.process_regime <- "pre"
  for (i in seq_len(nrow(d))) {
    cut <- cp_index[[as.character(d[[person]][i])]]
    if (!is.null(cut) && is.finite(cut) && d[[order]][i] > cut) d$.process_regime[i] <- "post"
  }
  d$.process_regime <- factor(d$.process_regime, levels = c("pre", "post"))
  used <- switch(policy,
                 flag = d,
                 add_regime = d,
                 exclude_post_change = d[d$.process_regime != "post", , drop = FALSE])
  fit <- fitter(used)
  structure(list(changepoints = cp, data = used, fit = fit, policy = policy),
            class = "eye_changepoint_recalibration")
}
