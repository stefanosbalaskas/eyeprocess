# eyeprocess 0.9 Milestone #2: advanced IRT diagnostics, targeting, and governance

#' Compute residual-based Infit and Outfit summaries
#'
#' These statistics summarize response-model residual behavior. They are not
#' labels for motivation, misconduct, diagnosis, or respondent intent.
#' @param observed Observed responses or observed values.
#' @param expected Model-expected probabilities or expected values.
#' @param by Grouping variables or aggregation level.
#' @param min_variance Minimum variance used to stabilize residual calculations.
#' @export
eyeprocess_irt_infit_outfit <- function(observed, expected, by = c("item", "person"), min_variance = 1e-8) {
  by <- match.arg(by)
  y <- .ep09m2_binary_matrix(observed, "observed")
  p <- .ep09m2_prob_matrix(expected, dim(y), "expected")
  min_variance <- as.numeric(min_variance)
  if (length(min_variance) != 1L || !is.finite(min_variance) || min_variance <= 0)
    stop("min_variance must be a positive finite scalar.", call. = FALSE)
  v <- p * (1 - p)
  missing_cell <- is.na(y) | is.na(p)
  v[missing_cell] <- NA_real_
  r2 <- (y - p)^2
  z2 <- r2 / pmax(v, min_variance)
  margin <- if (by == "item") 2L else 1L
  outfit <- if (margin == 2L) colMeans(z2, na.rm = TRUE) else rowMeans(z2, na.rm = TRUE)
  infit_num <- if (margin == 2L) colSums(r2, na.rm = TRUE) else rowSums(r2, na.rm = TRUE)
  infit_den <- if (margin == 2L) colSums(v, na.rm = TRUE) else rowSums(v, na.rm = TRUE)
  infit <- infit_num / pmax(infit_den, min_variance)
  n <- if (margin == 2L) colSums(is.finite(y) & is.finite(p)) else rowSums(is.finite(y) & is.finite(p))
  infit[n == 0L] <- NA_real_; outfit[n == 0L] <- NA_real_
  data.frame(
    unit = if (by == "item")  .ep09m2_or(colnames(y), paste0("item_", seq_len(ncol(y)))) else  .ep09m2_or(rownames(y), paste0("person_", seq_len(nrow(y)))),
    n = as.integer(n),
    infit = as.numeric(infit),
    outfit = as.numeric(outfit),
    stringsAsFactors = FALSE
  )
}

#' Standardized log-likelihood person-fit diagnostic
#'
#' Computes a Bernoulli response-pattern log-likelihood standardized against
#' its model-implied mean and variance. Extreme values are model diagnostics,
#' not evidence of cheating, disengagement, or a psychological state.
#' @param observed Observed responses or observed values.
#' @param expected Model-expected probabilities or expected values.
#' @param min_probability Lower probability bound used for numerical stabilization.
#' @export
eyeprocess_irt_person_fit_lz <- function(observed, expected, min_probability = 1e-8) {
  y <- .ep09m2_binary_matrix(observed, "observed")
  p <- .ep09m2_prob_matrix(expected, dim(y), "expected")
  min_probability <- as.numeric(min_probability)
  if (length(min_probability) != 1L || !is.finite(min_probability) || min_probability <= 0 || min_probability >= 0.5)
    stop("min_probability must lie in (0, 0.5).", call. = FALSE)
  # Preserve matrix dimensions while clipping probabilities. Using pmax()/pmin()
  # with a scalar first argument may strip matrix attributes and break row-wise
  # diagnostics downstream.
  p[p < min_probability] <- min_probability
  p[p > 1 - min_probability] <- 1 - min_probability
  p[is.na(y)] <- NA_real_
  logp1 <- log(p); logp0 <- log1p(-p)
  ll <- rowSums(y * logp1 + (1 - y) * logp0, na.rm = TRUE)
  mu <- rowSums(p * logp1 + (1 - p) * logp0, na.rm = TRUE)
  var_ll <- rowSums(p * (1 - p) * (logp1 - logp0)^2, na.rm = TRUE)
  lz <- (ll - mu) / sqrt(pmax(var_ll, .Machine$double.eps))
  n_observed <- rowSums(is.finite(y) & is.finite(p))
  lz[n_observed == 0L] <- NA_real_
  data.frame(
    person =  .ep09m2_or(rownames(y), paste0("person_", seq_len(nrow(y)))),
    n_observed = n_observed,
    log_likelihood = ll,
    expected_log_likelihood = mu,
    lz = lz,
    stringsAsFactors = FALSE
  )
}

#' Audit item-bank information coverage across a theta region
#' @param items Item-parameter data frame or item collection.
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param target_information Target test-information level.
#' @param target Target level, distribution, or criterion.
#' @export
eyeprocess_irt_bank_coverage <- function(items, theta = seq(-4, 4, length.out = 161), target_information = 5, target = c(-2, 2)) {
  theta <- .ep09m2_theta(theta)
  target_information <- as.numeric(target_information)
  target <- as.numeric(target)
  if (length(target_information) != 1L || !is.finite(target_information) || target_information < 0)
    stop("target_information must be a finite non-negative scalar.", call. = FALSE)
  if (length(target) != 2L || any(!is.finite(target)) || target[[1L]] >= target[[2L]])
    stop("target must be an increasing finite length-2 vector.", call. = FALSE)
  curve <- eyeprocess_irt_test_information(theta, items)
  keep <- curve$theta >= target[[1L]] & curve$theta <= target[[2L]]
  if (!any(keep)) stop("theta grid does not overlap the target region.", call. = FALSE)
  below <- curve$information[keep] < target_information
  structure(list(
    curve = curve,
    target = target,
    target_information = target_information,
    fraction_target_met = mean(!below),
    minimum_information = min(curve$information[keep]),
    maximum_sem = max(curve$conditional_sem[keep]),
    gaps = curve[keep & curve$information < target_information, , drop = FALSE]
  ), class = "eye_irt_bank_coverage")
}

#' Compare an examinee distribution with item-bank targeting
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param items Item-parameter data frame or item collection.
#' @param breaks Break points used to summarize latent-scale targeting.
#' @export
eyeprocess_irt_targeting_gap <- function(theta, items, breaks = seq(-4, 4, by = 0.5)) {
  theta <- as.numeric(theta); theta <- theta[is.finite(theta)]
  if (!length(theta)) stop("theta must contain at least one finite value.", call. = FALSE)
  breaks <- as.numeric(breaks)
  if (length(breaks) < 3L || any(!is.finite(breaks)) || is.unsorted(breaks, strictly = TRUE))
    stop("breaks must be a strictly increasing finite vector.", call. = FALSE)
  mids <- head(breaks, -1L) + diff(breaks) / 2
  if (min(theta) < min(breaks) || max(theta) > max(breaks)) stop("breaks must span all finite theta values.", call. = FALSE)
  h <- hist(theta, breaks = breaks, plot = FALSE, include.lowest = TRUE)
  info <- eyeprocess_irt_test_information(mids, items)
  density <- h$counts / sum(h$counts)
  info_scaled <- if (sum(info$information) > 0) info$information / sum(info$information) else rep(0, length(mids))
  tab <- data.frame(theta = mids, person_mass = density, information_mass = info_scaled,
                    gap = density - info_scaled, stringsAsFactors = FALSE)
  structure(list(table = tab, absolute_gap = sum(abs(tab$gap)) / 2,
                 interpretation = "Targeting gap compares empirical score-location mass with normalized information; it is not a validity coefficient."),
            class = "eye_irt_targeting_gap")
}

#' Summarise decision precision at one or more theta cut scores
#' @param theta_estimate Estimated latent-trait values.
#' @param standard_error Standard errors corresponding to the estimates.
#' @param cut_score Latent-scale classification cut score.
#' @param confidence Requested confidence level.
#' @export
eyeprocess_irt_classification_precision <- function(theta_estimate, standard_error, cut_score = 0, confidence = 0.95) {
  th <- as.numeric(theta_estimate); se <- as.numeric(standard_error)
  if (length(se) == 1L) se <- rep(se, length(th))
  if (length(th) != length(se) || any(!is.finite(th)) || any(!is.finite(se)) || any(se < 0))
    stop("theta_estimate and standard_error must be compatible finite vectors with non-negative SEs.", call. = FALSE)
  cut <- as.numeric(cut_score)
  if (!length(cut) || any(!is.finite(cut))) stop("cut_score must be finite.", call. = FALSE)
  confidence <- as.numeric(confidence)
  if (length(confidence) != 1L || !is.finite(confidence) || confidence <= 0 || confidence >= 1)
    stop("confidence must lie in (0,1).", call. = FALSE)
  z <- stats::qnorm((1 + confidence) / 2)
  out <- do.call(rbind, lapply(cut, function(c0) {
    prob_above <- ifelse(se > 0, 1 - stats::pnorm((c0 - th) / se), as.numeric(th > c0) + 0.5 * as.numeric(th == c0))
    data.frame(case = seq_along(th), cut_score = c0, theta = th, se = se,
               lower = th - z * se, upper = th + z * se,
               probability_above = prob_above,
               classification = ifelse(prob_above >= 0.5, "above", "below"),
               confidence_in_classification = pmax(prob_above, 1 - prob_above), stringsAsFactors = FALSE)
  }))
  attr(out, "guardrail") <- "Classification precision quantifies uncertainty relative to declared cut scores; it does not justify the substantive meaning of those cuts."
  out
}

#' Audit missing-by-design structure in an IRT response matrix
#' @param responses Response matrix or response data.
#' @param design Validation or simulation design object.
#' @param min_administered Minimum number of administered items required for a record.
#' @export
eyeprocess_irt_missing_by_design_audit <- function(responses, design = NULL, min_administered = 1L) {
  y <- as.matrix(responses)
  if (!is.numeric(y) && !is.integer(y)) stop("responses must be a numeric/integer matrix.", call. = FALSE)
  min_administered <- as.integer(min_administered)
  if (length(min_administered) != 1L || is.na(min_administered) || min_administered < 1L)
    stop("min_administered must be a positive integer.", call. = FALSE)
  observed <- !is.na(y)
  if (!is.null(design)) {
    d <- as.matrix(design)
    if (!identical(dim(d), dim(y))) stop("design must have the same dimensions as responses.", call. = FALSE)
    if (any(!is.na(d) & !d %in% c(0, 1))) stop("design must contain only 0/1/NA.", call. = FALSE)
    expected <- d == 1
    structural_missing <- !observed & !is.na(expected) & !expected
    unexpected_missing <- !observed & expected
  } else {
    structural_missing <- matrix(FALSE, nrow(y), ncol(y))
    unexpected_missing <- !observed
  }
  structure(list(
    n_persons = nrow(y),
    n_items = ncol(y),
    observed_fraction = mean(observed),
    administered_per_person = rowSums(observed),
    administered_per_item = colSums(observed),
    sparse_persons = which(rowSums(observed) < min_administered),
    structural_missing = sum(structural_missing, na.rm = TRUE),
    unexpected_missing = sum(unexpected_missing, na.rm = TRUE),
    has_declared_design = !is.null(design)
  ), class = "eye_irt_missing_design_audit")
}

#' Declare prior families for Bayesian IRT engine adapters
#' @param discrimination Discrimination vector or matrix.
#' @param difficulty Item difficulty or location parameter.
#' @param guessing Prior distribution family for the guessing parameter.
#' @param location Prior location hyperparameter.
#' @param scale Prior scale hyperparameter.
#' @param guessing_shape Shape parameters for the guessing prior.
#' @param label Human-readable label.
#' @export
eyeprocess_irt_prior_spec <- function(discrimination = c("lognormal", "normal"), difficulty = "normal",
                                      guessing = c("beta", "logit-normal"), location = 0, scale = 1,
                                      guessing_shape = c(5, 17), label = "default") {
  discrimination <- match.arg(discrimination)
  guessing <- match.arg(guessing)
  difficulty <- match.arg(difficulty, c("normal", "student-t"))
  location <- as.numeric(location); scale <- as.numeric(scale); guessing_shape <- as.numeric(guessing_shape)
  if (length(location) != 1L || !is.finite(location) || length(scale) != 1L || !is.finite(scale) || scale <= 0)
    stop("location must be finite and scale positive finite.", call. = FALSE)
  if (length(guessing_shape) != 2L || any(!is.finite(guessing_shape)) || any(guessing_shape <= 0))
    stop("guessing_shape must contain two positive finite values.", call. = FALSE)
  structure(list(discrimination = discrimination, difficulty = difficulty, guessing = guessing,
                 location = location, scale = scale, guessing_shape = guessing_shape,
                 label = as.character(label)), class = "eye_irt_prior_spec")
}

#' Construct a prior-sensitivity grid for Bayesian IRT analyses
#' @param discrimination_scale Prior scale for item discrimination.
#' @param difficulty_scale Prior scale for item difficulty or location.
#' @param guessing_mean Prior mean for the lower-asymptote or guessing parameter.
#' @export
eyeprocess_irt_prior_sensitivity_grid <- function(discrimination_scale = c(0.5, 1, 1.5),
                                                  difficulty_scale = c(1, 2),
                                                  guessing_mean = c(0.10, 0.20)) {
  discrimination_scale <- as.numeric(discrimination_scale)
  difficulty_scale <- as.numeric(difficulty_scale)
  guessing_mean <- as.numeric(guessing_mean)
  if (any(!is.finite(discrimination_scale)) || any(discrimination_scale <= 0) ||
      any(!is.finite(difficulty_scale)) || any(difficulty_scale <= 0) ||
      any(!is.finite(guessing_mean)) || any(guessing_mean <= 0 | guessing_mean >= 1))
    stop("Prior grid values are outside their admissible ranges.", call. = FALSE)
  out <- expand.grid(discrimination_scale = discrimination_scale,
                     difficulty_scale = difficulty_scale,
                     guessing_mean = guessing_mean,
                     KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  out$prior_id <- sprintf("P%03d", seq_len(nrow(out)))
  out[, c("prior_id", "discrimination_scale", "difficulty_scale", "guessing_mean")]
}

#' Summarise sensitivity of an estimand across declared prior specifications
#' @param results Results table or analysis results.
#' @param prior_id Identifier for the prior specification.
#' @param estimate Estimate column or numerical estimates to summarize.
#' @export
eyeprocess_irt_prior_sensitivity_summary <- function(results, prior_id = "prior_id", estimate = "estimate") {
  results <- .ep09m2_as_df(results, "results")
  .ep09m2_req_cols(results, c(prior_id, estimate), "results")
  v <- as.numeric(results[[estimate]]); keep <- is.finite(v)
  structure(list(
    n_specifications = nrow(results),
    n_finite = sum(keep),
    median = if (any(keep)) stats::median(v[keep]) else NA_real_,
    range = if (any(keep)) diff(range(v[keep])) else NA_real_,
    sd = if (sum(keep) > 1L) stats::sd(v[keep]) else NA_real_,
    table = results,
    guardrail = "Prior sensitivity describes specification dependence; it is not a license for selective prior choice."
  ), class = "eye_irt_prior_sensitivity")
}

#' Create a governed IRT model card
#' @param spec Model, validation, or analysis specification object.
#' @param engine_status Availability/status record for the selected estimation engine.
#' @param identification Identification specification or identification audit.
#' @param fit_evidence Model-fit evidence or diagnostics.
#' @param invariance Measurement-invariance evidence or audit.
#' @param validation Value supplied for the validation argument.
#' @param intended_use Statement of the intended analytical use.
#' @param excluded_interpretations Interpretations explicitly excluded by the model card.
#' @export
eyeprocess_irt_model_card <- function(spec, engine_status = NULL, identification = NULL,
                                      fit_evidence = NULL, invariance = NULL, validation = NULL,
                                      intended_use = NULL, excluded_interpretations = c("diagnosis", "cheating inference", "mental-state inference")) {
  if (!inherits(spec, "eyeprocess_irt_model_spec") && !inherits(spec, "eye_joint_process_irt_spec"))
    stop("spec must be an eyeprocess IRT or joint-process IRT specification.", call. = FALSE)
  structure(list(
    specification = spec,
    engine_status = engine_status,
    identification = identification,
    fit_evidence = fit_evidence,
    invariance = invariance,
    validation = validation,
    intended_use = intended_use,
    excluded_interpretations = unique(as.character(excluded_interpretations)),
    created = as.character(Sys.Date()),
    hash = .ep09m2_hash(list(spec, engine_status, identification, fit_evidence, invariance, validation, intended_use, excluded_interpretations))
  ), class = "eye_irt_model_card")
}

#' Audit completeness of an IRT model card
#' @param card Value supplied for the card argument.
#' @export
eyeprocess_irt_model_card_audit <- function(card) {
  if (!inherits(card, "eye_irt_model_card")) stop("card must be created by eyeprocess_irt_model_card().", call. = FALSE)
  fields <- c("engine_status", "identification", "fit_evidence", "invariance", "validation", "intended_use")
  present <- !vapply(card[fields], function(x) is.null(x) || !length(x), logical(1))
  data.frame(field = fields, present = unname(present), stringsAsFactors = FALSE)
}

#' @export
print.eye_irt_bank_coverage <- function(x, ...) {
  cat("eyeprocess IRT bank-coverage audit\n")
  cat("  target region        :", paste(x$target, collapse = " to "), "\n")
  cat("  target information   :", x$target_information, "\n")
  cat("  fraction target met  :", sprintf("%.3f", x$fraction_target_met), "\n")
  invisible(x)
}

#' @export
print.eye_irt_targeting_gap <- function(x, ...) {
  cat("eyeprocess IRT targeting gap\n")
  cat("  total-variation-like gap:", sprintf("%.3f", x$absolute_gap), "\n")
  invisible(x)
}

#' @export
print.eye_irt_missing_design_audit <- function(x, ...) {
  cat("eyeprocess IRT missing-by-design audit\n")
  cat("  persons/items     :", x$n_persons, "/", x$n_items, "\n")
  cat("  observed fraction :", sprintf("%.3f", x$observed_fraction), "\n")
  cat("  unexpected missing:", x$unexpected_missing, "\n")
  invisible(x)
}

#' @export
print.eye_irt_model_card <- function(x, ...) {
  cat("eyeprocess IRT model card\n")
  cat("  hash:", x$hash, "\n")
  cat("  intended use:", if (is.null(x$intended_use)) "<not declared>" else paste(x$intended_use, collapse = "; "), "\n")
  cat("  excluded interpretations:", paste(x$excluded_interpretations, collapse = "; "), "\n")
  invisible(x)
}
