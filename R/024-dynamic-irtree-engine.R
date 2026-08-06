# Hardened dynamic IRTree and transition engines ------------------------------

.di_stop <- function(...) {
  if (exists(".eye_stop", mode = "function", inherits = TRUE)) .eye_stop(...) else stop(paste0(...), call. = FALSE)
}

.di_softmax <- function(eta) {
  if (is.vector(eta)) {
    eta <- eta - max(eta)
    p <- exp(eta)
    return(p / sum(p))
  }
  eta <- eta - apply(eta, 1L, max)
  p <- exp(eta)
  p / rowSums(p)
}

.di_state_levels <- function(d, states = NULL) {
  observed <- unique(c(as.character(d$from_state), as.character(d$to_state)))
  observed <- observed[!is.na(observed) & nzchar(observed)]
  if (is.null(states)) states <- sort(observed)
  states <- unique(as.character(states))
  if (length(states) < 2L) .di_stop("At least two states are required.")
  if (anyNA(states) || any(!nzchar(states))) .di_stop("State labels must be non-missing and non-empty.")
  states
}

#' Specify a hardened dynamic gaze-state IRTree
#'
#' @param source Sequence source for an `eye_dataset`.
#' @param collapse_consecutive Collapse consecutive identical states.
#' @param engine Estimation engine: auditable binary baseline, penalized
#'   multinomial transition model, or optional CmdStan model.
#' @param hidden_states Number of latent states. Zero fits observed-state
#'   transitions; values of two or more request a hidden-state extension.
#' @param include_response Include item response as a predictor.
#' @param include_person Include person effects.
#' @param include_item Include item effects.
#' @param condition_columns Condition-level predictors.
#' @param transition_predictors Additional transition-level predictors.
#' @param interactions Optional interaction terms supplied as formula strings.
#' @param include_time_gap Include log time gap for irregular observations.
#' @param person_effect Person effect type for Stan.
#' @param item_effect Item effect type for Stan.
#' @param structural_zeros Optional forbidden observed-state transition pairs.
#' @param allowed_transitions Optional allowed observed-state transition pairs.
#' @param hidden_structural_zeros Optional forbidden hidden-state pairs named `state1`, `state2`, and so forth.
#' @param hidden_allowed_transitions Optional explicitly allowed hidden-state pairs.
#' @param missing_state Treatment of missing observed states.
#' @param uncertain_state_probability Optional column containing probability of
#'   the recorded destination state.
#' @param misclassification_matrix Optional observed-state misclassification matrix.
#' @param ridge Penalization for the multinomial baseline.
#' @param standardize Standardize numeric design columns.
#' @param reference_state Optional reference destination state.
#' @param chains,parallel_chains,iter_warmup,iter_sampling CmdStan controls.
#' @param adapt_delta,max_treedepth CmdStan sampler controls.
#' @return An `eye_dynamic_irtree_spec`.
#' @export
dynamic_irtree_spec <- function(
    source = c("samples", "visits", "fixations"),
    collapse_consecutive = TRUE,
    engine = c("baseline", "multinomial", "stan"),
    hidden_states = 0L,
    include_response = TRUE,
    include_person = FALSE,
    include_item = TRUE,
    condition_columns = character(),
    transition_predictors = character(),
    interactions = character(),
    include_time_gap = TRUE,
    person_effect = c("none", "fixed", "random"),
    item_effect = c("none", "fixed", "random"),
    structural_zeros = NULL,
    allowed_transitions = NULL,
    hidden_structural_zeros = NULL,
    hidden_allowed_transitions = NULL,
    missing_state = c("drop", "unknown", "marginalize"),
    uncertain_state_probability = NULL,
    misclassification_matrix = NULL,
    ridge = 1e-4,
    standardize = TRUE,
    reference_state = NULL,
    chains = 4L,
    parallel_chains = chains,
    iter_warmup = 1000L,
    iter_sampling = 1000L,
    adapt_delta = 0.95,
    max_treedepth = 12L) {
  hidden_states <- as.integer(hidden_states)
  if (length(hidden_states) != 1L || is.na(hidden_states) || hidden_states == 1L || hidden_states < 0L) .di_stop("`hidden_states` must be zero or at least two.")
  ridge <- as.numeric(ridge)
  if (length(ridge) != 1L || !is.finite(ridge) || ridge < 0) .di_stop("`ridge` must be finite and non-negative.")
  engine <- match.arg(engine)
  person_effect <- match.arg(person_effect)
  item_effect <- match.arg(item_effect)
  if (isTRUE(include_person) && person_effect == "none") person_effect <- if (engine == "stan") "random" else "fixed"
  if (isTRUE(include_item) && item_effect == "none") item_effect <- if (engine == "stan") "random" else "fixed"
  if (hidden_states >= 2L && person_effect == "random") {
    warning("Hidden-state dynamic IRTree currently represents person effects explicitly as fixed design effects.", call. = FALSE)
    person_effect <- "fixed"
  }
  if (hidden_states >= 2L && item_effect == "random") {
    warning("Hidden-state dynamic IRTree currently represents item effects explicitly as fixed design effects.", call. = FALSE)
    item_effect <- "fixed"
  }
  integer_controls <- c(chains = chains, parallel_chains = parallel_chains, iter_warmup = iter_warmup, iter_sampling = iter_sampling, max_treedepth = max_treedepth)
  integer_controls <- stats::setNames(suppressWarnings(as.integer(integer_controls)), names(integer_controls))
  if (anyNA(integer_controls) || any(integer_controls < 1L)) .di_stop("Stan iteration, chain, and tree-depth controls must be positive integers.")
  if (integer_controls[["parallel_chains"]] > integer_controls[["chains"]]) .di_stop("`parallel_chains` cannot exceed `chains`.")
  adapt_delta <- suppressWarnings(as.numeric(adapt_delta))
  if (length(adapt_delta) != 1L || !is.finite(adapt_delta) || adapt_delta <= 0 || adapt_delta >= 1) .di_stop("`adapt_delta` must lie strictly between 0 and 1.")
  character_vectors <- list(condition_columns = condition_columns, transition_predictors = transition_predictors, interactions = interactions)
  if (any(vapply(character_vectors, function(z) anyNA(z) || any(!nzchar(as.character(z))), logical(1)))) .di_stop("Predictor and interaction names must be non-missing and non-empty.")
  structure(list(
    source = match.arg(source),
    collapse_consecutive = isTRUE(collapse_consecutive),
    engine = engine,
    hidden_states = hidden_states,
    include_response = isTRUE(include_response),
    include_person = person_effect != "none",
    include_item = item_effect != "none",
    condition_columns = unique(as.character(condition_columns)),
    transition_predictors = unique(as.character(transition_predictors)),
    interactions = unique(as.character(interactions)),
    include_time_gap = isTRUE(include_time_gap),
    person_effect = person_effect,
    item_effect = item_effect,
    structural_zeros = structural_zeros,
    allowed_transitions = allowed_transitions,
    hidden_structural_zeros = hidden_structural_zeros,
    hidden_allowed_transitions = hidden_allowed_transitions,
    missing_state = match.arg(missing_state),
    uncertain_state_probability = uncertain_state_probability,
    misclassification_matrix = misclassification_matrix,
    ridge = ridge,
    standardize = isTRUE(standardize),
    reference_state = reference_state,
    chains = integer_controls[["chains"]],
    parallel_chains = integer_controls[["parallel_chains"]],
    iter_warmup = integer_controls[["iter_warmup"]],
    iter_sampling = integer_controls[["iter_sampling"]],
    adapt_delta = adapt_delta,
    max_treedepth = integer_controls[["max_treedepth"]]
  ), class = "eye_dynamic_irtree_spec")
}

.di_transitions_from_long <- function(data, person = "participant_id", item = "item_id", trial = "trial_id", state = "state", time = NULL) {
  required <- c(person, item, state)
  missing <- setdiff(required, names(data))
  if (length(missing)) .di_stop("Long state data are missing: ", paste(missing, collapse = ", "))
  if (anyNA(data[[person]]) || anyNA(data[[item]])) .di_stop("Participant and item identifiers must be non-missing.")
  if (!trial %in% names(data)) data[[trial]] <- paste(data[[person]], data[[item]], sep = "::")
  if (anyNA(data[[trial]])) .di_stop("Trial identifiers must be non-missing.")
  order_columns <- c(person, trial, if (!is.null(time) && time %in% names(data)) time else NULL)
  data <- data[do.call(order, data[order_columns]), , drop = FALSE]
  group_key <- paste(as.character(data[[person]]), as.character(data[[trial]]), sep = "\r")
  groups <- split(seq_len(nrow(data)), group_key, drop = TRUE)
  rows <- lapply(groups, function(index) {
    if (length(index) < 2L) return(NULL)
    z <- data[index, , drop = FALSE]
    out <- z[-nrow(z), , drop = FALSE]
    out$from_state <- as.character(z[[state]][-nrow(z)])
    out$to_state <- as.character(z[[state]][-1L])
    out$step <- seq_len(nrow(out))
    out$participant_id <- as.character(z[[person]][-nrow(z)])
    out$item_id <- as.character(z[[item]][-nrow(z)])
    out$trial_id <- as.character(z[[trial]][-nrow(z)])
    if (!is.null(time) && time %in% names(z)) {
      out$time <- as.numeric(z[[time]][-nrow(z)])
      out$next_time <- as.numeric(z[[time]][-1L])
      out$time_gap <- out$next_time - out$time
    } else {
      out$time_gap <- 1
    }
    out
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Prepare ordered transition data for dynamic IRTree models
#'
#' @param x An `eye_dataset` or data frame.
#' @param spec Dynamic IRTree specification.
#' @param person,item,trial,state,time Column names for long state data.
#' @param from,to Existing transition columns for transition-format data.
#' @param states Optional complete state vocabulary.
#' @return An `eye_dynamic_transition_data` data frame.
#' @export
prepare_dynamic_irtree_data <- function(
    x,
    spec = dynamic_irtree_spec(),
    person = "participant_id",
    item = "item_id",
    trial = "trial_id",
    state = "state",
    time = NULL,
    from = "from_state",
    to = "to_state",
    states = NULL) {
  if (!inherits(spec, "eye_dynamic_irtree_spec")) .di_stop("`spec` must be created by `dynamic_irtree_spec()`.")
  if (inherits(x, "eye_dataset")) {
    d <- .ep_sequence_transitions(x, spec$source, spec$collapse_consecutive)
    if (!nrow(d)) .di_stop("No AOI transitions are available.")
    names(d)[match(c("from", "to"), names(d))] <- c("from_state", "to_state")
    if (!"time_gap" %in% names(d)) d$time_gap <- 1
  } else {
    if (!is.data.frame(x)) .di_stop("`x` must be an eye dataset or data frame.")
    if (all(c(from, to) %in% names(x))) {
      d <- x
      d$from_state <- as.character(d[[from]])
      d$to_state <- as.character(d[[to]])
      if (!person %in% names(d)) d[[person]] <- "P1"
      if (!item %in% names(d)) d[[item]] <- "I1"
      if (!trial %in% names(d)) d[[trial]] <- paste(d[[person]], d[[item]], sep = "::")
      d$participant_id <- as.character(d[[person]])
      d$item_id <- as.character(d[[item]])
      d$trial_id <- as.character(d[[trial]])
      if (!"step" %in% names(d)) d$step <- ave(seq_len(nrow(d)), paste(d$participant_id, d$trial_id, sep = "\r"), FUN = seq_along)
      if (!"time_gap" %in% names(d)) d$time_gap <- 1
    } else {
      d <- .di_transitions_from_long(x, person, item, trial, state, time)
    }
  }
  if (!nrow(d)) .di_stop("No transitions could be prepared.")
  required_ids <- c("participant_id", "item_id", "trial_id")
  if (any(vapply(d[required_ids], function(z) anyNA(z) || any(!nzchar(as.character(z))), logical(1)))) .di_stop("Participant, item, and trial identifiers must be non-missing and non-empty.")
  d$from_state <- as.character(d$from_state)
  d$to_state <- as.character(d$to_state)
  missing_from <- is.na(d$from_state) | !nzchar(d$from_state)
  missing_to <- is.na(d$to_state) | !nzchar(d$to_state)
  if (spec$missing_state == "drop") {
    d <- d[!missing_from & !missing_to, , drop = FALSE]
  } else {
    d$from_state[missing_from] <- "<UNKNOWN>"
    d$to_state[missing_to] <- "<UNKNOWN>"
    if (spec$missing_state == "marginalize" && spec$hidden_states < 2L) .di_stop("`missing_state = 'marginalize'` requires the hidden-state Stan engine.")
  }
  if (!nrow(d)) .di_stop("No transitions remain after missing-state handling.")
  states <- .di_state_levels(d, states)
  d$from_state <- factor(d$from_state, levels = states)
  d$to_state <- factor(d$to_state, levels = states)
  d$from_index <- as.integer(d$from_state)
  d$to_index <- as.integer(d$to_state)
  d$person_index <- match(d$participant_id, unique(d$participant_id))
  d$item_index <- match(d$item_id, unique(d$item_id))
  trial_key <- paste(d$participant_id, d$trial_id, sep = "\r")
  d$trial_index <- match(trial_key, unique(trial_key))
  d$time_gap <- suppressWarnings(as.numeric(d$time_gap))
  d$time_gap[!is.finite(d$time_gap) | d$time_gap <= 0] <- 1
  if (!is.null(spec$uncertain_state_probability)) {
    if (!spec$uncertain_state_probability %in% names(d)) .di_stop("Uncertain-state probability column is absent: ", spec$uncertain_state_probability)
    state_probability <- suppressWarnings(as.numeric(d[[spec$uncertain_state_probability]]))
    if (any(!is.finite(state_probability)) || any(state_probability <= 0) || any(state_probability > 1)) .di_stop("Recorded-state probabilities must be finite values in (0, 1].")
    d$state_probability <- state_probability
  } else {
    d$state_probability <- ifelse(spec$missing_state == "marginalize" & as.character(d$to_state) == "<UNKNOWN>", 1 / length(states), 1)
  }
  attr(d, "states") <- states
  class(d) <- c("eye_dynamic_transition_data", "data.frame")
  d
}

#' Define structural-zero and allowed transition masks
#'
#' @param states State labels.
#' @param forbidden Two-column data frame/matrix of forbidden from-to pairs.
#' @param structural_zeros Alias for `forbidden`.
#' @param allowed Two-column data frame/matrix of explicitly allowed pairs.
#' @param allowed_transitions Alias for `allowed`.
#' @param allow_self Whether self transitions are allowed by default.
#' @return Logical transition matrix with rows as source states.
#' @export
structural_transition_mask <- function(states, forbidden = NULL, allowed = NULL, allow_self = TRUE, structural_zeros = NULL, allowed_transitions = NULL) {
  if (is.null(forbidden) && !is.null(structural_zeros)) forbidden <- structural_zeros
  if (is.null(allowed) && !is.null(allowed_transitions)) allowed <- allowed_transitions
  states <- unique(as.character(states))
  if (length(states) < 2L) .di_stop("At least two states are required.")
  mask <- matrix(TRUE, nrow = length(states), ncol = length(states), dimnames = list(from = states, to = states))
  if (!isTRUE(allow_self)) diag(mask) <- FALSE
  apply_pairs <- function(pairs, value) {
    if (is.null(pairs)) return()
    pairs <- as.data.frame(pairs, stringsAsFactors = FALSE)
    if (ncol(pairs) < 2L) .di_stop("Transition pairs require from and to columns.")
    for (i in seq_len(nrow(pairs))) {
      from <- as.character(pairs[i, 1L]); to <- as.character(pairs[i, 2L])
      if (!from %in% states || !to %in% states) .di_stop("Unknown state in transition pair: ", from, " -> ", to)
      mask[from, to] <<- value
    }
  }
  if (!is.null(allowed)) {
    mask[,] <- FALSE
    apply_pairs(allowed, TRUE)
  }
  apply_pairs(forbidden, FALSE)
  if (any(rowSums(mask) == 0L)) .di_stop("Every source state must permit at least one destination state.")
  mask
}

.di_design_formula <- function(spec, data) {
  terms <- c(if (spec$hidden_states < 2L) "from_state" else NULL, "stats::poly(step, 2, raw = TRUE)")
  if (spec$include_time_gap) terms <- c(terms, "log1p(time_gap)")
  if (spec$include_response && "score" %in% names(data) && any(is.finite(data$score))) terms <- c(terms, "score")
  terms <- c(terms, intersect(spec$condition_columns, names(data)), intersect(spec$transition_predictors, names(data)))
  if (spec$person_effect == "fixed") terms <- c(terms, "participant_id")
  if (spec$item_effect == "fixed") terms <- c(terms, "item_id")
  terms <- c(terms, spec$interactions)
  stats::as.formula(paste("~", paste(unique(terms), collapse = " + ")))
}

#' Build an explicit dynamic-transition design matrix
#'
#' @param data Prepared transition data.
#' @param spec Dynamic IRTree specification.
#' @param formula Optional explicit right-hand-side formula.
#' @return An `eye_transition_design` object.
#' @export
dynamic_transition_design <- function(data, spec = dynamic_irtree_spec(), formula = NULL) {
  if (!inherits(data, "eye_dynamic_transition_data")) data <- prepare_dynamic_irtree_data(data, spec)
  if (is.null(formula)) formula <- .di_design_formula(spec, data)
  X <- stats::model.matrix(formula, data = data, na.action = stats::na.pass)
  if (anyNA(X)) {
    keep <- stats::complete.cases(X)
    data <- data[keep, , drop = FALSE]
    X <- X[keep, , drop = FALSE]
  }
  scaling <- NULL
  if (isTRUE(spec$standardize) && ncol(X)) {
    numeric_columns <- which(vapply(seq_len(ncol(X)), function(j) length(unique(X[, j])) > 2L && all(is.finite(X[, j])), logical(1)))
    numeric_columns <- setdiff(numeric_columns, which(colnames(X) == "(Intercept)"))
    if (length(numeric_columns)) {
      center <- colMeans(X[, numeric_columns, drop = FALSE])
      scale <- apply(X[, numeric_columns, drop = FALSE], 2L, stats::sd)
      scale[!is.finite(scale) | scale == 0] <- 1
      X[, numeric_columns] <- sweep(sweep(X[, numeric_columns, drop = FALSE], 2L, center, "-"), 2L, scale, "/")
      scaling <- data.frame(term = colnames(X)[numeric_columns], center = center, scale = scale, stringsAsFactors = FALSE)
    }
  }
  states <- attr(data, "states")
  mask <- structural_transition_mask(states, spec$structural_zeros, spec$allowed_transitions)
  allowed <- t(vapply(data$from_index, function(index) mask[index, ], logical(length(states))))
  out <- list(
    data = data,
    formula = formula,
    X = X,
    y = data$to_index,
    states = states,
    allowed = allowed,
    transition_mask = mask,
    scaling = scaling,
    participants = unique(data$participant_id),
    items = unique(data$item_id),
    trials = unique(data$trial_index),
    spec = spec
  )
  class(out) <- "eye_transition_design"
  out
}

#' @export
print.eye_transition_design <- function(x, ...) {
  cat("Dynamic transition design\n")
  cat("Transitions: ", nrow(x$X), "\n", sep = "")
  cat("Predictors:  ", ncol(x$X), "\n", sep = "")
  cat("States:      ", length(x$states), "\n", sep = "")
  invisible(x)
}

.di_multinomial_unpack <- function(par, D, K, reference) {
  B <- matrix(0, nrow = D, ncol = K)
  destinations <- setdiff(seq_len(K), reference)
  B[, destinations] <- matrix(par, nrow = D, ncol = K - 1L)
  B
}

.di_multinomial_objective <- function(par, X, y, allowed, weights, ridge, reference) {
  K <- ncol(allowed); D <- ncol(X)
  B <- .di_multinomial_unpack(par, D, K, reference)
  eta <- X %*% B
  eta[!allowed] <- -1e12
  eta <- eta - apply(eta, 1L, max)
  log_denom <- log(rowSums(exp(eta)))
  logp <- eta[cbind(seq_len(nrow(X)), y)] - log_denom
  if (any(!is.finite(logp))) return(.Machine$double.xmax / 10)
  -sum(weights * logp) + 0.5 * ridge * sum(par^2)
}

#' Fit a penalized multinomial transition model
#'
#' @param design Transition design.
#' @param ridge Ridge penalty.
#' @param reference_state Reference destination state.
#' @param control Passed to `optim()`.
#' @return An `eye_multinomial_transition` object.
#' @export
fit_multinomial_transition <- function(design, ridge = 1e-4, reference_state = NULL, control = list(maxit = 1000L, reltol = 1e-9)) {
  if (!inherits(design, "eye_transition_design")) .di_stop("Expected an `eye_transition_design`.")
  X <- design$X; y <- design$y; K <- length(design$states); D <- ncol(X)
  if (is.null(reference_state)) reference_state <- tail(design$states, 1L)
  reference <- match(reference_state, design$states)
  if (is.na(reference)) .di_stop("Unknown reference state: ", reference_state)
  weights <- design$data$state_probability
  observed_allowed <- design$allowed[cbind(seq_len(nrow(design$allowed)), y)]
  if (any(!observed_allowed)) .di_stop("Observed transitions violate the declared structural-transition mask.")
  initial <- rep(0, D * (K - 1L))
  fit <- stats::optim(initial, .di_multinomial_objective, X = X, y = y, allowed = design$allowed, weights = weights, ridge = ridge, reference = reference, method = "BFGS", hessian = TRUE, control = control)
  B <- .di_multinomial_unpack(fit$par, D, K, reference)
  dimnames(B) <- list(colnames(X), design$states)
  covariance <- tryCatch(solve(fit$hessian), error = function(e) matrix(NA_real_, length(fit$par), length(fit$par)))
  se_vector <- sqrt(pmax(diag(covariance), 0))
  SE <- matrix(0, nrow = D, ncol = K, dimnames = dimnames(B))
  SE[, setdiff(seq_len(K), reference)] <- matrix(se_vector, nrow = D)
  eta <- X %*% B
  eta[!design$allowed] <- -1e12
  probabilities <- .di_softmax(eta)
  colnames(probabilities) <- design$states
  coefficients <- data.frame(
    term = rep(rownames(B), times = K),
    destination = rep(colnames(B), each = D),
    estimate = as.vector(B),
    std_error = as.vector(SE),
    stringsAsFactors = FALSE
  )
  coefficients$z <- coefficients$estimate / coefficients$std_error
  coefficients$p_value <- 2 * stats::pnorm(abs(coefficients$z), lower.tail = FALSE)
  out <- list(
    design = design,
    coefficients = coefficients,
    coefficient_matrix = B,
    standard_error_matrix = SE,
    probabilities = probabilities,
    fitted_state = factor(design$states[max.col(probabilities, ties.method = "first")], levels = design$states),
    log_likelihood = -fit$value + 0.5 * ridge * sum(fit$par^2),
    convergence = fit$convergence,
    message = fit$message,
    iterations = fit$counts,
    reference_state = reference_state,
    ridge = ridge,
    optimizer = fit
  )
  class(out) <- "eye_multinomial_transition"
  out
}

#' @export
print.eye_multinomial_transition <- function(x, ...) {
  cat("Penalized multinomial transition model\n")
  cat("Transitions: ", nrow(x$design$X), "\n", sep = "")
  cat("States:      ", length(x$design$states), "\n", sep = "")
  cat("Convergence: ", x$convergence, "\n", sep = "")
  invisible(x)
}

#' Predict destination-state probabilities
#'
#' @param object Multinomial transition model.
#' @param newdata Optional prepared transition data.
#' @param type Probability, class, or linear predictor.
#' @param ... Unused.
#' @export
predict.eye_multinomial_transition <- function(object, newdata = NULL, type = c("probability", "class", "link"), ...) {
  type <- match.arg(type)
  if (is.null(newdata)) {
    X <- object$design$X
    allowed <- object$design$allowed
  } else {
    prediction_spec <- object$design$spec
    prediction_spec$standardize <- FALSE
    design <- dynamic_transition_design(newdata, spec = prediction_spec, formula = object$design$formula)
    X <- design$X
    missing <- setdiff(rownames(object$coefficient_matrix), colnames(X))
    if (length(missing)) .di_stop("New data are missing design columns: ", paste(missing, collapse = ", "))
    X <- X[, rownames(object$coefficient_matrix), drop = FALSE]
    scaling <- object$design$scaling
    if (is.data.frame(scaling) && nrow(scaling)) {
      for (i in seq_len(nrow(scaling))) if (scaling$term[i] %in% colnames(X)) X[, scaling$term[i]] <- (X[, scaling$term[i]] - scaling$center[i]) / scaling$scale[i]
    }
    allowed <- design$allowed
  }
  eta <- X %*% object$coefficient_matrix
  eta[!allowed] <- -1e12
  if (type == "link") return(eta)
  probability <- .di_softmax(eta)
  colnames(probability) <- colnames(object$coefficient_matrix)
  if (type == "probability") return(probability)
  factor(colnames(probability)[max.col(probability, ties.method = "first")], levels = colnames(probability))
}

.di_stan_file <- function(name) {
  path <- system.file("stan", name, package = "eyeprocess")
  if (!nzchar(path) || !file.exists(path)) .di_stop("Bundled Stan program is unavailable: ", name)
  path
}

.di_cmdstan_diagnostics <- function(fit) {
  summary <- fit$summary()
  sampler <- tryCatch(fit$diagnostic_summary(), error = function(e) NULL)
  divergences <- if (!is.null(sampler) && "num_divergent" %in% names(sampler)) sum(sampler$num_divergent) else NA_integer_
  data.frame(
    converged = all(summary$rhat[is.finite(summary$rhat)] <= 1.05),
    divergences = divergences,
    max_rhat = if (any(is.finite(summary$rhat))) max(summary$rhat, na.rm = TRUE) else NA_real_,
    min_ess_bulk = if (any(is.finite(summary$ess_bulk))) min(summary$ess_bulk, na.rm = TRUE) else NA_real_,
    min_ess_tail = if (any(is.finite(summary$ess_tail))) min(summary$ess_tail, na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

.di_stan_data_observed <- function(design, spec) {
  list(
    N = nrow(design$X),
    K = length(design$states),
    D = ncol(design$X),
    P = length(design$participants),
    J = length(design$items),
    X = unname(design$X),
    y = as.integer(design$y),
    person = as.integer(design$data$person_index),
    item = as.integer(design$data$item_index),
    from_state = as.integer(design$data$from_index),
    allowed = matrix(as.integer(design$allowed), nrow = nrow(design$allowed)),
    observation_weight = as.numeric(design$data$state_probability),
    use_person_re = as.integer(identical(spec$person_effect, "random")),
    use_item_re = as.integer(identical(spec$item_effect, "random"))
  )
}

.di_sequences_for_hmm <- function(data) {
  groups <- split(seq_len(nrow(data)), data$trial_index)
  order <- unlist(groups, use.names = FALSE)
  starts <- cumsum(c(1L, head(lengths(groups), -1L)))
  list(order = order, starts = starts, lengths = lengths(groups), sequences = length(groups))
}

.di_stan_data_hidden <- function(design, hidden_states, misclassification_matrix = NULL) {
  sequence <- .di_sequences_for_hmm(design$data)
  Kobs <- length(design$states)
  if (is.null(misclassification_matrix)) {
    misclassification_matrix <- matrix(1 / Kobs, nrow = hidden_states, ncol = Kobs)
    diagonal <- min(hidden_states, Kobs)
    for (k in seq_len(diagonal)) {
      misclassification_matrix[k, ] <- (1 - 0.90) / max(1, Kobs - 1)
      misclassification_matrix[k, k] <- 0.90
    }
  }
  misclassification_matrix <- as.matrix(misclassification_matrix)
  storage.mode(misclassification_matrix) <- "double"
  if (!all(dim(misclassification_matrix) == c(hidden_states, Kobs))) .di_stop("Misclassification matrix must have hidden-state rows and observed-state columns.")
  if (any(!is.finite(misclassification_matrix)) || any(misclassification_matrix < 0) || any(rowSums(misclassification_matrix) <= 0)) .di_stop("Misclassification probabilities must be finite, non-negative, and have positive row sums.")
  misclassification_matrix <- misclassification_matrix / rowSums(misclassification_matrix)
  hidden_labels <- paste0("state", seq_len(hidden_states))
  hidden_mask <- structural_transition_mask(hidden_labels, forbidden = design$spec$hidden_structural_zeros, allowed = design$spec$hidden_allowed_transitions)
  list(
    N = nrow(design$X),
    S = sequence$sequences,
    H = hidden_states,
    Kobs = Kobs,
    D = ncol(design$X),
    X = unname(design$X[sequence$order, , drop = FALSE]),
    observed = as.integer(design$y[sequence$order]),
    observation_certainty = as.numeric(design$data$state_probability[sequence$order]),
    sequence_start = as.integer(sequence$starts),
    sequence_length = as.integer(sequence$lengths),
    allowed_hidden = matrix(as.integer(hidden_mask), nrow = hidden_states),
    emission_prior = unname(misclassification_matrix)
  )
}

#' Fit an optional CmdStan dynamic-transition model
#'
#' @param design Transition design.
#' @param spec Dynamic IRTree specification.
#' @param seed Random seed.
#' @param refresh CmdStan refresh interval.
#' @param output_dir Optional CmdStan output directory.
#' @param ... Additional arguments to `CmdStanModel$sample()`.
#' @return An `eye_dynamic_irtree_stan` object.
#' @export
fit_dynamic_irtree_stan <- function(design, spec, seed = 1L, refresh = 0L, output_dir = NULL, ...) {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) .di_stop("The `cmdstanr` package is required for `engine = 'stan'`.")
  if (!inherits(design, "eye_transition_design")) .di_stop("Expected an `eye_transition_design`.")
  hidden <- spec$hidden_states >= 2L
  stan_file <- .di_stan_file(if (hidden) "dynamic_irtree_hidden.stan" else "dynamic_irtree_observed.stan")
  model <- cmdstanr::cmdstan_model(stan_file, quiet = TRUE)
  sequence_order <- if (hidden) .di_sequences_for_hmm(design$data)$order else seq_len(nrow(design$data))
  data <- if (hidden) .di_stan_data_hidden(design, spec$hidden_states, spec$misclassification_matrix) else .di_stan_data_observed(design, spec)
  fit <- model$sample(
    data = data,
    seed = as.integer(seed),
    chains = spec$chains,
    parallel_chains = spec$parallel_chains,
    iter_warmup = spec$iter_warmup,
    iter_sampling = spec$iter_sampling,
    adapt_delta = spec$adapt_delta,
    max_treedepth = spec$max_treedepth,
    refresh = refresh,
    output_dir = output_dir,
    ...
  )
  out <- list(
    design = design,
    spec = spec,
    fit = fit,
    summary = fit$summary(),
    diagnostics = .di_cmdstan_diagnostics(fit),
    hidden = hidden,
    sequence_order = as.integer(sequence_order),
    stan_file = stan_file
  )
  class(out) <- "eye_dynamic_irtree_stan"
  out
}

#' Decode latent or fitted transition states
#'
#' @param object Dynamic IRTree fit.
#' @param method Marginal probabilities, posterior mode, or draw.
#' @return Data frame of decoded states/probabilities.
#' @export
decode_dynamic_states <- function(object, method = c("mode", "probability", "draw")) {
  method <- match.arg(method)
  if (inherits(object$model, "eye_multinomial_transition")) {
    probability <- object$model$probabilities
    if (method == "probability") {
      out <- as.data.frame(probability, stringsAsFactors = FALSE)
      out$transition <- seq_len(nrow(out))
      return(out)
    }
    state <- colnames(probability)[max.col(probability, ties.method = if (method == "draw") "random" else "first")]
    return(data.frame(transition = seq_along(state), decoded_state = state, stringsAsFactors = FALSE))
  }
  if (inherits(object$model, "eye_dynamic_irtree_stan")) {
    if (!isTRUE(object$model$hidden)) .di_stop("Latent-state decoding applies only to hidden-state fits.")
    summary <- object$model$fit$summary(variables = "filtered_probability")
    index <- regmatches(summary$variable, gregexpr("[0-9]+", summary$variable))
    index <- do.call(rbind, lapply(index, as.integer))
    if (is.null(index) || ncol(index) < 2L) .di_stop("Could not parse filtered-state probability indices.")
    probability <- matrix(NA_real_, nrow = max(index[, 1L]), ncol = max(index[, 2L]))
    probability[cbind(index[, 1L], index[, 2L])] <- summary$mean
    if (length(object$model$sequence_order) == nrow(probability)) {
      original <- probability
      probability[object$model$sequence_order, ] <- original
    }
    colnames(probability) <- paste0("state", seq_len(ncol(probability)))
    if (method == "probability") {
      out <- as.data.frame(probability, stringsAsFactors = FALSE)
      out$transition <- seq_len(nrow(out))
      return(out)
    }
    if (method == "draw") {
      draws <- object$model$fit$draws(variables = "filtered_probability", format = "matrix")
      selected <- draws[sample.int(nrow(draws), 1L), , drop = TRUE]
      sampled <- matrix(NA_real_, nrow = nrow(probability), ncol = ncol(probability))
      sampled[cbind(index[, 1L], index[, 2L])] <- selected
      if (length(object$model$sequence_order) == nrow(sampled)) {
        original_sampled <- sampled
        sampled[object$model$sequence_order, ] <- original_sampled
      }
      state <- apply(sampled, 1L, function(p) sample.int(length(p), 1L, prob = p))
    } else {
      state <- max.col(probability, ties.method = "first")
    }
    data.frame(
      transition = seq_along(state),
      decoded_state = colnames(probability)[state],
      posterior_probability = probability[cbind(seq_along(state), state)],
      stringsAsFactors = FALSE
    )
  }
  .di_stop("State decoding is unavailable for this engine.")
}

#' Posterior predictive checks for dynamic state models
#'
#' @param object Dynamic IRTree fit using the Stan engine.
#' @param draws Maximum posterior predictive draws to summarize.
#' @param seed Seed used when subsampling posterior draws.
#' @return An `eye_dynamic_ppc` object with observed and replicated state frequencies.
#' @export
dynamic_posterior_predictive_check <- function(object, draws = 200L, seed = 1L) {
  if (!inherits(object, "eye_dynamic_irtree") || !inherits(object$model, "eye_dynamic_irtree_stan")) {
    .di_stop("Posterior predictive checks require a Stan dynamic IRTree fit.")
  }
  draws <- as.integer(draws)
  if (length(draws) != 1L || is.na(draws) || draws < 1L) .di_stop("`draws` must be a positive integer.")
  variable <- if (isTRUE(object$model$hidden)) "observed_rep" else "y_rep"
  matrix_draws <- object$model$fit$draws(variables = variable, format = "matrix")
  if (!nrow(matrix_draws) || !ncol(matrix_draws)) .di_stop("Posterior predictive draws are unavailable.")
  set.seed(as.integer(seed))
  if (nrow(matrix_draws) > draws) matrix_draws <- matrix_draws[sample.int(nrow(matrix_draws), draws), , drop = FALSE]
  variable_index <- suppressWarnings(as.integer(sub(sprintf("^%s\\[([0-9]+)\\]$", variable), "\\1", colnames(matrix_draws))))
  if (anyNA(variable_index)) .di_stop("Could not parse posterior predictive variable indices.")
  matrix_draws <- matrix_draws[, order(variable_index), drop = FALSE]
  if (isTRUE(object$model$hidden) && length(object$model$sequence_order) == ncol(matrix_draws)) {
    original <- matrix_draws
    matrix_draws[, object$model$sequence_order] <- original
  }
  observed <- as.integer(object$design$y)
  states <- object$states
  observed_frequency <- tabulate(observed, nbins = length(states)) / length(observed)
  replicated_frequency <- t(apply(matrix_draws, 1L, function(z) tabulate(as.integer(z), nbins = length(states)) / length(z)))
  colnames(replicated_frequency) <- states
  summary <- data.frame(
    state = states,
    observed = observed_frequency,
    replicated_mean = colMeans(replicated_frequency),
    replicated_lower = apply(replicated_frequency, 2L, stats::quantile, probs = 0.025, na.rm = TRUE),
    replicated_upper = apply(replicated_frequency, 2L, stats::quantile, probs = 0.975, na.rm = TRUE),
    posterior_predictive_p = colMeans(sweep(replicated_frequency, 2L, observed_frequency, ">=")),
    stringsAsFactors = FALSE
  )
  out <- list(summary = summary, replicated_frequency = replicated_frequency, observed_state = observed, engine = "stan", hidden = isTRUE(object$model$hidden))
  class(out) <- "eye_dynamic_ppc"
  out
}

#' @export
print.eye_dynamic_ppc <- function(x, ...) {
  cat("Dynamic-state posterior predictive check\n")
  cat("Hidden-state engine: ", x$hidden, "\n", sep = "")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_dynamic_ppc <- function(x, ...) {
  limits <- range(c(x$summary$observed, x$summary$replicated_lower, x$summary$replicated_upper), finite = TRUE)
  position <- seq_len(nrow(x$summary))
  graphics::plot(position, x$summary$replicated_mean, ylim = limits, xaxt = "n", xlab = "State", ylab = "Frequency", main = "Dynamic-state posterior predictive check", ...)
  graphics::segments(position, x$summary$replicated_lower, position, x$summary$replicated_upper)
  graphics::points(position, x$summary$observed, pch = 19)
  graphics::axis(1, at = position, labels = x$summary$state, las = 2)
  invisible(x$summary)
}

#' Compute transition residual diagnostics
#'
#' @param object Dynamic IRTree fit.
#' @param type Pearson, deviance, or randomized quantile residuals.
#' @return An `eye_transition_diagnostics` object.
#' @export
transition_residual_diagnostics <- function(object, type = c("pearson", "deviance", "randomized")) {
  type <- match.arg(type)
  if (!inherits(object, "eye_dynamic_irtree")) .di_stop("Expected an `eye_dynamic_irtree`.")
  d <- object$transitions
  if (inherits(object$model, "eye_multinomial_transition")) {
    p <- object$model$probabilities
    observed <- object$model$design$y
  } else if (identical(object$spec$engine, "baseline")) {
    states <- object$states
    p <- matrix(NA_real_, nrow = nrow(d), ncol = length(states), dimnames = list(NULL, states))
    for (state in names(object$fits)) p[, state] <- stats::predict(object$fits[[state]], type = "response")
    p[!is.finite(p)] <- 0
    p <- p / pmax(rowSums(p), .Machine$double.eps)
    observed <- match(as.character(d$to_state), states)
  } else {
    .di_stop("Residual diagnostics for Stan fits require posterior predictive draws; use `dynamic_posterior_predictive_check()`.")
  }
  observed_probability <- p[cbind(seq_len(nrow(p)), observed)]
  if (type == "pearson") {
    residual <- (1 - observed_probability) / sqrt(pmax(observed_probability * (1 - observed_probability), 1e-8))
  } else if (type == "deviance") {
    residual <- sign(1 - observed_probability) * sqrt(-2 * log(pmax(observed_probability, 1e-12)))
  } else {
    lower <- rowSums(p * (col(p) < observed))
    upper <- lower + observed_probability
    residual <- stats::qnorm(stats::runif(length(lower), lower, upper))
  }
  rows <- data.frame(
    transition = seq_len(nrow(d)),
    observed_state = as.character(d$to_state),
    fitted_probability = observed_probability,
    residual = residual,
    participant_id = d$participant_id,
    item_id = d$item_id,
    trial_id = d$trial_id,
    step = d$step,
    stringsAsFactors = FALSE
  )
  summary <- data.frame(
    n = nrow(rows),
    mean = mean(rows$residual, na.rm = TRUE),
    sd = stats::sd(rows$residual, na.rm = TRUE),
    rmse = sqrt(mean(rows$residual^2, na.rm = TRUE)),
    max_absolute = max(abs(rows$residual), na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  out <- list(type = type, residuals = rows, summary = summary)
  class(out) <- "eye_transition_diagnostics"
  out
}

#' @export
print.eye_transition_diagnostics <- function(x, ...) {
  cat("Transition residual diagnostics (", x$type, ")\n", sep = "")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_transition_diagnostics <- function(x, ...) {
  graphics::plot(x$residuals$fitted_probability, x$residuals$residual, xlab = "Fitted probability", ylab = "Residual", main = "Transition residuals", ...)
  graphics::abline(h = 0, lty = 2)
  invisible(x$residuals)
}

#' Compare dynamic transition models
#'
#' @param ... Named fitted dynamic IRTree models.
#' @param criterion AIC, BIC, log score, or classification accuracy.
#' @return An `eye_dynamic_model_comparison` data frame.
#' @export
compare_dynamic_transition_models <- function(..., criterion = c("AIC", "BIC", "log_score", "accuracy")) {
  models <- list(...)
  if (!length(models)) .di_stop("At least one model is required.")
  if (
    length(models) == 1L &&
    is.list(models[[1L]]) &&
    !inherits(models[[1L]], "eye_dynamic_irtree") &&
    length(models[[1L]]) &&
    all(vapply(models[[1L]], inherits, logical(1), what = "eye_dynamic_irtree"))
  ) {
    models <- models[[1L]]
  }
  if (is.null(names(models)) || any(!nzchar(names(models)))) names(models) <- paste0("model", seq_along(models))
  criterion <- match.arg(criterion)
  rows <- lapply(names(models), function(name) {
    object <- models[[name]]
    if (!inherits(object, "eye_dynamic_irtree")) .di_stop("All inputs must be dynamic IRTree fits.")
    n <- nrow(object$transitions)
    if (inherits(object$model, "eye_multinomial_transition")) {
      ll <- object$model$log_likelihood
      k <- sum(is.finite(object$model$coefficients$estimate) & object$model$coefficients$destination != object$model$reference_state)
      p <- object$model$probabilities
      y <- object$model$design$y
      accuracy <- mean(max.col(p) == y)
      log_score <- mean(log(pmax(p[cbind(seq_len(nrow(p)), y)], 1e-12)))
    } else if (identical(object$spec$engine, "baseline")) {
      ll <- sum(vapply(object$fits, function(fit) as.numeric(stats::logLik(fit)), numeric(1)))
      k <- sum(vapply(object$fits, function(fit) length(stats::coef(fit)), integer(1)))
      accuracy <- NA_real_; log_score <- NA_real_
    } else {
      ll <- NA_real_; k <- NA_integer_; accuracy <- NA_real_; log_score <- NA_real_
    }
    data.frame(model = name, engine = object$spec$engine, n = n, parameters = k, log_likelihood = ll, AIC = if (is.finite(ll)) -2 * ll + 2 * k else NA_real_, BIC = if (is.finite(ll)) -2 * ll + log(n) * k else NA_real_, log_score = log_score, accuracy = accuracy, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out$rank <- rank(if (criterion %in% c("AIC", "BIC")) out[[criterion]] else -out[[criterion]], na.last = "keep")
  class(out) <- c("eye_dynamic_model_comparison", "data.frame")
  out
}

#' Simulate observed dynamic-state transitions
#'
#' @param n_person,n_item Number of persons and items.
#' @param transitions_per_trial Number of transitions per person-item trial.
#' @param states State labels.
#' @param beta_response Response effect on transitions.
#' @param person_sd,item_sd Person/item heterogeneity.
#' @param irregular_time Whether to generate irregular time gaps.
#' @param state_misclassification Destination-state error probability.
#' @param missing_state Missing destination-state probability.
#' @param structural_zeros Optional forbidden transitions.
#' @param seed Random seed.
#' @return List with transitions and truth.
#' @export
simulate_dynamic_irtree_data <- function(
    n_person = 100L,
    n_item = 20L,
    transitions_per_trial = 8L,
    states = c("prompt", "evidence", "options"),
    beta_response = 0.5,
    person_sd = 0.4,
    item_sd = 0.3,
    irregular_time = TRUE,
    state_misclassification = 0,
    missing_state = 0,
    structural_zeros = NULL,
    seed = 1L) {
  n_person <- as.integer(n_person); n_item <- as.integer(n_item); transitions_per_trial <- as.integer(transitions_per_trial)
  if (length(n_person) != 1L || is.na(n_person) || length(n_item) != 1L || is.na(n_item) || length(transitions_per_trial) != 1L || is.na(transitions_per_trial) || n_person < 2L || n_item < 2L || transitions_per_trial < 2L) .di_stop("Simulation sizes are too small.")
  rates <- c(state_misclassification = state_misclassification, missing_state = missing_state)
  if (any(!is.finite(rates)) || any(rates < 0) || any(rates >= 1)) .di_stop("Misclassification and missing-state rates must lie in [0, 1).")
  scales <- c(person_sd = person_sd, item_sd = item_sd)
  if (any(!is.finite(scales)) || any(scales < 0)) .di_stop("Person and item heterogeneity SDs must be finite and non-negative.")
  states <- unique(as.character(states)); K <- length(states)
  if (K < 2L || anyNA(states) || any(!nzchar(states))) .di_stop("At least two non-empty state labels are required.")
  mask <- structural_transition_mask(states, structural_zeros)
  set.seed(seed)
  persons <- paste0("P", seq_len(n_person)); items <- paste0("I", seq_len(n_item))
  theta <- stats::rnorm(n_person); difficulty <- stats::rnorm(n_item)
  person_effect <- matrix(stats::rnorm(n_person * K, 0, person_sd), n_person, K)
  item_effect <- matrix(stats::rnorm(n_item * K, 0, item_sd), n_item, K)
  base <- matrix(stats::rnorm(K * K, 0, 0.4), K, K)
  for (from in seq_len(K)) base[from, from] <- base[from, from] + 0.8
  rows <- vector("list", n_person * n_item); k <- 0L
  for (p in seq_len(n_person)) for (i in seq_len(n_item)) {
    k <- k + 1L
    score <- stats::rbinom(1L, 1L, stats::plogis(theta[p] - difficulty[i]))
    from <- sample(seq_len(K), 1L)
    time <- 0
    trial <- vector("list", transitions_per_trial)
    for (step in seq_len(transitions_per_trial)) {
      eta <- base[from, ] + person_effect[p, ] + item_effect[i, ] + beta_response * score * seq(-0.5, 0.5, length.out = K)
      eta[!mask[from, ]] <- -1e12
      probability <- .di_softmax(eta)
      destination <- sample(seq_len(K), 1L, prob = probability)
      gap <- if (isTRUE(irregular_time)) stats::rexp(1L, rate = 2) else 1
      observed <- if (stats::runif(1L) < state_misclassification) sample(setdiff(seq_len(K), destination), 1L) else destination
      observed_label <- states[observed]
      if (stats::runif(1L) < missing_state) observed_label <- NA_character_
      trial[[step]] <- data.frame(
        participant_id = persons[p], item_id = items[i], trial_id = paste(persons[p], items[i], sep = "-"),
        step = step, time = time, time_gap = gap, from_state = states[from], true_to_state = states[destination],
        to_state = observed_label, score = score, stringsAsFactors = FALSE
      )
      time <- time + gap; from <- destination
    }
    rows[[k]] <- do.call(rbind, trial)
  }
  list(
    transitions = do.call(rbind, rows),
    truth = list(base_transition = base, beta_response = beta_response, person_sd = person_sd, item_sd = item_sd, state_misclassification = state_misclassification, missing_state = missing_state, states = states, mask = mask)
  )
}

#' Evaluate dynamic-state recovery under misclassification
#'
#' @param grid Scenario grid.
#' @param replications Replications per scenario.
#' @param spec Dynamic IRTree specification.
#' @param base_seed Base seed.
#' @return An `eye_dynamic_recovery` object.
#' @export
dynamic_irtree_recovery <- function(grid = expand.grid(state_misclassification = c(0, 0.05, 0.15), missing_state = c(0, 0.10), stringsAsFactors = FALSE), replications = 20L, spec = dynamic_irtree_spec(engine = "multinomial"), base_seed = 1L) {
  plan <- validation_job_plan(grid, replications, base_seed, model_family = "dynamic_irtree")
  simulator <- function(state_misclassification, missing_state, seed, ...) simulate_dynamic_irtree_data(state_misclassification = state_misclassification, missing_state = missing_state, seed = seed)
  fitter <- function(simulation) fit_dynamic_irtree(simulation$transitions, spec = spec)
  extractor <- function(fit) {
    if (!inherits(fit$model, "eye_multinomial_transition")) return(data.frame(parameter = "classification_accuracy", estimate = NA_real_))
    data.frame(parameter = "classification_accuracy", estimate = mean(as.character(fit$model$fitted_state) == as.character(fit$model$design$data$true_to_state), na.rm = TRUE), stringsAsFactors = FALSE)
  }
  truth <- function(simulation) c(classification_accuracy = 1 - simulation$truth$state_misclassification)
  out <- list(plan = plan, simulator = simulator, fitter = fitter, extractor = extractor, truth_extractor = truth, spec = spec)
  class(out) <- "eye_dynamic_recovery"
  out
}

#' @export
print.eye_dynamic_recovery <- function(x, ...) {
  cat("Dynamic IRTree recovery programme\n")
  cat("Plan:         ", x$plan$plan_id, "\n", sep = "")
  cat("Jobs:         ", nrow(x$plan$jobs), "\n", sep = "")
  cat("Engine:       ", x$spec$engine, "\n", sep = "")
  cat("Evidence use: recovery under state misclassification; not automatic construct validation\n")
  invisible(x)
}

#' Fit a dynamic gaze-state response-tree model
#'
#' @param x An `eye_dataset` or transition/long-state data frame.
#' @param spec Dynamic IRTree specification.
#' @param min_transitions Minimum support per destination for baseline logits.
#' @param seed Random seed for probabilistic engines.
#' @param ... Engine-specific arguments.
#' @return An `eye_dynamic_irtree` object.
#' @export
fit_dynamic_irtree <- function(x, spec = dynamic_irtree_spec(), min_transitions = 10L, seed = 1L, ...) {
  if (!inherits(spec, "eye_dynamic_irtree_spec")) .di_stop("`spec` must be created by `dynamic_irtree_spec()`.")
  d <- prepare_dynamic_irtree_data(x, spec)
  design <- dynamic_transition_design(d, spec)
  states <- design$states
  if (spec$engine == "baseline") {
    fits <- setNames(vector("list", length(states)), states)
    fit_warnings <- setNames(vector("list", length(states)), states)
    coefficients <- list()
    baseline_data <- design$data
    for (state in states) {
      z <- baseline_data
      z$.destination <- as.integer(z$to_state == state)
      if (sum(z$.destination) < min_transitions || sum(1L - z$.destination) < min_transitions) next
      terms <- attr(stats::terms(design$formula), "term.labels")
      formula <- stats::reformulate(terms, response = ".destination")
      warning_text <- character()
      fits[[state]] <- withCallingHandlers(stats::glm(formula, family = stats::binomial(), data = z, weights = z$state_probability), warning = function(w) {
        warning_text <<- c(warning_text, conditionMessage(w)); invokeRestart("muffleWarning")
      })
      fit_warnings[[state]] <- unique(warning_text)
      co <- stats::coef(summary(fits[[state]]))
      coefficients[[state]] <- data.frame(destination = state, term = rownames(co), estimate = co[, 1L], std_error = co[, 2L], statistic = co[, 3L], p_value = co[, 4L], row.names = NULL)
    }
    keep <- !vapply(fits, is.null, logical(1)); fits <- fits[keep]; fit_warnings <- fit_warnings[keep]
    if (!length(fits)) .di_stop("No destination state had sufficient transition support.")
    model <- NULL
    coefficients <- do.call(rbind, coefficients)
    diagnostics <- data.frame(converged = TRUE, models = length(fits), stringsAsFactors = FALSE)
  } else if (spec$engine == "multinomial") {
    model <- fit_multinomial_transition(design, ridge = spec$ridge, reference_state = spec$reference_state, ...)
    fits <- list(); fit_warnings <- list(); coefficients <- model$coefficients
    diagnostics <- data.frame(converged = model$convergence == 0L, optimizer_code = model$convergence, log_likelihood = model$log_likelihood, stringsAsFactors = FALSE)
  } else {
    model <- fit_dynamic_irtree_stan(design, spec, seed = seed, ...)
    fits <- list(); fit_warnings <- list(); coefficients <- model$summary
    diagnostics <- model$diagnostics
  }
  out <- list(
    spec = spec,
    transitions = design$data,
    design = design,
    states = states,
    model = model,
    fits = fits,
    coefficients = coefficients,
    fit_warnings = fit_warnings,
    diagnostics = diagnostics,
    warning = "Observed AOI states, inferred hidden states, and transition parameters are not named cognitive states without external experimental validation."
  )
  class(out) <- "eye_dynamic_irtree"
  out
}

#' @export
print.eye_dynamic_irtree <- function(x, ...) {
  cat("Dynamic gaze-state IRTree\n")
  cat("Engine:      ", x$spec$engine, if (x$spec$hidden_states >= 2L) paste0(" (", x$spec$hidden_states, " hidden states)") else "", "\n", sep = "")
  cat("Transitions: ", nrow(x$transitions), "\n", sep = "")
  cat("States:      ", paste(x$states, collapse = ", "), "\n", sep = "")
  if (nrow(x$diagnostics)) print(x$diagnostics, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_dynamic_irtree <- function(x, type = c("observed", "fitted", "residual"), ...) {
  type <- match.arg(type)
  if (type == "observed") {
    tab <- table(x$transitions$from_state, x$transitions$to_state)
    graphics::image(t(tab[nrow(tab):1, , drop = FALSE]), axes = FALSE, main = "Observed transition counts", ...)
    graphics::axis(1, at = seq(0, 1, length.out = ncol(tab)), labels = colnames(tab), las = 2)
    graphics::axis(2, at = seq(0, 1, length.out = nrow(tab)), labels = rev(rownames(tab)), las = 2)
    return(invisible(tab))
  }
  if (type == "fitted" && inherits(x$model, "eye_multinomial_transition")) {
    p <- x$model$probabilities
    graphics::matplot(p, type = "l", lty = 1, xlab = "Transition", ylab = "Destination probability", main = "Fitted transition probabilities", ...)
    graphics::legend("topright", legend = colnames(p), lty = seq_len(ncol(p)), cex = 0.7)
    return(invisible(p))
  }
  diagnostics <- transition_residual_diagnostics(x)
  plot(diagnostics, ...)
}
