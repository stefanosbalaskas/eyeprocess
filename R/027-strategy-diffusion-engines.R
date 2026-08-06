# Strategy-mixture and gaze-informed diffusion engines -----------------------

.sd_stop <- function(message, class = "eyeprocess_model_error") {
  condition <- structure(list(message = message, call = NULL), class = c(class, "error", "condition"))
  stop(condition)
}

.sd_match_columns <- function(data, columns, object = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) .sd_stop(sprintf("Missing required columns in `%s`: %s.", object, paste(missing, collapse = ", ")))
  invisible(data)
}

.sd_softmax <- function(eta) {
  eta <- as.matrix(eta)
  eta <- sweep(eta, 1L, apply(eta, 1L, max), "-")
  value <- exp(eta)
  value / rowSums(value)
}

.sd_entropy <- function(probability) {
  probability <- as.matrix(probability)
  -rowSums(ifelse(probability > 0, probability * log(probability), 0))
}

.sd_cmdstan_file <- function(name) {
  path <- system.file("stan", name, package = "eyeprocess")
  if (!nzchar(path)) {
    candidate <- file.path("inst", "stan", name)
    if (file.exists(candidate)) path <- normalizePath(candidate, winslash = "/", mustWork = TRUE)
  }
  if (!nzchar(path) || !file.exists(path)) .sd_stop(sprintf("Bundled Stan program `%s` was not found.", name))
  path
}

.sd_cmdstan_diagnostics <- function(fit) {
  summary <- fit$summary()
  diagnostic <- tryCatch(fit$diagnostic_summary(), error = function(e) NULL)
  data.frame(
    converged = if (any(is.finite(summary$rhat))) all(summary$rhat[is.finite(summary$rhat)] <= 1.05) else NA,
    divergences = if (!is.null(diagnostic) && "num_divergent" %in% names(diagnostic)) sum(diagnostic$num_divergent) else NA_integer_,
    max_rhat = if (any(is.finite(summary$rhat))) max(summary$rhat, na.rm = TRUE) else NA_real_,
    min_ess_bulk = if (any(is.finite(summary$ess_bulk))) min(summary$ess_bulk, na.rm = TRUE) else NA_real_,
    min_ess_tail = if (any(is.finite(summary$ess_tail))) min(summary$ess_tail, na.rm = TRUE) else NA_real_,
    stringsAsFactors = FALSE
  )
}

#' Define a theory-constrained strategy-mixture model
#'
#' @param strategies Named list of strategy signatures. Each signature is a named
#'   numeric vector over process features. Names are substantive labels and must
#'   be supplied before estimation.
#' @param feature_columns Process-feature columns.
#' @param response Binary response column.
#' @param participant Participant identifier.
#' @param item Item identifier.
#' @param condition Optional condition column.
#' @param item_availability Optional item-by-strategy availability matrix or data frame.
#' @param engine Estimation engine.
#' @param multiple_starts Number of starts for the EM baseline.
#' @param anchor_strength Prior/penalty strength anchoring classes to signatures.
#' @param chains,parallel_chains,iter_warmup,iter_sampling,adapt_delta,max_treedepth Stan controls.
#' @return An `eye_theory_strategy_spec`.
#' @export
theory_strategy_spec <- function(
  strategies = NULL,
  feature_columns = NULL,
  response = "score",
  participant = "participant_id",
  item = "item_id",
  condition = NULL,
  item_availability = NULL,
  engine = c("em", "stan"),
  multiple_starts = 10L,
  anchor_strength = 3,
  chains = 4L,
  parallel_chains = min(4L, chains),
  iter_warmup = 1000L,
  iter_sampling = 1000L,
  adapt_delta = 0.95,
  max_treedepth = 12L,
  prototypes = NULL,
  feature_sd = NULL,
  prior = NULL
) {
  engine <- match.arg(engine)
  if (is.null(strategies) && !is.null(prototypes)) strategies <- prototypes
  legacy_mode <- is.matrix(strategies) || is.data.frame(strategies)
  if (legacy_mode) {
    prototypes <- as.matrix(strategies)
    if (nrow(prototypes) < 2L || ncol(prototypes) < 1L || !is.numeric(prototypes) || any(!is.finite(prototypes))) {
      .sd_stop("`prototypes` must contain at least two strategies and one finite numeric feature.")
    }
    if (is.null(rownames(prototypes))) rownames(prototypes) <- paste0("strategy_", seq_len(nrow(prototypes)))
    if (is.null(colnames(prototypes)) || any(!nzchar(colnames(prototypes)))) .sd_stop("Strategy prototypes must have non-empty feature column names.")
    if (anyDuplicated(rownames(prototypes)) || anyDuplicated(colnames(prototypes))) .sd_stop("Strategy and feature names must be unique.")
    if (is.null(feature_sd)) feature_sd <- rep(1, ncol(prototypes))
    if (length(feature_sd) == 1L) feature_sd <- rep(feature_sd, ncol(prototypes))
    if (length(feature_sd) != ncol(prototypes) || any(!is.finite(feature_sd)) || any(feature_sd <= 0)) .sd_stop("`feature_sd` must contain positive finite values matching the prototype features.")
    if (is.null(prior)) prior <- rep(1 / nrow(prototypes), nrow(prototypes))
    if (length(prior) != nrow(prototypes) || any(!is.finite(prior)) || any(prior <= 0)) .sd_stop("Strategy priors must be positive finite values matching the number of prototypes.")
    prior <- prior / sum(prior)
    strategies <- lapply(seq_len(nrow(prototypes)), function(i) prototypes[i, ])
    names(strategies) <- rownames(prototypes)
    feature_columns <- colnames(prototypes)
  } else {
    if (!is.list(strategies) || is.null(names(strategies)) || any(!nzchar(names(strategies))) || anyDuplicated(names(strategies))) {
      .sd_stop("`strategies` must be a uniquely named list of theory-defined signatures or a prototype matrix.")
    }
    if (length(strategies) < 2L) .sd_stop("At least two prespecified strategies are required.")
    raw_names <- unique(unlist(lapply(strategies, names), use.names = FALSE))
    if (is.null(feature_columns)) feature_columns <- raw_names
    prototypes <- matrix(0, nrow = length(strategies), ncol = length(feature_columns), dimnames = list(names(strategies), feature_columns))
    for (k in seq_along(strategies)) {
      signature <- strategies[[k]]
      if (is.null(names(signature))) .sd_stop("Every strategy signature must be a named numeric vector.")
      unknown <- setdiff(names(signature), feature_columns)
      if (length(unknown)) .sd_stop(sprintf("Unknown signature features: %s.", paste(unknown, collapse = ", ")))
      prototypes[k, names(signature)] <- as.numeric(signature)
    }
    feature_sd <- rep(1, ncol(prototypes))
    prior <- rep(1 / nrow(prototypes), nrow(prototypes))
  }
  multiple_starts <- suppressWarnings(as.integer(multiple_starts))
  if (length(multiple_starts) != 1L || is.na(multiple_starts) || multiple_starts < 1L) .sd_stop("`multiple_starts` must be a positive integer.")
  anchor_strength <- suppressWarnings(as.numeric(anchor_strength))
  if (length(anchor_strength) != 1L || !is.finite(anchor_strength) || anchor_strength < 0) .sd_stop("`anchor_strength` must be finite and non-negative.")
  signature_matrix <- prototypes
  norms <- sqrt(rowSums(signature_matrix^2))
  if (any(norms == 0)) .sd_stop("Every strategy signature must contain at least one non-zero anchor.")
  signature_matrix <- signature_matrix / norms
  out <- list(
    strategies = rownames(prototypes), signatures = signature_matrix, prototypes = prototypes,
    feature_sd = as.numeric(feature_sd), prior = as.numeric(prior), legacy_mode = legacy_mode,
    feature_columns = colnames(prototypes), response = response, participant = participant,
    item = item, condition = condition, item_availability = item_availability,
    engine = engine, multiple_starts = multiple_starts,
    anchor_strength = anchor_strength, chains = as.integer(chains),
    parallel_chains = as.integer(parallel_chains), iter_warmup = as.integer(iter_warmup),
    iter_sampling = as.integer(iter_sampling), adapt_delta = adapt_delta,
    max_treedepth = as.integer(max_treedepth),
    interpretation = "Strategy labels are prespecified theoretical hypotheses; estimated classes are not automatically cognitive strategies."
  )
  class(out) <- "eye_theory_strategy_spec"
  out
}
#' @export
print.eye_theory_strategy_spec <- function(x, ...) {
  cat("Theory-constrained strategy mixture\n")
  cat("Strategies: ", paste(x$strategies, collapse = ", "), "\n", sep = "")
  cat("Features:   ", paste(x$feature_columns, collapse = ", "), "\n", sep = "")
  cat("Engine:     ", x$engine, "\n", sep = "")
  cat("Safeguard:  ", x$interpretation, "\n", sep = "")
  invisible(x)
}

.sd_strategy_availability <- function(data, spec) {
  K <- length(spec$strategies)
  J <- length(unique(data[[spec$item]]))
  item_levels <- unique(as.character(data[[spec$item]]))
  availability <- matrix(1L, nrow = J, ncol = K, dimnames = list(item_levels, spec$strategies))
  supplied <- spec$item_availability
  if (is.null(supplied)) return(availability)
  if (is.matrix(supplied)) {
    if (is.null(rownames(supplied)) || is.null(colnames(supplied))) .sd_stop("Item-availability matrices need item row names and strategy column names.")
    common_item <- intersect(rownames(supplied), rownames(availability))
    common_strategy <- intersect(colnames(supplied), colnames(availability))
    availability[common_item, common_strategy] <- as.integer(supplied[common_item, common_strategy, drop = FALSE] != 0)
  } else if (is.data.frame(supplied)) {
    .sd_match_columns(supplied, c("item_id", "strategy", "available"), "item_availability")
    for (i in seq_len(nrow(supplied))) {
      item <- as.character(supplied$item_id[i]); strategy <- as.character(supplied$strategy[i])
      if (item %in% rownames(availability) && strategy %in% colnames(availability)) availability[item, strategy] <- as.integer(isTRUE(supplied$available[i]))
    }
  } else .sd_stop("`item_availability` must be NULL, a matrix, or a data frame.")
  if (any(rowSums(availability) == 0L)) .sd_stop("Every item must permit at least one strategy.")
  availability
}

#' Prepare data for a strategy-mixture model
#'
#' @param data Trial-level data.
#' @param spec Strategy specification.
#' @param standardize Whether to standardize process features.
#' @return An `eye_strategy_data` object.
#' @export
prepare_strategy_mixture_data <- function(data, spec, standardize = TRUE) {
  if (!inherits(spec, "eye_theory_strategy_spec")) .sd_stop("`spec` must be created by `theory_strategy_spec()`.")
  if (!is.data.frame(data)) .sd_stop("`data` must be a data frame.")
  required <- c(spec$participant, spec$item, spec$response, spec$feature_columns, spec$condition)
  .sd_match_columns(data, required[!is.na(required) & nzchar(required)])
  keep <- stats::complete.cases(data[c(spec$participant, spec$item, spec$response, spec$feature_columns)])
  d <- data[keep, , drop = FALSE]
  if (!nrow(d)) .sd_stop("No complete strategy-mixture rows remain.")
  y <- as.integer(d[[spec$response]])
  if (!all(y %in% 0:1)) .sd_stop("Strategy-mixture responses must be coded 0/1.")
  X <- as.matrix(d[spec$feature_columns])
  storage.mode(X) <- "double"
  center <- rep(0, ncol(X)); scale <- rep(1, ncol(X))
  if (standardize) {
    center <- colMeans(X)
    scale <- apply(X, 2L, stats::sd)
    scale[!is.finite(scale) | scale == 0] <- 1
    X <- sweep(sweep(X, 2L, center, "-"), 2L, scale, "/")
  }
  participant_levels <- unique(as.character(d[[spec$participant]]))
  item_levels <- unique(as.character(d[[spec$item]]))
  condition_levels <- if (!is.null(spec$condition)) unique(as.character(d[[spec$condition]])) else "all"
  availability_item <- .sd_strategy_availability(d, spec)
  availability <- availability_item[match(as.character(d[[spec$item]]), rownames(availability_item)), , drop = FALSE]
  out <- list(
    data = d, X = X, y = y, signatures = spec$signatures,
    participant = match(as.character(d[[spec$participant]]), participant_levels),
    item = match(as.character(d[[spec$item]]), item_levels),
    condition = if (!is.null(spec$condition)) match(as.character(d[[spec$condition]]), condition_levels) else rep(1L, nrow(d)),
    participant_levels = participant_levels, item_levels = item_levels,
    condition_levels = condition_levels, availability = availability,
    availability_item = availability_item, center = center, scale = scale, spec = spec
  )
  class(out) <- "eye_strategy_data"
  out
}

#' @export
print.eye_strategy_data <- function(x, ...) {
  cat("Prepared strategy-mixture data\n")
  cat("Trials:       ", nrow(x$X), "\n", sep = "")
  cat("Participants: ", length(x$participant_levels), "\n", sep = "")
  cat("Items:        ", length(x$item_levels), "\n", sep = "")
  cat("Strategies:   ", nrow(x$signatures), "\n", sep = "")
  invisible(x)
}

.sd_strategy_loglik <- function(prepared, means, covariance, intercept, ability, difficulty, mixing) {
  N <- nrow(prepared$X); K <- nrow(means); F <- ncol(prepared$X)
  log_component <- matrix(-Inf, N, K)
  for (k in seq_len(K)) {
    difference <- sweep(prepared$X, 2L, means[k, ], "-")
    process_ll <- -0.5 * rowSums(sweep(difference^2, 2L, covariance[k, ], "/") + log(2 * pi * covariance[k, ]))
    eta <- intercept[k] + ability[prepared$participant] - difficulty[prepared$item]
    response_ll <- stats::dbinom(prepared$y, 1L, stats::plogis(eta), log = TRUE)
    prior <- log(pmax(mixing[k], 1e-12))
    log_component[, k] <- prior + process_ll + response_ll
    log_component[prepared$availability[, k] == 0L, k] <- -Inf
  }
  maximum <- apply(log_component, 1L, max)
  normalizer <- maximum + log(rowSums(exp(log_component - maximum)))
  probability <- exp(log_component - normalizer)
  list(log_likelihood = sum(normalizer), probability = probability)
}

#' Fit the deterministic multi-start EM baseline
#'
#' @param prepared Prepared strategy data.
#' @param starts Number of starts.
#' @param max_iter Maximum EM iterations.
#' @param tolerance Relative log-likelihood tolerance.
#' @param seed Seed.
#' @return An `eye_strategy_mixture_em` object.
#' @export
fit_strategy_mixture_em <- function(prepared, starts = prepared$spec$multiple_starts, max_iter = 300L, tolerance = 1e-7, seed = 1L) {
  if (!inherits(prepared, "eye_strategy_data")) .sd_stop("Expected prepared strategy-mixture data.")
  set.seed(seed)
  N <- nrow(prepared$X); K <- nrow(prepared$signatures); F <- ncol(prepared$X)
  P <- length(prepared$participant_levels); J <- length(prepared$item_levels)
  result <- vector("list", starts)
  for (start in seq_len(starts)) {
    means <- prepared$signatures * prepared$spec$anchor_strength + matrix(stats::rnorm(K * F, sd = 0.15), K, F)
    covariance <- matrix(1, K, F)
    intercept <- stats::rnorm(K, sd = 0.25)
    ability <- rep(0, P); difficulty <- rep(0, J); mixing <- rep(1 / K, K)
    history <- numeric(max_iter)
    converged <- FALSE
    for (iteration in seq_len(max_iter)) {
      e <- .sd_strategy_loglik(prepared, means, covariance, intercept, ability, difficulty, mixing)
      probability <- e$probability
      history[iteration] <- e$log_likelihood
      weight <- colSums(probability)
      mixing <- weight / sum(weight)
      for (k in seq_len(K)) {
        if (weight[k] <= 1e-8) next
        empirical <- colSums(prepared$X * probability[, k]) / weight[k]
        means[k, ] <- (empirical * weight[k] + prepared$signatures[k, ] * prepared$spec$anchor_strength) / (weight[k] + prepared$spec$anchor_strength)
        difference <- sweep(prepared$X, 2L, means[k, ], "-")
        covariance[k, ] <- pmax(colSums(difference^2 * probability[, k]) / weight[k], 0.05)
      }
      # Weighted logistic update with alternating person/item offsets.
      expanded <- do.call(rbind, lapply(seq_len(K), function(k) data.frame(
        y = prepared$y, strategy = factor(k), person = factor(prepared$participant),
        item = factor(prepared$item), weight = probability[, k]
      )))
      logistic <- tryCatch(
        suppressWarnings(stats::glm(y ~ strategy + person + item - 1, family = stats::binomial(), weights = weight, data = expanded)),
        error = function(e) NULL
      )
      if (!is.null(logistic)) {
        coefficient <- stats::coef(logistic)
        for (k in seq_len(K)) {
          value <- coefficient[paste0("strategy", k)]
          if (length(value) && is.finite(value)) intercept[k] <- value
        }
        for (p in seq_len(P)) {
          value <- coefficient[paste0("person", p)]
          if (length(value) && is.finite(value)) ability[p] <- value
        }
        for (j in seq_len(J)) {
          value <- coefficient[paste0("item", j)]
          if (length(value) && is.finite(value)) difficulty[j] <- -value
        }
        ability <- ability - mean(ability); difficulty <- difficulty - mean(difficulty)
      }
      if (iteration > 1L && abs(history[iteration] - history[iteration - 1L]) <= tolerance * (1 + abs(history[iteration - 1L]))) {
        converged <- TRUE; break
      }
    }
    used <- seq_len(iteration)
    final <- .sd_strategy_loglik(prepared, means, covariance, intercept, ability, difficulty, mixing)
    result[[start]] <- list(
      means = means, variance = covariance, intercept = intercept, ability = ability,
      difficulty = difficulty, mixing = mixing, posterior = final$probability,
      log_likelihood = final$log_likelihood, history = history[used], converged = converged,
      iterations = iteration, start = start
    )
  }
  score <- vapply(result, `[[`, numeric(1), "log_likelihood")
  best <- result[[which.max(score)]]
  best$all_starts <- data.frame(
    start = seq_along(result), log_likelihood = score,
    converged = vapply(result, `[[`, logical(1), "converged"),
    iterations = vapply(result, `[[`, integer(1), "iterations"), stringsAsFactors = FALSE
  )
  best$prepared <- prepared
  rownames(best$means) <- prepared$spec$strategies; colnames(best$means) <- prepared$spec$feature_columns
  class(best) <- "eye_strategy_mixture_em"
  best
}

#' Fit the probabilistic strategy-mixture engine
#'
#' @param prepared Prepared strategy data.
#' @param seed Seed.
#' @param refresh CmdStan refresh interval.
#' @param output_dir Optional output directory.
#' @param ... Additional CmdStan sampling arguments.
#' @return An `eye_strategy_mixture_stan` object.
#' @export
fit_strategy_mixture_stan <- function(prepared, seed = 1L, refresh = 0L, output_dir = NULL, ...) {
  if (!inherits(prepared, "eye_strategy_data")) .sd_stop("Expected prepared strategy-mixture data.")
  if (!requireNamespace("cmdstanr", quietly = TRUE)) .sd_stop("The `cmdstanr` package is required for the Stan strategy engine.")
  spec <- prepared$spec
  model <- cmdstanr::cmdstan_model(.sd_cmdstan_file("theory_strategy_mixture.stan"), quiet = TRUE)
  fit <- model$sample(
    data = list(
      N = nrow(prepared$X), K = nrow(prepared$signatures), F = ncol(prepared$X),
      P = length(prepared$participant_levels), J = length(prepared$item_levels),
      X = unname(prepared$X), y = prepared$y, person = prepared$participant,
      item = prepared$item, available = matrix(as.integer(prepared$availability), nrow(prepared$availability)),
      signature = unname(prepared$signatures), anchor_strength = spec$anchor_strength
    ),
    seed = as.integer(seed), chains = spec$chains, parallel_chains = spec$parallel_chains,
    iter_warmup = spec$iter_warmup, iter_sampling = spec$iter_sampling,
    adapt_delta = spec$adapt_delta, max_treedepth = spec$max_treedepth,
    refresh = refresh, output_dir = output_dir, ...
  )
  out <- list(prepared = prepared, spec = spec, fit = fit, summary = fit$summary(), diagnostics = .sd_cmdstan_diagnostics(fit))
  class(out) <- "eye_strategy_mixture_stan"
  out
}

#' Fit a theory-constrained strategy mixture
#'
#' @param data Trial-level data.
#' @param spec Strategy specification.
#' @param seed Seed.
#' @param ... Engine arguments.
#' @return An `eye_theory_strategy_irt` object.
#' @export
fit_theory_strategy_irt <- function(data, spec, seed = 1L, response = NULL, participant = NULL, item = NULL, ...) {
  if (!inherits(spec, "eye_theory_strategy_spec")) .sd_stop("`spec` must be created by `theory_strategy_spec()`.")
  if (!is.null(response)) spec$response <- response
  if (!is.null(participant)) spec$participant <- participant
  if (!is.null(item)) spec$item <- item
  if (isTRUE(spec$legacy_mode)) {
    d <- if (inherits(data, "eye_dataset") && exists("model_data", mode = "function")) model_data(data, include_features = TRUE) else as.data.frame(data)
    features <- colnames(spec$prototypes)
    .sd_match_columns(d, c(spec$response, spec$participant, spec$item, features))
    X <- as.matrix(d[features])
    complete <- stats::complete.cases(X)
    log_post <- matrix(NA_real_, nrow(d), nrow(spec$prototypes), dimnames = list(NULL, rownames(spec$prototypes)))
    for (k in seq_len(nrow(spec$prototypes))) {
      z <- sweep(X, 2L, spec$prototypes[k, ], "-")
      z <- sweep(z, 2L, spec$feature_sd, "/")
      log_post[, k] <- -0.5 * rowSums(z^2) + log(spec$prior[k])
    }
    posterior <- matrix(NA_real_, nrow(d), ncol(log_post), dimnames = dimnames(log_post))
    posterior[complete, ] <- .sd_softmax(log_post[complete, , drop = FALSE])
    assignment <- rep(NA_character_, nrow(d))
    if (any(complete)) assignment[complete] <- colnames(posterior)[max.col(posterior[complete, , drop = FALSE], ties.method = "first")]
    model_frame <- d
    if (ncol(posterior) > 1L) for (k in seq_len(ncol(posterior) - 1L)) model_frame[[paste0("p_strategy_", k)]] <- posterior[, k]
    strategy_terms <- if (ncol(posterior) > 1L) paste0("p_strategy_", seq_len(ncol(posterior) - 1L)) else character()
    formula <- stats::reformulate(c(strategy_terms, spec$participant, spec$item), response = spec$response)
    response_model <- stats::glm(formula, family = stats::binomial(), data = model_frame)
    model <- list(posterior = posterior, assignment = assignment, response_model = response_model)
    class(model) <- "eye_strategy_mixture_legacy"
    out <- list(spec = spec, posterior = posterior, assignment = assignment, response_model = response_model,
                data = model_frame, model = model, prepared = NULL, interpretation = spec$interpretation)
    class(out) <- "eye_theory_strategy_irt"
    return(out)
  }
  prepared <- prepare_strategy_mixture_data(data, spec)
  model <- if (spec$engine == "em") fit_strategy_mixture_em(prepared, seed = seed, ...) else fit_strategy_mixture_stan(prepared, seed = seed, ...)
  out <- list(spec = spec, prepared = prepared, model = model, interpretation = spec$interpretation)
  class(out) <- "eye_theory_strategy_irt"
  out
}
#' @export
print.eye_theory_strategy_irt <- function(x, ...) {
  cat("Theory-constrained strategy-mixture fit\n")
  cat("Engine:     ", x$spec$engine, "\n", sep = "")
  cat("Strategies: ", paste(x$spec$strategies, collapse = ", "), "\n", sep = "")
  if (inherits(x$model, "eye_strategy_mixture_em")) cat("LogLik:     ", format(x$model$log_likelihood, digits = 6), "\n", sep = "")
  cat("Status:     experimental pending external strategy validation\n")
  invisible(x)
}

#' Posterior strategy probabilities
#'
#' @param object Strategy fit.
#' @return Trial-level posterior probabilities.
#' @export
strategy_posterior_probabilities <- function(object) {
  if (!inherits(object, "eye_theory_strategy_irt")) .sd_stop("Expected an `eye_theory_strategy_irt` object.")
  if (!is.null(object$posterior)) {
    probability <- object$posterior
    out <- as.data.frame(probability, stringsAsFactors = FALSE)
    out$trial <- seq_len(nrow(out))
    out$participant_id <- as.character(object$data[[object$spec$participant]])
    out$item_id <- as.character(object$data[[object$spec$item]])
    out$modal_strategy <- NA_character_
    complete <- stats::complete.cases(probability)
    if (any(complete)) out$modal_strategy[complete] <- colnames(probability)[max.col(probability[complete, , drop = FALSE], ties.method = "first")]
    out$modal_probability <- rep(NA_real_, nrow(probability))
    out$entropy <- rep(NA_real_, nrow(probability))
    if (any(complete)) {
      out$modal_probability[complete] <- apply(probability[complete, , drop = FALSE], 1L, max)
      out$entropy[complete] <- .sd_entropy(probability[complete, , drop = FALSE])
    }
    return(out)
  }
  if (inherits(object$model, "eye_strategy_mixture_em")) probability <- object$model$posterior else {
    summary <- object$model$fit$summary(variables = "posterior_probability")
    index <- regmatches(summary$variable, gregexpr("[0-9]+", summary$variable))
    index <- do.call(rbind, lapply(index, as.integer))
    probability <- matrix(NA_real_, max(index[, 1L]), max(index[, 2L]))
    probability[cbind(index[, 1L], index[, 2L])] <- summary$mean
  }
  colnames(probability) <- object$spec$strategies
  out <- as.data.frame(probability, stringsAsFactors = FALSE)
  out$trial <- seq_len(nrow(out))
  out$participant_id <- object$prepared$participant_levels[object$prepared$participant]
  out$item_id <- object$prepared$item_levels[object$prepared$item]
  out$modal_strategy <- colnames(probability)[max.col(probability, ties.method = "first")]
  out$modal_probability <- apply(probability, 1L, max)
  out$entropy <- .sd_entropy(probability)
  out
}

#' Quantify strategy-classification uncertainty
#'
#' @param object Strategy fit.
#' @param threshold Minimum modal probability.
#' @return Summary and trial-level flags.
#' @export
strategy_classification_uncertainty <- function(object, threshold = 0.70) {
  threshold <- suppressWarnings(as.numeric(threshold))
  if (length(threshold) != 1L || !is.finite(threshold) || threshold < 0 || threshold > 1) .sd_stop("`threshold` must be a single probability in [0, 1].")
  probability <- strategy_posterior_probabilities(object)
  out <- list(
    trial = probability,
    summary = data.frame(
      trials = nrow(probability), mean_entropy = mean(probability$entropy, na.rm = TRUE),
      median_modal_probability = stats::median(probability$modal_probability, na.rm = TRUE),
      uncertain_fraction = mean(probability$modal_probability < threshold, na.rm = TRUE),
      threshold = threshold, stringsAsFactors = FALSE
    )
  )
  class(out) <- "eye_strategy_uncertainty"
  out
}

#' Diagnose label stability across multiple starts
#'
#' @param object Strategy fit.
#' @param tolerance Log-likelihood tolerance for equivalent starts.
#' @return Diagnostic data frame.
#' @export
strategy_label_switching_diagnostics <- function(object, tolerance = 1e-4) {
  if (!inherits(object, "eye_theory_strategy_irt")) .sd_stop("Expected a strategy fit.")
  if (inherits(object$model, "eye_strategy_mixture_stan")) {
    summary <- object$model$fit$summary(variables = "feature_mean")
    index <- regmatches(summary$variable, gregexpr("[0-9]+", summary$variable))
    index <- do.call(rbind, lapply(index, as.integer))
    K <- length(object$spec$strategies); F <- length(object$spec$feature_columns)
    means <- matrix(NA_real_, K, F)
    if (!is.null(index) && ncol(index) >= 2L) means[cbind(index[, 1L], index[, 2L])] <- summary$mean
    cosine <- matrix(NA_real_, K, K, dimnames = list(component = object$spec$strategies, signature = object$spec$strategies))
    for (k in seq_len(K)) for (j in seq_len(K)) {
      denominator <- sqrt(sum(means[k, ]^2)) * sqrt(sum(object$spec$signatures[j, ]^2))
      cosine[k, j] <- if (is.finite(denominator) && denominator > 0) sum(means[k, ] * object$spec$signatures[j, ]) / denominator else NA_real_
    }
    closest <- apply(cosine, 1L, function(z) if (all(is.na(z))) NA_character_ else colnames(cosine)[which.max(z)])
    out <- data.frame(
      engine = "stan", component = rownames(cosine), expected_signature = rownames(cosine), closest_signature = closest,
      cosine_expected = diag(cosine), cosine_best = apply(cosine, 1L, max, na.rm = TRUE),
      anchored = closest == rownames(cosine) & diag(cosine) >= tolerance, stringsAsFactors = FALSE
    )
    class(out) <- c("eye_strategy_label_diagnostics", "data.frame")
    return(out)
  }
  if (!inherits(object$model, "eye_strategy_mixture_em")) {
    return(data.frame(engine = object$spec$engine, assessed = FALSE, reason = "Label diagnostics are unavailable for this engine.", stringsAsFactors = FALSE))
  }
  starts <- object$model$all_starts
  starts$delta_best <- max(starts$log_likelihood) - starts$log_likelihood
  starts$equivalent_optimum <- starts$delta_best <= tolerance * (1 + abs(max(starts$log_likelihood)))
  starts$label_anchor <- "theory_signature"
  class(starts) <- c("eye_strategy_label_diagnostics", "data.frame")
  starts
}

#' Assess sensitivity to alternative AOI feature definitions
#'
#' @param datasets Named list of alternative trial-level datasets.
#' @param spec Strategy specification.
#' @param seed Seed.
#' @param ... Fit arguments.
#' @return An `eye_strategy_aoi_sensitivity` object.
#' @export
strategy_aoi_sensitivity <- function(datasets, spec, seed = 1L, ...) {
  if (!is.list(datasets) || !length(datasets)) .sd_stop("`datasets` must be a non-empty named list.")
  if (is.null(names(datasets))) names(datasets) <- paste0("definition", seq_along(datasets))
  fits <- lapply(seq_along(datasets), function(i) fit_theory_strategy_irt(datasets[[i]], spec, seed = seed + i - 1L, ...))
  names(fits) <- names(datasets)
  summaries <- lapply(names(fits), function(name) {
    probability <- strategy_posterior_probabilities(fits[[name]])
    share <- prop.table(table(factor(probability$modal_strategy, levels = spec$strategies)))
    data.frame(definition = name, strategy = names(share), modal_share = as.numeric(share), mean_entropy = mean(probability$entropy, na.rm = TRUE), stringsAsFactors = FALSE)
  })
  out <- list(fits = fits, summary = do.call(rbind, summaries), spec = spec)
  class(out) <- "eye_strategy_aoi_sensitivity"
  out
}

#' @export
plot.eye_strategy_aoi_sensitivity <- function(x, ...) {
  if (requireNamespace("ggplot2", quietly = TRUE)) {
    return(ggplot2::ggplot(x$summary, ggplot2::aes(x = definition, y = modal_share, fill = strategy)) +
      ggplot2::geom_col(position = "dodge") + ggplot2::labs(x = NULL, y = "Modal strategy share") +
      ggplot2::theme_minimal() + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)))
  }
  matrix <- xtabs(modal_share ~ strategy + definition, x$summary)
  graphics::barplot(matrix, beside = TRUE, legend.text = TRUE, ylab = "Modal strategy share", ...)
  invisible(x)
}

#' Validate strategy posteriors against an experimental manipulation
#'
#' @param object Fitted strategy-mixture object.
#' @param condition Condition column in the original trial data.
#' @param expected_strategy Named character vector mapping condition values to prespecified strategy labels.
#' @param minimum_contrast Minimum mean posterior-probability contrast over alternative strategies.
#' @return An `eye_strategy_manipulation_validation` object.
#' @export
validate_strategy_manipulation <- function(object, condition, expected_strategy, minimum_contrast = 0) {
  if (!inherits(object, "eye_theory_strategy_irt")) .sd_stop("Expected a strategy fit.")
  data <- if (!is.null(object$prepared$data)) object$prepared$data else object$data
  if (!is.data.frame(data) || !condition %in% names(data)) .sd_stop(sprintf("Condition column `%s` is unavailable.", condition))
  expected_strategy <- as.character(expected_strategy)
  if (is.null(names(expected_strategy)) || any(!nzchar(names(expected_strategy)))) .sd_stop("`expected_strategy` must be named by condition value.")
  unknown <- setdiff(expected_strategy, object$spec$strategies)
  if (length(unknown)) .sd_stop(sprintf("Unknown expected strategies: %s.", paste(unique(unknown), collapse = ", ")))
  probability <- strategy_posterior_probabilities(object)
  if (nrow(probability) != nrow(data)) .sd_stop("Posterior rows do not align with the original trial data.")
  rows <- lapply(names(expected_strategy), function(level) {
    index <- which(as.character(data[[condition]]) == level)
    strategy <- expected_strategy[[level]]
    if (!length(index)) return(data.frame(condition = level, expected_strategy = strategy, trials = 0L, expected_probability = NA_real_, alternative_probability = NA_real_, contrast = NA_real_, passed = FALSE, stringsAsFactors = FALSE))
    alternatives <- setdiff(object$spec$strategies, strategy)
    expected_probability <- mean(probability[[strategy]][index], na.rm = TRUE)
    alternative_probability <- if (length(alternatives)) mean(as.matrix(probability[index, alternatives, drop = FALSE]), na.rm = TRUE) else NA_real_
    contrast <- expected_probability - alternative_probability
    data.frame(condition = level, expected_strategy = strategy, trials = length(index), expected_probability = expected_probability, alternative_probability = alternative_probability, contrast = contrast, passed = is.finite(contrast) && contrast >= minimum_contrast, stringsAsFactors = FALSE)
  })
  findings <- do.call(rbind, rows)
  out <- list(valid = nrow(findings) > 0L && all(findings$passed), findings = findings, condition = condition, minimum_contrast = minimum_contrast, interpretation = "Experimental alignment supports, but does not by itself identify, a cognitive strategy.")
  class(out) <- "eye_strategy_manipulation_validation"
  out
}

#' @export
print.eye_strategy_manipulation_validation <- function(x, ...) {
  cat("Strategy-manipulation validation\n")
  cat("Passed: ", x$valid, "\n", sep = "")
  print(x$findings, row.names = FALSE)
  invisible(x)
}

#' Compare mixture and continuous heterogeneity descriptions
#'
#' @param object Strategy fit.
#' @return Comparison table.
#' @export
compare_strategy_heterogeneity <- function(object) {
  if (!inherits(object, "eye_theory_strategy_irt")) .sd_stop("Expected a strategy fit.")
  if (is.null(object$prepared)) {
    return(data.frame(
      model = c("theory_strategy_mixture", "continuous_heterogeneity"),
      log_likelihood = c(as.numeric(stats::logLik(object$response_model)), NA_real_),
      parameters = c(length(stats::coef(object$response_model)), NA_integer_),
      AIC = c(stats::AIC(object$response_model), NA_real_),
      BIC = c(stats::BIC(object$response_model), NA_real_),
      interpretation = c("legacy prototype-weighted response model", "not fitted for legacy objects"),
      stringsAsFactors = FALSE
    ))
  }
  prepared <- object$prepared
  continuous_data <- data.frame(y = prepared$y, prepared$X, check.names = FALSE)
  continuous <- if (ncol(prepared$X)) {
    stats::glm(stats::reformulate(colnames(prepared$X), response = "y"), data = continuous_data, family = stats::binomial())
  } else {
    stats::glm(y ~ 1, data = continuous_data, family = stats::binomial())
  }
  continuous_ll <- as.numeric(stats::logLik(continuous)); continuous_k <- length(stats::coef(continuous))
  if (inherits(object$model, "eye_strategy_mixture_em")) {
    mixture_ll <- object$model$log_likelihood
    K <- length(object$spec$strategies); F <- length(object$spec$feature_columns)
    mixture_k <- K * (2L * F + 1L) + length(object$model$ability) + length(object$model$difficulty) + K - 1L
  } else { mixture_ll <- NA_real_; mixture_k <- NA_integer_ }
  data.frame(
    model = c("theory_strategy_mixture", "continuous_heterogeneity"),
    log_likelihood = c(mixture_ll, continuous_ll),
    parameters = c(mixture_k, continuous_k),
    AIC = c(if (is.finite(mixture_ll)) -2 * mixture_ll + 2 * mixture_k else NA_real_, -2 * continuous_ll + 2 * continuous_k),
    BIC = c(if (is.finite(mixture_ll)) -2 * mixture_ll + log(nrow(prepared$X)) * mixture_k else NA_real_, -2 * continuous_ll + log(nrow(prepared$X)) * continuous_k),
    interpretation = c("prespecified discrete strategies", "continuous process heterogeneity"), stringsAsFactors = FALSE
  )
}

#' Simulate a theory-defined strategy-mixture study
#'
#' @param n_person Number of participants.
#' @param n_item Number of items.
#' @param signatures Strategy signature matrix.
#' @param trials_per_item Trials per person-item combination.
#' @param strategy_prevalence Strategy prevalence.
#' @param feature_sd Feature residual SD.
#' @param seed Seed.
#' @return Simulated trial data with truth attributes.
#' @export
simulate_strategy_mixture_data <- function(n_person = 100L, n_item = 20L, signatures, trials_per_item = 1L, strategy_prevalence = NULL, feature_sd = 0.6, seed = 1L) {
  n_person <- suppressWarnings(as.integer(n_person)); n_item <- suppressWarnings(as.integer(n_item)); trials_per_item <- suppressWarnings(as.integer(trials_per_item))
  if (length(n_person) != 1L || is.na(n_person) || n_person < 2L || length(n_item) != 1L || is.na(n_item) || n_item < 2L || length(trials_per_item) != 1L || is.na(trials_per_item) || trials_per_item < 1L) .sd_stop("Simulation requires at least two persons, two items, and one trial per item.")
  feature_sd <- suppressWarnings(as.numeric(feature_sd))
  if (length(feature_sd) != 1L || !is.finite(feature_sd) || feature_sd <= 0) .sd_stop("`feature_sd` must be a positive finite number.")
  signatures <- as.matrix(signatures)
  storage.mode(signatures) <- "double"
  if (nrow(signatures) < 2L || ncol(signatures) < 1L || any(!is.finite(signatures))) .sd_stop("`signatures` must contain at least two strategies and one finite feature.")
  if (is.null(rownames(signatures)) || is.null(colnames(signatures)) || any(!nzchar(rownames(signatures))) || any(!nzchar(colnames(signatures)))) .sd_stop("`signatures` needs non-empty strategy and feature names.")
  if (anyDuplicated(rownames(signatures)) || anyDuplicated(colnames(signatures))) .sd_stop("Strategy and feature names must be unique.")
  K <- nrow(signatures)
  if (is.null(strategy_prevalence)) strategy_prevalence <- rep(1 / K, K)
  strategy_prevalence <- suppressWarnings(as.numeric(strategy_prevalence))
  if (length(strategy_prevalence) != K || any(!is.finite(strategy_prevalence)) || any(strategy_prevalence < 0) || sum(strategy_prevalence) <= 0) .sd_stop("`strategy_prevalence` must contain one non-negative finite value per strategy and have a positive sum.")
  strategy_prevalence <- strategy_prevalence / sum(strategy_prevalence)
  set.seed(seed)
  grid <- expand.grid(participant_id = paste0("P", seq_len(n_person)), item_id = paste0("I", seq_len(n_item)), repetition = seq_len(trials_per_item), stringsAsFactors = FALSE)
  theta <- stats::rnorm(n_person); difficulty <- stats::rnorm(n_item)
  strategy <- sample(seq_len(K), nrow(grid), replace = TRUE, prob = strategy_prevalence)
  X <- signatures[strategy, , drop = FALSE] + matrix(stats::rnorm(nrow(grid) * ncol(signatures), sd = feature_sd), nrow(grid))
  score <- stats::rbinom(nrow(grid), 1L, stats::plogis(theta[match(grid$participant_id, unique(grid$participant_id))] - difficulty[match(grid$item_id, unique(grid$item_id))] + seq(-0.3, 0.3, length.out = K)[strategy]))
  out <- cbind(grid, score = score, as.data.frame(X, stringsAsFactors = FALSE), true_strategy = rownames(signatures)[strategy])
  attr(out, "truth") <- list(theta = theta, difficulty = difficulty, signatures = signatures, prevalence = strategy_prevalence)
  out
}

# Gaze-informed diffusion ------------------------------------------------------

#' Define a gaze-informed diffusion model
#'
#' @param response Binary response column.
#' @param response_time Response-time column in seconds.
#' @param participant Participant identifier.
#' @param item Item identifier.
#' @param drift_features Features assigned a priori to drift rate.
#' @param boundary_features Features assigned a priori to boundary separation.
#' @param nondecision_features Features assigned a priori to non-decision time.
#' @param starting_features Features assigned a priori to starting-point bias.
#' @param censor_column Optional censoring column with `observed`, `left`, or `right`.
#' @param contaminant Whether to estimate a uniform contaminant mixture.
#' @param engine Baseline approximation or Stan Wiener model.
#' @param chains,parallel_chains,iter_warmup,iter_sampling,adapt_delta,max_treedepth Stan controls.
#' @return An `eye_gaze_diffusion_spec`.
#' @export
gaze_diffusion_spec <- function(
  response = "score", response_time = "response_time",
  participant = "participant_id", item = "item_id",
  drift_features = character(), boundary_features = character(),
  nondecision_features = character(), starting_features = character(),
  censor_column = NULL, contaminant = TRUE,
  engine = c("baseline", "stan", "ez_regression", "diffIRT", "brms"),
  gaze_features = NULL,
  chains = 4L, parallel_chains = min(4L, chains),
  iter_warmup = 1000L, iter_sampling = 1000L,
  adapt_delta = 0.97, max_treedepth = 13L
) {
  engine <- match.arg(engine)
  if (!is.null(gaze_features) && !length(drift_features)) drift_features <- as.character(gaze_features)
  all_features <- c(drift_features, boundary_features, nondecision_features, starting_features)
  if (anyDuplicated(all_features)) .sd_stop("A feature may map to only one diffusion parameter in a confirmatory specification.")
  if (anyNA(all_features) || any(!nzchar(all_features))) .sd_stop("Diffusion feature names must be non-missing and non-empty.")
  if (length(c(response, response_time, participant, item)) != 4L || any(!nzchar(c(response, response_time, participant, item)))) .sd_stop("Response, time, participant, and item columns must be scalar names.")
  out <- list(
    response = response, response_time = response_time, participant = participant, item = item,
    drift_features = drift_features, gaze_features = unique(all_features), boundary_features = boundary_features,
    nondecision_features = nondecision_features, starting_features = starting_features,
    censor_column = censor_column, contaminant = isTRUE(contaminant), engine = engine,
    normalized_engine = if (engine %in% c("ez_regression", "diffIRT", "brms")) engine else engine,
    legacy_mode = engine %in% c("ez_regression", "diffIRT", "brms"),
    chains = as.integer(chains), parallel_chains = as.integer(parallel_chains),
    iter_warmup = as.integer(iter_warmup), iter_sampling = as.integer(iter_sampling),
    adapt_delta = adapt_delta, max_treedepth = as.integer(max_treedepth),
    interpretation = "Gaze covariates are parameter predictors, not direct measures of attention or evidence quality."
  )
  class(out) <- "eye_gaze_diffusion_spec"
  out
}
#' @export
print.eye_gaze_diffusion_spec <- function(x, ...) {
  cat("Gaze-informed diffusion specification\n")
  cat("Engine:       ", x$engine, "\n", sep = "")
  cat("Drift:        ", if (length(x$drift_features)) paste(x$drift_features, collapse = ", ") else "intercept only", "\n", sep = "")
  cat("Boundary:     ", if (length(x$boundary_features)) paste(x$boundary_features, collapse = ", ") else "intercept only", "\n", sep = "")
  cat("Nondecision:  ", if (length(x$nondecision_features)) paste(x$nondecision_features, collapse = ", ") else "intercept only", "\n", sep = "")
  cat("Starting bias:", if (length(x$starting_features)) paste0(" ", paste(x$starting_features, collapse = ", ")) else " intercept only", "\n", sep = "")
  invisible(x)
}

.sd_matrix <- function(data, columns, prefix) {
  if (!length(columns)) return(matrix(0, nrow(data), 0L, dimnames = list(NULL, character())))
  X <- as.matrix(data[columns]); storage.mode(X) <- "double"
  center <- colMeans(X); scale <- apply(X, 2L, stats::sd); scale[!is.finite(scale) | scale == 0] <- 1
  X <- sweep(sweep(X, 2L, center, "-"), 2L, scale, "/")
  colnames(X) <- paste0(prefix, columns)
  attr(X, "center") <- center; attr(X, "scale") <- scale
  X
}

#' Prepare joint accuracy-response-time data
#'
#' @param data Trial-level data.
#' @param spec Diffusion specification.
#' @param minimum_rt Minimum admissible RT in seconds.
#' @return An `eye_gaze_diffusion_data` object.
#' @export
prepare_gaze_diffusion_data <- function(data, spec, minimum_rt = 0.05) {
  if (!inherits(spec, "eye_gaze_diffusion_spec")) .sd_stop("`spec` must be created by `gaze_diffusion_spec()`.")
  if (!is.data.frame(data)) .sd_stop("`data` must be a data frame.")
  feature <- c(spec$drift_features, spec$boundary_features, spec$nondecision_features, spec$starting_features)
  required <- c(spec$response, spec$response_time, spec$participant, spec$item, feature, spec$censor_column)
  required <- required[!is.na(required) & nzchar(required)]
  .sd_match_columns(data, required)
  complete_columns <- c(spec$response, spec$response_time, spec$participant, spec$item, feature, spec$censor_column)
  complete_columns <- complete_columns[!is.na(complete_columns) & nzchar(complete_columns)]
  keep <- stats::complete.cases(data[complete_columns])
  d <- data[keep, , drop = FALSE]
  d[[spec$response_time]] <- as.numeric(d[[spec$response_time]])
  if (!nrow(d) || any(!is.finite(d[[spec$response_time]])) || any(d[[spec$response_time]] <= minimum_rt)) .sd_stop("Response times must be finite seconds greater than `minimum_rt`.")
  if (stats::median(d[[spec$response_time]]) > 30) .sd_stop("Response times appear to be milliseconds; convert them to seconds before diffusion modelling.")
  y <- as.integer(d[[spec$response]])
  if (!all(y %in% 0:1)) .sd_stop("Diffusion responses must be coded 0/1.")
  censor <- if (is.null(spec$censor_column)) rep(0L, nrow(d)) else {
    value <- tolower(as.character(d[[spec$censor_column]]))
    match(value, c("observed", "right", "left")) - 1L
  }
  if (anyNA(censor)) .sd_stop("Censoring values must be `observed`, `right`, or `left`.")
  participant_levels <- unique(as.character(d[[spec$participant]])); item_levels <- unique(as.character(d[[spec$item]]))
  out <- list(
    data = d, y = y, rt = d[[spec$response_time]], censor = censor,
    participant = match(as.character(d[[spec$participant]]), participant_levels),
    item = match(as.character(d[[spec$item]]), item_levels),
    participant_levels = participant_levels, item_levels = item_levels,
    X_drift = .sd_matrix(d, spec$drift_features, "drift:"),
    X_boundary = .sd_matrix(d, spec$boundary_features, "boundary:"),
    X_nondecision = .sd_matrix(d, spec$nondecision_features, "nondecision:"),
    X_starting = .sd_matrix(d, spec$starting_features, "starting:"),
    rt_lower = minimum_rt,
    rt_upper = max(d[[spec$response_time]]) * 1.25,
    minimum_observed_rt = min(d[[spec$response_time]]), spec = spec
  )
  class(out) <- "eye_gaze_diffusion_data"
  out
}

#' @export
print.eye_gaze_diffusion_data <- function(x, ...) {
  cat("Prepared gaze-diffusion data\n")
  cat("Trials:       ", length(x$y), "\n", sep = "")
  cat("Participants: ", length(x$participant_levels), "\n", sep = "")
  cat("Items:        ", length(x$item_levels), "\n", sep = "")
  cat("Censored:     ", sum(x$censor != 0L), "\n", sep = "")
  invisible(x)
}

#' Fit the CmdStan Wiener diffusion model
#'
#' @param prepared Prepared data.
#' @param seed Seed.
#' @param refresh CmdStan refresh interval.
#' @param output_dir Optional output directory.
#' @param ... Additional sampling arguments.
#' @return An `eye_gaze_diffusion_stan` object.
#' @export
fit_gaze_diffusion_stan <- function(prepared, seed = 1L, refresh = 0L, output_dir = NULL, ...) {
  if (!inherits(prepared, "eye_gaze_diffusion_data")) .sd_stop("Expected prepared gaze-diffusion data.")
  if (!requireNamespace("cmdstanr", quietly = TRUE)) .sd_stop("The `cmdstanr` package is required for the Stan diffusion engine.")
  cmdstan_version <- tryCatch(cmdstanr::cmdstan_version(error_on_NA = FALSE), error = function(e) NA)
  if (length(cmdstan_version) != 1L || is.na(cmdstan_version) || utils::compareVersion(as.character(cmdstan_version), "2.38.0") < 0L) {
    .sd_stop("The censored Wiener engine requires CmdStan 2.38.0 or newer because it uses `wiener_lcdf_unnorm()` and `wiener_lccdf_unnorm()`.")
  }
  spec <- prepared$spec
  model <- cmdstanr::cmdstan_model(.sd_cmdstan_file("gaze_diffusion_irt.stan"), quiet = TRUE)
  fit <- model$sample(
    data = list(
      N = length(prepared$y), P = length(prepared$participant_levels), J = length(prepared$item_levels),
      Fd = ncol(prepared$X_drift), Fb = ncol(prepared$X_boundary),
      Fn = ncol(prepared$X_nondecision), Fs = ncol(prepared$X_starting),
      Xd = unname(prepared$X_drift), Xb = unname(prepared$X_boundary),
      Xn = unname(prepared$X_nondecision), Xs = unname(prepared$X_starting),
      y = prepared$y, rt = prepared$rt, censor = prepared$censor,
      person = prepared$participant, item = prepared$item,
      min_rt = prepared$minimum_observed_rt, rt_lower = prepared$rt_lower,
      rt_upper = prepared$rt_upper, use_contaminant = as.integer(spec$contaminant)
    ),
    seed = as.integer(seed), chains = spec$chains, parallel_chains = spec$parallel_chains,
    iter_warmup = spec$iter_warmup, iter_sampling = spec$iter_sampling,
    adapt_delta = spec$adapt_delta, max_treedepth = spec$max_treedepth,
    refresh = refresh, output_dir = output_dir, ...
  )
  out <- list(prepared = prepared, spec = spec, fit = fit, summary = fit$summary(), diagnostics = .sd_cmdstan_diagnostics(fit))
  class(out) <- "eye_gaze_diffusion_stan"
  out
}

.sd_baseline_diffusion <- function(prepared) {
  d <- prepared$data
  d$.y <- prepared$y; d$.log_rt <- log(prepared$rt)
  all_features <- unique(c(prepared$spec$drift_features, prepared$spec$boundary_features, prepared$spec$nondecision_features, prepared$spec$starting_features))
  rhs <- c(all_features, sprintf("factor(%s)", prepared$spec$participant), sprintf("factor(%s)", prepared$spec$item))
  accuracy <- suppressWarnings(stats::glm(stats::reformulate(rhs, response = ".y"), family = stats::binomial(), data = d))
  timing <- stats::lm(stats::reformulate(rhs, response = ".log_rt"), data = d)
  out <- list(accuracy = accuracy, timing = timing, prepared = prepared,
              item_parameters = data.frame(item_id = prepared$item_levels, difficulty = NA_real_, boundary_proxy = NA_real_, nondecision_proxy = NA_real_, stringsAsFactors = FALSE))
  class(out) <- "eye_gaze_diffusion_baseline"
  out
}

#' Fit a gaze-informed diffusion model
#'
#' @param data Trial-level data.
#' @param spec Diffusion specification.
#' @param seed Seed.
#' @param ... Engine arguments.
#' @return An `eye_gaze_diffusion_irt` object.
#' @export
fit_gaze_diffusion_irt <- function(data, spec = gaze_diffusion_spec(), seed = 1L, ...) {
  if (!inherits(spec, "eye_gaze_diffusion_spec")) .sd_stop("`spec` must be created by `gaze_diffusion_spec()`.")
  if (inherits(data, "eye_dataset") || isTRUE(spec$legacy_mode)) {
    if (!inherits(data, "eye_dataset")) .sd_stop("Legacy diffusion engines require an `eye_dataset`.")
    if (spec$engine == "diffIRT") {
      baseline <- fit_diffirt_adapter(data, ...)
      out <- list(spec = spec, baseline = baseline, gaze_model = NULL, data = NULL,
                  warning = "The diffIRT adapter is a benchmark and does not make gaze a momentary accumulation input.")
    } else if (spec$engine == "brms") {
      if (!requireNamespace("brms", quietly = TRUE)) .sd_stop("The `brms` package is required for the legacy Wiener engine.")
      d <- .ep_diffusion_model_data(data, spec)
      .sd_match_columns(d, c(spec$response, spec$response_time, spec$gaze_features, "participant_id", "item_id"))
      rhs <- paste(c(spec$gaze_features, "(1 | participant_id)", "(1 | item_id)"), collapse = " + ")
      formula <- stats::as.formula(paste0(spec$response_time, " | dec(", spec$response, ") ~ ", rhs))
      model <- brms::brm(formula, data = d, family = brms::wiener(), ...)
      out <- list(spec = spec, baseline = NULL, gaze_model = model, data = d,
                  warning = "Wiener interpretation depends on coding, priors, identification, and temporal gaze definitions.")
    } else {
      d <- .ep_diffusion_model_data(data, spec)
      .sd_match_columns(d, c(spec$response, spec$response_time, "item_id", spec$gaze_features))
      ez <- estimate_ez_diffusion(d, accuracy = spec$response, response_time = spec$response_time, by = "item_id")
      item_gaze <- if (length(spec$gaze_features)) stats::aggregate(d[spec$gaze_features], list(item_id = d$item_id), mean, na.rm = TRUE) else data.frame(item_id = unique(d$item_id))
      z <- merge(ez, item_gaze, by = "item_id", all.x = TRUE)
      fits <- list()
      if (length(spec$gaze_features) && nrow(z) > length(spec$gaze_features) + 2L) {
        rhs <- paste(spec$gaze_features, collapse = " + ")
        for (parameter in c("drift_rate", "boundary_separation", "nondecision_time")) fits[[parameter]] <- stats::lm(stats::as.formula(paste(parameter, "~", rhs)), data = z)
      }
      out <- list(spec = spec, baseline = ez, gaze_model = fits, data = z,
                  warning = "EZ regression is an item-level approximation and not a fixation-dependent accumulation likelihood.")
    }
    class(out) <- "eye_gaze_diffusion_irt"
    return(out)
  }
  prepared <- prepare_gaze_diffusion_data(data, spec)
  model <- if (spec$engine == "baseline") .sd_baseline_diffusion(prepared) else fit_gaze_diffusion_stan(prepared, seed = seed, ...)
  out <- list(spec = spec, prepared = prepared, model = model, interpretation = spec$interpretation)
  class(out) <- "eye_gaze_diffusion_irt"
  out
}
#' @export
print.eye_gaze_diffusion_irt <- function(x, ...) {
  cat("Gaze-informed joint accuracy-RT model\n")
  cat("Engine: ", x$spec$engine, "\n", sep = "")
  trials <- if (!is.null(x$prepared$y)) length(x$prepared$y) else if (!is.null(x$data)) nrow(x$data) else NA_integer_
  if (is.finite(trials)) cat("Trials: ", trials, "\n", sep = "")
  cat("Status: experimental pending identification, recovery, and empirical equivalence evidence\n")
  invisible(x)
}

#' Extract diffusion parameter summaries
#'
#' @param object Diffusion fit.
#' @param variables Optional variable prefixes.
#' @return Parameter summary.
#' @export
extract_diffusion_parameters <- function(object, variables = c("beta_drift", "beta_boundary", "beta_nondecision", "beta_starting", "person_drift", "item_difficulty", "boundary", "nondecision", "starting", "contaminant_probability")) {
  if (!inherits(object, "eye_gaze_diffusion_irt")) .sd_stop("Expected a gaze-diffusion fit.")
  if (is.null(object$model) && isTRUE(object$spec$legacy_mode)) .sd_stop("Use the legacy object's `baseline` and `gaze_model` components; joint diffusion parameters are unavailable.")
  if (inherits(object$model, "eye_gaze_diffusion_baseline")) {
    accuracy <- data.frame(component = "accuracy", term = names(stats::coef(object$model$accuracy)), estimate = unname(stats::coef(object$model$accuracy)), stringsAsFactors = FALSE)
    timing <- data.frame(component = "log_response_time", term = names(stats::coef(object$model$timing)), estimate = unname(stats::coef(object$model$timing)), stringsAsFactors = FALSE)
    return(rbind(accuracy, timing))
  }
  summary <- object$model$fit$summary()
  keep <- Reduce(`|`, lapply(variables, function(prefix) startsWith(summary$variable, prefix)))
  summary[keep, intersect(c("variable", "mean", "median", "sd", "q5", "q95", "rhat", "ess_bulk", "ess_tail"), names(summary)), drop = FALSE]
}

#' Diagnose diffusion-parameter trade-offs and sampling
#'
#' @param object Diffusion fit.
#' @param correlation_threshold Correlation threshold.
#' @return An `eye_diffusion_diagnostics` object.
#' @export
diffusion_parameter_diagnostics <- function(object, correlation_threshold = 0.85) {
  if (!inherits(object, "eye_gaze_diffusion_irt")) .sd_stop("Expected a gaze-diffusion fit.")
  if (!inherits(object$model, "eye_gaze_diffusion_stan")) {
    return(structure(list(engine = "baseline", sampling = NULL, correlations = data.frame(), flagged = data.frame(), message = "Posterior identification diagnostics require the Stan engine."), class = "eye_diffusion_diagnostics"))
  }
  prefixes <- c("beta_drift", "beta_boundary", "beta_nondecision", "beta_starting", "boundary_intercept", "nondecision_intercept", "starting_intercept")
  variables <- object$model$fit$metadata()$model_params
  variables <- variables[Reduce(`|`, lapply(prefixes, function(prefix) startsWith(variables, prefix)))]
  if (!length(variables)) .sd_stop("No diffusion parameters were available for correlation diagnostics.")
  draws <- object$model$fit$draws(variables = variables, format = "matrix")
  correlation <- stats::cor(draws)
  pair <- which(abs(correlation) >= correlation_threshold & upper.tri(correlation), arr.ind = TRUE)
  flagged <- if (nrow(pair)) data.frame(parameter1 = colnames(correlation)[pair[, 1L]], parameter2 = colnames(correlation)[pair[, 2L]], correlation = correlation[pair], stringsAsFactors = FALSE) else data.frame(parameter1 = character(), parameter2 = character(), correlation = numeric())
  out <- list(engine = "stan", sampling = object$model$diagnostics, correlations = correlation, flagged = flagged, threshold = correlation_threshold)
  class(out) <- "eye_diffusion_diagnostics"
  out
}

#' @export
print.eye_diffusion_diagnostics <- function(x, ...) {
  cat("Diffusion identification diagnostics\n")
  cat("Engine: ", x$engine, "\n", sep = "")
  if (!is.null(x$sampling)) print(x$sampling, row.names = FALSE)
  cat("High posterior parameter correlations: ", nrow(x$flagged), "\n", sep = "")
  invisible(x)
}

#' Posterior predictive summaries for accuracy and RT
#'
#' @param object Diffusion fit.
#' @param draws Number of generated-quantity draws to retain.
#' @return Trial- and aggregate-level summaries.
#' @export
diffusion_posterior_predictive <- function(object, draws = 200L, method = c("rtdists", "stan_proxy"), seed = 1L) {
  if (!inherits(object, "eye_gaze_diffusion_irt")) .sd_stop("Expected a gaze-diffusion fit.")
  if (is.null(object$prepared)) .sd_stop("Posterior predictive summaries are unavailable for legacy approximation objects.")
  draws <- as.integer(draws); if (length(draws) != 1L || is.na(draws) || draws < 1L) .sd_stop("`draws` must be a positive integer.")
  method <- match.arg(method); set.seed(as.integer(seed))
  observed <- data.frame(accuracy = mean(object$prepared$y), mean_rt = mean(object$prepared$rt), median_rt = stats::median(object$prepared$rt), stringsAsFactors = FALSE)
  if (!inherits(object$model, "eye_gaze_diffusion_stan")) return(list(observed = observed, replicated = NULL, method = "unavailable for baseline engine"))
  if (method == "stan_proxy") {
    warning("`stan_proxy` uses approximate generated quantities because Stan has no built-in Wiener RNG; use `method = 'rtdists'` for first-passage simulation.", call. = FALSE)
    yrep <- object$model$fit$draws(variables = "y_rep", format = "matrix")
    rtrep <- object$model$fit$draws(variables = "rt_rep", format = "matrix")
    index <- seq_len(min(draws, nrow(yrep)))
    replicated <- data.frame(
      draw = index, accuracy = rowMeans(yrep[index, , drop = FALSE]),
      mean_rt = rowMeans(rtrep[index, , drop = FALSE]),
      median_rt = apply(rtrep[index, , drop = FALSE], 1L, stats::median), stringsAsFactors = FALSE
    )
    return(list(observed = observed, replicated = replicated, method = "explicit approximate Stan generated quantities"))
  }
  if (!requireNamespace("rtdists", quietly = TRUE)) .sd_stop("The `rtdists` package is required for first-passage posterior predictive simulation.")
  variables <- c(
    "beta_drift", "beta_boundary", "beta_nondecision", "beta_starting",
    "person_drift", "item_difficulty", "person_boundary", "item_boundary",
    "drift_intercept", "boundary_intercept", "nondecision_intercept", "starting_intercept",
    "contaminant_probability"
  )
  posterior <- object$model$fit$draws(variables = variables, format = "matrix")
  if (!nrow(posterior)) .sd_stop("No posterior draws were returned for predictive simulation.")
  selected <- sort(sample.int(nrow(posterior), min(draws, nrow(posterior))))
  prepared <- object$prepared; spec <- prepared$spec; N <- length(prepared$y)
  indexed <- function(row, prefix, count) {
    if (count == 0L) return(numeric())
    columns <- paste0(prefix, "[", seq_len(count), "]")
    missing <- setdiff(columns, colnames(posterior)); if (length(missing)) .sd_stop("Posterior is missing variables: ", paste(missing, collapse = ", "))
    as.numeric(row[columns])
  }
  replicated <- lapply(seq_along(selected), function(k) {
    row <- posterior[selected[k], , drop = TRUE]
    drift <- as.numeric(row["drift_intercept"]) + indexed(row, "person_drift", length(prepared$participant_levels))[prepared$participant] - indexed(row, "item_difficulty", length(prepared$item_levels))[prepared$item]
    if (ncol(prepared$X_drift)) drift <- drift + as.numeric(prepared$X_drift %*% indexed(row, "beta_drift", ncol(prepared$X_drift)))
    boundary_eta <- as.numeric(row["boundary_intercept"]) + indexed(row, "person_boundary", length(prepared$participant_levels))[prepared$participant] + indexed(row, "item_boundary", length(prepared$item_levels))[prepared$item]
    if (ncol(prepared$X_boundary)) boundary_eta <- boundary_eta + as.numeric(prepared$X_boundary %*% indexed(row, "beta_boundary", ncol(prepared$X_boundary)))
    nondecision_eta <- rep(as.numeric(row["nondecision_intercept"]), N)
    if (ncol(prepared$X_nondecision)) nondecision_eta <- nondecision_eta + as.numeric(prepared$X_nondecision %*% indexed(row, "beta_nondecision", ncol(prepared$X_nondecision)))
    starting_eta <- rep(as.numeric(row["starting_intercept"]), N)
    if (ncol(prepared$X_starting)) starting_eta <- starting_eta + as.numeric(prepared$X_starting %*% indexed(row, "beta_starting", ncol(prepared$X_starting)))
    boundary <- exp(boundary_eta)
    nondecision <- 0.95 * prepared$minimum_observed_rt * stats::plogis(nondecision_eta)
    starting <- 0.02 + 0.96 * stats::plogis(starting_eta)
    sim <- rtdists::rdiffusion(N, a = boundary, v = drift, t0 = nondecision, z = starting)
    response_name <- intersect(c("response", "resp"), names(sim))[1L]; rt_name <- intersect(c("rt", "response_time"), names(sim))[1L]
    if (is.na(response_name) || is.na(rt_name)) .sd_stop("`rtdists::rdiffusion()` returned an unsupported structure.")
    response <- sim[[response_name]]
    if (is.factor(response) || is.character(response)) {
      label <- tolower(trimws(as.character(response)))
      response <- ifelse(label %in% c("upper", "correct", "1", "true"), 1L, ifelse(label %in% c("lower", "error", "0", "false"), 0L, NA_integer_))
    } else {
      response <- as.numeric(response); levels <- sort(unique(response[is.finite(response)]))
      if (identical(levels, c(1, 2))) response <- as.integer(response == 2) else if (identical(levels, c(-1, 1))) response <- as.integer(response == 1) else response <- as.integer(response)
    }
    rt <- as.numeric(sim[[rt_name]])
    contaminant_probability <- as.numeric(row["contaminant_probability"])
    if (is.finite(contaminant_probability) && contaminant_probability > 0) {
      contaminant <- stats::runif(N) < contaminant_probability
      response[contaminant] <- stats::rbinom(sum(contaminant), 1L, 0.5)
      rt[contaminant] <- stats::runif(sum(contaminant), prepared$rt_lower, prepared$rt_upper)
    }
    data.frame(draw = k, accuracy = mean(response, na.rm = TRUE), mean_rt = mean(rt, na.rm = TRUE), median_rt = stats::median(rt, na.rm = TRUE), stringsAsFactors = FALSE)
  })
  list(observed = observed, replicated = do.call(rbind, replicated), method = "rtdists first-passage simulation")
}

#' Compare diffusion and conventional accuracy-RT models
#'
#' @param object Diffusion fit.
#' @return Comparison table.
#' @export
compare_diffusion_accuracy_rt <- function(object) {
  if (!inherits(object, "eye_gaze_diffusion_irt")) .sd_stop("Expected a gaze-diffusion fit.")
  if (is.null(object$prepared)) .sd_stop("Model comparison is unavailable for legacy approximation objects.")
  baseline <- .sd_baseline_diffusion(object$prepared)
  baseline_ll <- as.numeric(stats::logLik(baseline$accuracy)) + as.numeric(stats::logLik(baseline$timing))
  baseline_k <- length(stats::coef(baseline$accuracy)) + length(stats::coef(baseline$timing)) + 1L
  diffusion_ll <- if (inherits(object$model, "eye_gaze_diffusion_stan")) {
    log_lik <- object$model$fit$draws(variables = "log_lik", format = "matrix")
    sum(log(colMeans(exp(sweep(log_lik, 2L, apply(log_lik, 2L, max), "-")))) + apply(log_lik, 2L, max))
  } else NA_real_
  data.frame(
    model = c("gaze_diffusion", "separate_accuracy_log_rt"),
    log_likelihood_proxy = c(diffusion_ll, baseline_ll), parameters = c(NA_integer_, baseline_k),
    purpose = c("joint cognitive-process likelihood", "descriptive accuracy and timing baseline"), stringsAsFactors = FALSE
  )
}

# Simulate a single two-boundary Wiener first-passage observation. This is
# an auditable discretization fallback used only when `rtdists` is unavailable.
.sd_wiener_trial <- function(drift, boundary, nondecision, starting = 0.5, dt = 0.002, max_time = 10) {
  position <- boundary * starting
  limit <- max(1L, ceiling(max_time / dt))
  for (step in seq_len(limit)) {
    position <- position + drift * dt + sqrt(dt) * stats::rnorm(1L)
    if (position >= boundary) return(c(response = 1, response_time = nondecision + step * dt))
    if (position <= 0) return(c(response = 0, response_time = nondecision + step * dt))
  }
  c(response = as.integer(position >= boundary / 2), response_time = nondecision + max_time)
}

#' Simulate a hierarchical gaze-diffusion study
#'
#' @param n_person,n_item Number of participants/items.
#' @param trials_per_item Replications per person-item.
#' @param gaze_effect Drift effect of gaze feature.
#' @param contaminant_fraction Contaminant fraction.
#' @param time_step Time step for the built-in Wiener discretization fallback.
#' @param max_decision_time Maximum fallback decision time in seconds.
#' @param seed Seed.
#' @return Simulated trial-level data with truth attributes.
#' @export
simulate_gaze_diffusion_data <- function(n_person = 80L, n_item = 20L, trials_per_item = 1L, gaze_effect = 0.35, contaminant_fraction = 0.02, time_step = 0.002, max_decision_time = 10, seed = 1L) {
  n_person <- as.integer(n_person); n_item <- as.integer(n_item); trials_per_item <- as.integer(trials_per_item)
  if (n_person < 2L || n_item < 2L || trials_per_item < 1L) .sd_stop("Simulation requires at least two persons, two items, and one trial per item.")
  if (!is.finite(contaminant_fraction) || contaminant_fraction < 0 || contaminant_fraction >= 1) .sd_stop("`contaminant_fraction` must be in [0, 1).")
  set.seed(seed)
  grid <- expand.grid(participant_id = paste0("P", seq_len(n_person)), item_id = paste0("I", seq_len(n_item)), repetition = seq_len(trials_per_item), stringsAsFactors = FALSE)
  P <- match(grid$participant_id, unique(grid$participant_id)); J <- match(grid$item_id, unique(grid$item_id))
  theta <- stats::rnorm(n_person); difficulty <- stats::rnorm(n_item); gaze_balance <- stats::rnorm(nrow(grid))
  drift <- theta[P] - difficulty[J] + gaze_effect * gaze_balance
  boundary <- exp(0.15 + stats::rnorm(n_person, sd = 0.12)[P])
  nondecision <- 0.18 + stats::plogis(stats::rnorm(n_item, -2, 0.3)[J]) * 0.18
  starting <- stats::plogis(stats::rnorm(n_person, 0, 0.15)[P])
  simulated <- NULL
  if (requireNamespace("rtdists", quietly = TRUE)) {
    candidate <- tryCatch(rtdists::rdiffusion(nrow(grid), a = boundary, v = drift, t0 = nondecision, z = starting), error = function(e) NULL)
    if (!is.null(candidate)) {
      response_name <- intersect(c("response", "resp"), names(candidate))[1L]
      rt_name <- intersect(c("rt", "response_time"), names(candidate))[1L]
      if (!is.na(response_name) && !is.na(rt_name)) {
        response_value <- candidate[[response_name]]
        if (is.factor(response_value) || is.character(response_value)) {
          label <- tolower(trimws(as.character(response_value)))
          score <- ifelse(label %in% c("upper", "correct", "1", "true"), 1L,
            ifelse(label %in% c("lower", "error", "0", "false"), 0L, NA_integer_))
        } else {
          numeric_response <- suppressWarnings(as.numeric(response_value))
          observed_codes <- sort(unique(numeric_response[is.finite(numeric_response)]))
          if (length(observed_codes) && all(observed_codes %in% c(0, 1))) score <- as.integer(numeric_response)
          else if (length(observed_codes) && all(observed_codes %in% c(1, 2))) score <- as.integer(numeric_response - 1)
          else if (length(observed_codes) && all(observed_codes %in% c(-1, 1))) score <- as.integer(numeric_response > 0)
          else score <- rep(NA_integer_, length(numeric_response))
        }
        candidate_rt <- suppressWarnings(as.numeric(candidate[[rt_name]]))
        if (all(score %in% 0:1) && all(is.finite(candidate_rt)) && all(candidate_rt > 0)) simulated <- cbind(response = score, response_time = candidate_rt)
      }
    }
  }
  if (is.null(simulated)) {
    simulated <- t(vapply(seq_len(nrow(grid)), function(i) .sd_wiener_trial(drift[i], boundary[i], nondecision[i], starting[i], dt = time_step, max_time = max_decision_time), numeric(2)))
  }
  score <- as.integer(simulated[, "response"]); response_time <- as.numeric(simulated[, "response_time"])
  contaminant <- stats::runif(nrow(grid)) < contaminant_fraction
  if (any(contaminant)) {
    response_time[contaminant] <- stats::runif(sum(contaminant), min = max(0.05, min(nondecision)), max = max(response_time, na.rm = TRUE))
    score[contaminant] <- stats::rbinom(sum(contaminant), 1L, 0.5)
  }
  out <- data.frame(grid, score = score, response_time = response_time, gaze_balance = gaze_balance, true_drift = drift, true_boundary = boundary, true_nondecision = nondecision, true_starting = starting, contaminant = contaminant, stringsAsFactors = FALSE)
  attr(out, "truth") <- list(theta = theta, difficulty = difficulty, gaze_effect = gaze_effect, drift = drift, boundary = boundary, nondecision = nondecision, starting = starting, contaminant_fraction = contaminant_fraction)
  out
}

#' Construct a simulation-based identification study
#'
#' @param conditions Named list or data frame of design conditions.
#' @param replications Replications per condition.
#' @param base_seed Base seed.
#' @param spec Confirmatory diffusion specification used for each fit.
#' @return An executable `eye_diffusion_identification_study` bundle containing a deterministic plan and simulator, fitter, estimate, and truth extractors.
#' @export
diffusion_identification_study <- function(
  conditions = list(n_person = c(50L, 150L), n_item = c(10L, 30L), gaze_effect = c(0, 0.35), contaminant_fraction = c(0, 0.05)),
  replications = 20L, base_seed = 20260805L,
  spec = gaze_diffusion_spec(drift_features = "gaze_balance", engine = "stan")
) {
  plan <- validation_job_plan(grid = conditions, replications = replications, model_family = "gaze_diffusion", base_seed = base_seed)
  simulator <- function(n_person, n_item, gaze_effect, contaminant_fraction, seed, ...) simulate_gaze_diffusion_data(
    n_person = n_person, n_item = n_item, gaze_effect = gaze_effect,
    contaminant_fraction = contaminant_fraction, seed = seed
  )
  fitter <- function(simulation) fit_gaze_diffusion_irt(simulation, spec = spec)
  extractor <- function(fit) {
    parameters <- extract_diffusion_parameters(fit)
    if ("variable" %in% names(parameters)) names(parameters)[names(parameters) == "variable"] <- "parameter"
    if (!"parameter" %in% names(parameters)) parameters$parameter <- paste(parameters$component, parameters$term, sep = ":")
    if (!"estimate" %in% names(parameters) && "mean" %in% names(parameters)) parameters$estimate <- parameters$mean
    target <- which(parameters$parameter %in% c("beta_drift[1]", "accuracy:gaze_balance"))
    if (length(target)) {
      target <- target[1L]
      if (identical(fit$spec$engine, "stan")) {
        scale <- attr(fit$prepared$X_drift, "scale")
        scale <- if (length(scale)) as.numeric(scale[1L]) else 1
        if (!is.finite(scale) || scale <= 0) scale <- 1
        for (column in intersect(c("estimate", "sd", "q5", "q95"), names(parameters))) parameters[[column]][target] <- parameters[[column]][target] / scale
      }
      parameters$parameter[target] <- "gaze_effect"
    }
    parameters[, intersect(c("parameter", "estimate", "sd", "q5", "q95", "rhat", "ess_bulk", "ess_tail"), names(parameters)), drop = FALSE]
  }
  truth_extractor <- function(simulation) c(gaze_effect = attr(simulation, "truth")$gaze_effect)
  out <- list(plan = plan, simulator = simulator, fitter = fitter, extractor = extractor, truth_extractor = truth_extractor, spec = spec,
              metadata = list(purpose = "parameter identification, recovery, censoring, contaminants, and trade-off diagnostics"))
  class(out) <- "eye_diffusion_identification_study"
  out
}

#' @export
print.eye_diffusion_identification_study <- function(x, ...) {
  cat("Gaze-diffusion identification study\n")
  cat("Plan:         ", x$plan$plan_id, "\n", sep = "")
  cat("Jobs:         ", nrow(x$plan$jobs), "\n", sep = "")
  cat("Engine:       ", x$spec$engine, "\n", sep = "")
  cat("Target:       gaze effect on drift\n")
  cat("Evidence use: recovery and identification; not automatic model promotion\n")
  invisible(x)
}
