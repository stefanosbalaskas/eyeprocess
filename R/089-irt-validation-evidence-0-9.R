# eyeprocess 0.9 Milestone #2: IRT simulation, recovery, SBC, and misspecification evidence

#' Simulate dichotomous IRT responses with optional local dependence and missingness
#' @param n_persons Number of persons.
#' @param items Item-parameter data frame or item collection.
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param missing_rate Proportion of responses or observations set missing.
#' @param testlet_sd Standard deviation of simulated testlet effects.
#' @param seed Random-number seed for reproducible execution.
#' @param D Logistic scaling constant.
#' @export
simulate_eyeprocess_irt_binary <- function(n_persons = 500L, items, theta = NULL, missing_rate = 0, testlet_sd = 0, seed = 1L, D = 1) {
  n_persons <- as.integer(n_persons); seed <- as.integer(seed); items <- .ep09m2_item_pars(items)
  missing_rate <- as.numeric(missing_rate); testlet_sd <- as.numeric(testlet_sd)
  if (length(n_persons) != 1L || is.na(n_persons) || n_persons < 20L || length(seed) != 1L || is.na(seed) || seed < 1L) stop("n_persons >= 20 and positive scalar seed required.", call. = FALSE)
  if (length(missing_rate) != 1L || !is.finite(missing_rate) || missing_rate < 0 || missing_rate >= 1 || length(testlet_sd) != 1L || !is.finite(testlet_sd) || testlet_sd < 0) stop("invalid scalar missing_rate/testlet_sd.", call. = FALSE)
  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({ if (is.null(old)) { if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old, envir = .GlobalEnv) }, add = TRUE)
  set.seed(seed)
  if (is.null(theta)) theta <- stats::rnorm(n_persons)
  theta <- as.numeric(theta); if (length(theta) != n_persons || any(!is.finite(theta))) stop("theta must be finite and length n_persons.", call. = FALSE)
  testlet <- if (testlet_sd > 0) stats::rnorm(n_persons, 0, testlet_sd) else rep(0, n_persons)
  p <- sapply(seq_len(nrow(items)), function(j) eyeprocess_irt_4pl_probability(theta + testlet, items$a[j], items$b[j], items$c[j], items$d[j], D))
  y <- matrix(stats::rbinom(length(p), 1, as.vector(p)), nrow = n_persons, ncol = nrow(items))
  colnames(y) <- items$item_id; rownames(y) <- paste0("P", seq_len(n_persons))
  if (missing_rate > 0) y[matrix(stats::runif(length(y)) < missing_rate, nrow(y), ncol(y))] <- NA
  structure(list(responses = y, probabilities = p, theta = theta, items = items, missing_rate = missing_rate, testlet_sd = testlet_sd, seed = seed), class = "eye_irt_simulation")
}

#' Create an IRT recovery design
#' @param sample_size Validation sample size or vector of sample sizes.
#' @param n_items Number of items.
#' @param missing_rate Proportion of responses or observations set missing.
#' @param testlet_sd Standard deviation of simulated testlet effects.
#' @param replications Number of simulation or validation replications.
#' @param seed Random-number seed for reproducible execution.
#' @export
eyeprocess_irt_recovery_design <- function(sample_size = c(250L, 750L), n_items = c(12L, 24L), missing_rate = c(0, .15), testlet_sd = c(0, .35), replications = 10L, seed = 20260811L) {
  sample_size <- as.integer(sample_size); n_items <- as.integer(n_items); missing_rate <- as.numeric(missing_rate); testlet_sd <- as.numeric(testlet_sd)
  replications <- as.integer(replications); seed <- as.integer(seed)
  if (!length(sample_size) || anyNA(sample_size) || any(sample_size < 20L) || !length(n_items) || anyNA(n_items) || any(n_items < 4L) || !length(missing_rate) || any(!is.finite(missing_rate)) || any(missing_rate < 0 | missing_rate >= 1) || !length(testlet_sd) || any(!is.finite(testlet_sd)) || any(testlet_sd < 0) || length(replications) != 1L || is.na(replications) || replications < 1L || length(seed) != 1L || is.na(seed) || seed < 1L) stop("invalid recovery design.", call. = FALSE)
  grid <- expand.grid(sample_size = sample_size, n_items = n_items, missing_rate = missing_rate, testlet_sd = testlet_sd, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  grid$scenario_id <- sprintf("IRTREC%03d", seq_len(nrow(grid))); grid$replications <- replications; grid$seed <- seed
  structure(grid[, c("scenario_id", "sample_size", "n_items", "missing_rate", "testlet_sd", "replications", "seed")], class = c("eye_irt_recovery_design", "data.frame"))
}

.ep09m2_default_item_truth <- function(n_items, seed) {
  n_items <- as.integer(n_items); seed <- as.integer(seed)
  if (length(n_items) != 1L || is.na(n_items) || n_items < 1L || length(seed) != 1L || is.na(seed) || seed < 1L) stop("n_items and seed must be positive integers.", call. = FALSE)
  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({ if (is.null(old)) { if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old, envir = .GlobalEnv) }, add = TRUE)
  set.seed(seed)
  data.frame(item_id = paste0("I", seq_len(n_items)), a = exp(stats::rnorm(n_items, log(1.2), .18)), b = stats::rnorm(n_items, 0, 1), c = 0, d = 1, stringsAsFactors = FALSE)
}

.ep09m2_extract_mirt_irtpars <- function(fit) {
  co <- mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)
  items <- as.data.frame(co$items)
  items$item_id <- rownames(items)
  a_col <- intersect(c("a", "a1"), names(items))[1L]
  if (is.na(a_col)) stop("Could not identify mirt discrimination column.", call. = FALSE)
  a <- as.numeric(items[[a_col]])
  if ("b" %in% names(items)) {
    b <- as.numeric(items[["b"]])
  } else if ("d" %in% names(items)) {
    d <- as.numeric(items[["d"]])
    if (any(!is.finite(a)) || any(a == 0)) stop("Cannot convert mirt intercept d to difficulty b with non-finite/zero discrimination.", call. = FALSE)
    b <- -d / a
  } else {
    stop("Could not identify mirt difficulty/intercept column.", call. = FALSE)
  }
  if (any(!is.finite(a)) || any(a <= 0) || any(!is.finite(b))) stop("mirt returned non-finite or non-positive IRT parameters.", call. = FALSE)
  data.frame(item_id = items$item_id, a = a, b = b, stringsAsFactors = FALSE)
}

#' Run IRT parameter recovery with the exact mirt engine
#'
#' When mirt is unavailable the function returns a gated result rather than a substitute estimator.
#' @param design Validation or simulation design object.
#' @param engine Requested estimation or analysis engine.
#' @param verbose Value supplied for the verbose argument.
#' @export
run_eyeprocess_irt_recovery <- function(design, engine = "mirt", verbose = TRUE) {
  if (!inherits(design, "eye_irt_recovery_design")) stop("design must come from eyeprocess_irt_recovery_design().", call. = FALSE)
  if (!identical(engine, "mirt")) stop("Milestone #2 recovery currently requires engine='mirt'.", call. = FALSE)
  if (!requireNamespace("mirt", quietly = TRUE)) return(.ep09m2_gated_engine("mirt", match.call(), "mirt is required for IRT parameter-recovery evidence."))
  rows <- list(); failures <- list(); k <- 0L; f <- 0L
  for (s in seq_len(nrow(design))) {
    sc <- design[s, , drop = FALSE]
    for (r in seq_len(sc$replications)) {
      seed <- eyeprocess_validation_seed(sc$seed, s, r)
      truth <- .ep09m2_default_item_truth(sc$n_items, seed)
      sim <- simulate_eyeprocess_irt_binary(sc$sample_size, truth, missing_rate = sc$missing_rate, testlet_sd = sc$testlet_sd, seed = seed + 17L)
      if (isTRUE(verbose)) message(sc$scenario_id, " replication ", r, "/", sc$replications)
      fit <- try(mirt::mirt(sim$responses, 1, itemtype = "2PL", verbose = FALSE), silent = TRUE)
      if (inherits(fit, "try-error")) {
        f <- f + 1L; failures[[f]] <- data.frame(scenario_id = sc$scenario_id, replication = r, seed = seed, error = as.character(fit), stringsAsFactors = FALSE)
        next
      }
      est <- try(.ep09m2_extract_mirt_irtpars(fit), silent = TRUE)
      if (inherits(est, "try-error")) {
        f <- f + 1L; failures[[f]] <- data.frame(scenario_id = sc$scenario_id, replication = r, seed = seed, error = as.character(est), stringsAsFactors = FALSE)
        next
      }
      z <- merge(truth[, c("item_id", "a", "b")], est, by = "item_id", suffixes = c("_truth", "_estimate"))
      z$scenario_id <- sc$scenario_id; z$replication <- r; z$seed <- seed; z$sample_size <- sc$sample_size; z$n_items <- sc$n_items; z$missing_rate <- sc$missing_rate; z$testlet_sd <- sc$testlet_sd
      k <- k + 1L; rows[[k]] <- z
    }
  }
  estimates <- if (length(rows)) do.call(rbind, rows) else data.frame()
  failure_tab <- if (length(failures)) do.call(rbind, failures) else data.frame(scenario_id = character(), replication = integer(), seed = integer(), error = character())
  structure(list(design = design, estimates = estimates, failures = failure_tab, engine = engine), class = "eye_irt_recovery_result")
}

#' Summarise IRT parameter recovery
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @export
eyeprocess_irt_recovery_summary <- function(x) {
  if (!inherits(x, "eye_irt_recovery_result")) stop("x must be an eye_irt_recovery_result.", call. = FALSE)
  if (!nrow(x$estimates)) return(data.frame())
  d <- x$estimates
  params <- c("a", "b")
  keys <- interaction(d$scenario_id, drop = TRUE)
  rows <- split(seq_len(nrow(d)), keys)
  do.call(rbind, unlist(lapply(rows, function(ii) lapply(params, function(p) {
    truth <- d[[paste0(p, "_truth")]][ii]; est <- d[[paste0(p, "_estimate")]][ii]; err <- est - truth
    data.frame(scenario_id = d$scenario_id[ii[1L]], parameter = p, n = sum(is.finite(err)), bias = .ep09m2_finite_mean(err), rmse = sqrt(.ep09m2_finite_mean(err^2)), mae = .ep09m2_finite_mean(abs(err)), correlation = if (sum(is.finite(truth) & is.finite(est)) >= 3L) suppressWarnings(stats::cor(truth, est, use = "complete.obs")) else NA_real_, stringsAsFactors = FALSE)
  })), recursive = FALSE))
}

#' Summarise recovery failure rates
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @export
eyeprocess_irt_recovery_failures <- function(x) {
  if (!inherits(x, "eye_irt_recovery_result")) stop("x must be an eye_irt_recovery_result.", call. = FALSE)
  des <- x$design
  out <- lapply(seq_len(nrow(des)), function(i) {
    id <- des$scenario_id[i]; nf <- sum(x$failures$scenario_id == id); data.frame(scenario_id = id, attempted = des$replications[i], failures = nf, failure_rate = nf / des$replications[i], stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

#' Construct SBC ranks from scalar truths and posterior draws
#' @param truth Known simulated parameter value or vector of true values.
#' @param draws Posterior draws, with draws arranged by simulation case as required.
#' @param randomize_ties Whether ties in SBC ranks are randomized.
#' @param seed Random-number seed for reproducible execution.
#' @export
eyeprocess_irt_sbc_ranks <- function(truth, draws, randomize_ties = TRUE, seed = 1L) {
  truth <- as.numeric(truth); draws <- as.matrix(draws); storage.mode(draws) <- "numeric"
  if (nrow(draws) != length(truth) || any(!is.finite(truth)) || any(!is.finite(draws))) stop("draws rows must match finite truths and contain finite draws.", call. = FALSE)
  seed <- as.integer(seed); if (length(seed) != 1L || seed < 1L || is.na(seed)) stop("seed must be positive.", call. = FALSE)
  if (ncol(draws) < 1L) stop("draws must contain at least one posterior draw per truth.", call. = FALSE)
  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({ if (is.null(old)) { if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old, envir = .GlobalEnv) }, add = TRUE)
  set.seed(seed)
  vapply(seq_along(truth), function(i) {
    less <- sum(draws[i, ] < truth[i]); equal <- sum(draws[i, ] == truth[i])
    if (isTRUE(randomize_ties) && equal > 0L) less + sample.int(equal + 1L, 1L) - 1L else less
  }, integer(1))
}

#' Run simulation-based calibration for known-item IRT ability scoring
#'
#' Simulates abilities from the declared normal prior, responses from the known
#' item-response model, and posterior draws from the same grid-based scoring
#' algorithm used by `eyeprocess_irt_eap_score()`. This validates computational
#' calibration of the scoring workflow under the declared generative model; it
#' does not establish empirical adequacy or construct validity.
#' @param items Item-parameter data frame or item collection.
#' @param replications Number of simulation or validation replications.
#' @param posterior_draws Number of posterior draws generated per SBC replication.
#' @param theta_grid Grid of latent-trait values used for numerical scoring or integration.
#' @param prior_mean Mean of the normal latent-trait prior.
#' @param prior_sd Standard deviation of the normal latent-trait prior.
#' @param interval Central posterior interval probability used for coverage assessment.
#' @param seed Random-number seed for reproducible execution.
#' @param D Logistic scaling constant.
#' @export
run_eyeprocess_irt_ability_sbc <- function(items, replications = 200L, posterior_draws = 99L,
                                           theta_grid = seq(-5, 5, length.out = 401),
                                           prior_mean = 0, prior_sd = 1,
                                           interval = 0.95, seed = 20260811L, D = 1) {
  items <- .ep09m2_item_pars(items)
  replications <- as.integer(replications); posterior_draws <- as.integer(posterior_draws); seed <- as.integer(seed)
  theta_grid <- .ep09m2_theta(theta_grid); prior_mean <- as.numeric(prior_mean); prior_sd <- as.numeric(prior_sd); interval <- as.numeric(interval)
  if (length(replications) != 1L || is.na(replications) || replications < 20L) stop("replications must be a scalar integer >= 20.", call. = FALSE)
  if (length(posterior_draws) != 1L || is.na(posterior_draws) || posterior_draws < 9L) stop("posterior_draws must be a scalar integer >= 9.", call. = FALSE)
  if (length(seed) != 1L || is.na(seed) || seed < 1L) stop("seed must be a positive scalar integer.", call. = FALSE)
  if (length(theta_grid) < 101L || is.unsorted(theta_grid, strictly = TRUE)) stop("theta_grid must be strictly increasing with at least 101 points.", call. = FALSE)
  if (length(prior_mean) != 1L || !is.finite(prior_mean) || length(prior_sd) != 1L || !is.finite(prior_sd) || prior_sd <= 0) stop("invalid normal prior.", call. = FALSE)
  if (length(interval) != 1L || !is.finite(interval) || interval <= 0 || interval >= 1) stop("interval must lie in (0,1).", call. = FALSE)
  old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({ if (is.null(old)) { if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv) } else assign(".Random.seed", old, envir = .GlobalEnv) }, add = TRUE)
  set.seed(seed)
  alpha <- (1 - interval) / 2
  rows <- vector("list", replications)
  rank <- integer(replications)
  for (i in seq_len(replications)) {
    truth <- stats::rnorm(1L, prior_mean, prior_sd)
    p <- vapply(seq_len(nrow(items)), function(j) eyeprocess_irt_4pl_probability(truth, items$a[j], items$b[j], items$c[j], items$d[j], D), numeric(1))
    response <- stats::rbinom(nrow(items), 1L, p)
    score <- eyeprocess_irt_eap_score(response, items, theta_grid = theta_grid, prior_mean = prior_mean, prior_sd = prior_sd, D = D)
    posterior_sample <- sample(score$theta, size = posterior_draws, replace = TRUE, prob = score$posterior)
    rank[[i]] <- sum(posterior_sample < truth)
    cdf <- cumsum(score$posterior)
    lower <- theta_grid[which(cdf >= alpha)[1L]]
    upper <- theta_grid[which(cdf >= 1 - alpha)[1L]]
    rows[[i]] <- data.frame(replication = i, truth = truth, estimate = score$estimate, se = score$se,
                            lower = lower, upper = upper, covered = truth >= lower && truth <= upper,
                            raw_score = sum(response), stringsAsFactors = FALSE)
  }
  details <- do.call(rbind, rows)
  out <- eyeprocess_irt_sbc_summary(rank, n_draws = posterior_draws)
  out$coverage <- mean(details$covered)
  out$nominal_coverage <- interval
  out$coverage_error <- out$coverage - interval
  out$details <- details
  out$seed <- seed
  out$method <- "known-item grid-posterior ability SBC"
  out$guardrail <- "SBC validates computational calibration under the declared generative model; it does not establish construct validity or empirical model adequacy."
  out
}

#' Summarise IRT SBC ranks with the package SBC diagnostics
#' @param ranks Simulation-based-calibration rank values.
#' @param n_draws Number of posterior draws underlying each rank.
#' @param bins Number of bins used for rank-distribution summaries.
#' @export
eyeprocess_irt_sbc_summary <- function(ranks, n_draws, bins = NULL) {
  if (is.null(bins)) bins <- min(as.integer(n_draws) + 1L, 20L)
  diag <- sbc_rank_diagnostics(ranks, n_draws = n_draws, bins = bins)
  structure(list(diagnostics = diag, ecdf_deviation = sbc_ecdf_deviation(diag), n = length(ranks), n_draws = n_draws), class = "eye_irt_sbc_evidence")
}

#' Create a model-misspecification suite
#' @export
eyeprocess_irt_misspecification_suite <- function() {
  data.frame(
    scenario = c("reference", "local_dependence", "missingness", "discrimination_heterogeneity", "lower_asymptote", "latent_mixture"),
    perturbation = c("none", "testlet random effect", "MCAR omission", "wider log-discrimination", "non-zero lower asymptote", "two-component theta mixture"),
    target = c("calibration baseline", "local independence", "missing-data robustness", "item heterogeneity", "guessing sensitivity", "latent distribution sensitivity"),
    stringsAsFactors = FALSE
  )
}

#' Compare recovery under reference and misspecified scenarios
#' @param reference_summary Reference-model validation summary.
#' @param misspecified_summary Misspecified-model validation summary.
#' @export
eyeprocess_irt_misspecification_metrics <- function(reference_summary, misspecified_summary) {
  reference_summary <- .ep09m2_as_df(reference_summary, "reference_summary"); misspecified_summary <- .ep09m2_as_df(misspecified_summary, "misspecified_summary")
  .ep09m2_req_cols(reference_summary, c("parameter", "bias", "rmse"), "reference_summary"); .ep09m2_req_cols(misspecified_summary, c("parameter", "bias", "rmse"), "misspecified_summary")
  z <- merge(reference_summary[, c("parameter", "bias", "rmse")], misspecified_summary[, c("parameter", "bias", "rmse")], by = "parameter", suffixes = c("_reference", "_misspecified"))
  z$rmse_inflation <- z$rmse_misspecified - z$rmse_reference; z$absolute_bias_inflation <- abs(z$bias_misspecified) - abs(z$bias_reference)
  z
}

#' Freeze IRT validation reference summaries
#' @param recovery_summary Parameter-recovery summary object or table.
#' @param sbc Simulation-based-calibration evidence object or table.
#' @param failures Failure records or failure summary.
#' @param metadata Named metadata to store with the frozen object.
#' @export
freeze_eyeprocess_irt_reference <- function(recovery_summary = NULL, sbc = NULL, failures = NULL, metadata = list()) {
  obj <- list(recovery_summary = recovery_summary, sbc = sbc, failures = failures, metadata = metadata,
              scientific_scope = "software-validation reference; not construct-validity evidence")
  obj$hash <- .ep09m2_hash(obj)
  class(obj) <- "eye_irt_validation_reference"
  obj
}

#' @export
print.eye_irt_recovery_result <- function(x, ...) {
  cat("eyeprocess IRT recovery result\n")
  cat("  scenarios:", nrow(x$design), "\n")
  cat("  estimate rows:", nrow(x$estimates), "\n")
  cat("  failures:", nrow(x$failures), "\n")
  invisible(x)
}
