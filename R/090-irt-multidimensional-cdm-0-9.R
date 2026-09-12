# eyeprocess 0.9 Milestone #2: multidimensional, testlet, and cognitive-diagnosis design utilities

#' Declare a multidimensional IRT loading structure
#' @param items Item-parameter data frame or item collection.
#' @param loadings Item-by-dimension loading matrix.
#' @param dimension_names Optional names for latent dimensions.
#' @param simple_structure Whether a simple-structure loading pattern is required.
#' @export
eyeprocess_mirt_loading_spec <- function(items, loadings, dimension_names = colnames(loadings), simple_structure = FALSE) {
  items <- as.character(items); loadings <- as.matrix(loadings); storage.mode(loadings) <- "numeric"
  if (length(items) != nrow(loadings) || !ncol(loadings) || any(!is.finite(loadings))) stop("loadings must be a finite item-by-dimension matrix matching items.", call. = FALSE)
  if (is.null(dimension_names)) dimension_names <- paste0("D", seq_len(ncol(loadings)))
  if (length(dimension_names) != ncol(loadings) || anyNA(dimension_names) || anyDuplicated(dimension_names)) stop("invalid dimension_names.", call. = FALSE)
  colnames(loadings) <- dimension_names; rownames(loadings) <- items
  violations <- if (isTRUE(simple_structure)) which(rowSums(abs(loadings) > 0) > 1L) else integer()
  structure(list(items = items, loadings = loadings, dimensions = dimension_names, simple_structure = simple_structure, violations = items[violations]), class = "eye_mirt_loading_spec")
}

#' Audit multidimensional IRT loading coverage
#' @param spec Model, validation, or analysis specification object.
#' @param min_items_per_dimension Minimum number of items required per dimension.
#' @export
eyeprocess_mirt_loading_audit <- function(spec, min_items_per_dimension = 3L) {
  if (!inherits(spec, "eye_mirt_loading_spec")) stop("spec must be created by eyeprocess_mirt_loading_spec().", call. = FALSE)
  min_items_per_dimension <- as.integer(min_items_per_dimension); if (length(min_items_per_dimension) != 1L || is.na(min_items_per_dimension) || min_items_per_dimension < 1L) stop("minimum must be a positive scalar integer.", call. = FALSE)
  active <- abs(spec$loadings) > 0
  counts <- colSums(active)
  data.frame(dimension = spec$dimensions, n_loading_items = counts, meets_minimum = counts >= min_items_per_dimension, stringsAsFactors = FALSE)
}

#' Directional multidimensional 2PL information
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param discrimination Discrimination vector or matrix.
#' @param difficulty Item difficulty or location parameter.
#' @param direction Direction vector used to project multidimensional information.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_mirt_directional_information <- function(theta, discrimination, difficulty = 0, direction = NULL, D = 1) {
  theta <- as.numeric(theta); discrimination <- as.numeric(discrimination)
  if (length(theta) != length(discrimination) || !length(theta) || any(!is.finite(theta)) || any(!is.finite(discrimination))) stop("theta and discrimination must be finite vectors of equal dimension.", call. = FALSE)
  if (is.null(direction)) direction <- discrimination
  direction <- as.numeric(direction); if (length(direction) != length(theta) || any(!is.finite(direction)) || sum(direction^2) == 0) stop("invalid direction.", call. = FALSE)
  direction <- direction / sqrt(sum(direction^2)); eta <- D * (sum(discrimination * theta) - difficulty); p <- stats::plogis(eta)
  info_matrix <- (D^2) * p * (1 - p) * tcrossprod(discrimination)
  as.numeric(t(direction) %*% info_matrix %*% direction)
}

#' Multidimensional 2PL item information matrix
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param discrimination Discrimination vector or matrix.
#' @param difficulty Item difficulty or location parameter.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_mirt_information_matrix <- function(theta, discrimination, difficulty = 0, D = 1) {
  theta <- as.numeric(theta); discrimination <- as.numeric(discrimination)
  if (length(theta) != length(discrimination) || any(!is.finite(theta)) || any(!is.finite(discrimination))) stop("theta/discrimination dimension mismatch.", call. = FALSE)
  p <- stats::plogis(D * (sum(discrimination * theta) - difficulty))
  (D^2) * p * (1 - p) * tcrossprod(discrimination)
}

#' Declare a testlet structure for bifactor/two-tier IRT engines
#' @param item_id Item identifier or vector of item identifiers.
#' @param testlet Testlet membership identifier.
#' @param general_dimension General dimension name or index.
#' @export
eyeprocess_irt_testlet_spec <- function(item_id, testlet, general_dimension = "general") {
  item_id <- as.character(item_id); testlet <- as.character(testlet)
  if (length(item_id) != length(testlet) || !length(item_id) || anyNA(item_id) || anyNA(testlet) || anyDuplicated(item_id)) stop("item_id/testlet must be non-missing equal-length vectors with unique items.", call. = FALSE)
  structure(data.frame(item_id = item_id, testlet = testlet, general_dimension = general_dimension, stringsAsFactors = FALSE), class = c("eye_irt_testlet_spec", "data.frame"))
}

#' Audit testlet sizes and singleton structures
#' @param spec Model, validation, or analysis specification object.
#' @param min_items Minimum number of items required.
#' @export
eyeprocess_irt_testlet_audit <- function(spec, min_items = 2L) {
  if (!inherits(spec, "eye_irt_testlet_spec")) stop("spec must be an eye_irt_testlet_spec.", call. = FALSE)
  min_items <- as.integer(min_items); if (length(min_items) != 1L || is.na(min_items) || min_items < 1L) stop("min_items must be a positive scalar integer.", call. = FALSE); tab <- table(spec$testlet)
  data.frame(testlet = names(tab), n_items = as.integer(tab), singleton = as.integer(tab) == 1L, meets_minimum = as.integer(tab) >= min_items, stringsAsFactors = FALSE)
}

#' Build a latent-regression design matrix with explicit centering metadata
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param formula Model formula.
#' @param center_numeric Whether numeric predictors are centered.
#' @export
eyeprocess_irt_latent_regression_design <- function(data, formula, center_numeric = TRUE) {
  data <- .ep09m2_as_df(data, "data"); formula <- stats::as.formula(formula)
  mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
  mm <- stats::model.matrix(formula, data = mf)
  centers <- numeric(ncol(mm)); names(centers) <- colnames(mm)
  if (isTRUE(center_numeric)) {
    idx <- which(colnames(mm) != "(Intercept)")
    for (j in idx) if (is.numeric(mm[, j])) {
      finite <- is.finite(mm[, j])
      centers[j] <- if (any(finite)) mean(mm[finite, j]) else NA_real_
      if (is.finite(centers[j])) mm[, j] <- mm[, j] - centers[j]
    }
  }
  structure(list(matrix = mm, formula = formula, centers = centers, complete = stats::complete.cases(mm)), class = "eye_irt_latent_regression_design")
}

#' Audit a cognitive-diagnosis Q-matrix
#' @param Q Binary item-by-attribute Q-matrix.
#' @param item_ids Optional item identifiers.
#' @param attribute_names Optional names for the cognitive-diagnosis attributes.
#' @export
eyeprocess_cdm_qmatrix_audit <- function(Q, item_ids = rownames(Q), attribute_names = colnames(Q)) {
  Q <- as.matrix(Q); storage.mode(Q) <- "numeric"
  if (!nrow(Q) || !ncol(Q) || any(!Q %in% c(0, 1))) stop("Q must be a non-empty binary matrix.", call. = FALSE)
  if (is.null(item_ids)) item_ids <- paste0("item_", seq_len(nrow(Q))); if (is.null(attribute_names)) attribute_names <- paste0("A", seq_len(ncol(Q)))
  if (length(item_ids) != nrow(Q) || length(attribute_names) != ncol(Q)) stop("identifier lengths mismatch Q dimensions.", call. = FALSE)
  item_load <- rowSums(Q); attr_load <- colSums(Q)
  structure(list(item = data.frame(item_id = item_ids, n_attributes = item_load, no_attribute = item_load == 0, stringsAsFactors = FALSE),
                 attribute = data.frame(attribute = attribute_names, n_items = attr_load, unmeasured = attr_load == 0, stringsAsFactors = FALSE),
                 duplicate_rows = duplicated(Q) | duplicated(Q, fromLast = TRUE),
                 complete_identity_block = all(vapply(seq_len(ncol(Q)), function(k) {
                   target <- as.numeric(seq_len(ncol(Q)) == k)
                   any(apply(Q, 1L, function(row) identical(as.numeric(row), target)))
                 }, logical(1)))),
            class = "eye_cdm_qmatrix_audit")
}

#' Enumerate latent attribute profiles
#' @param n_attributes Number of cognitive-diagnosis attributes.
#' @param attribute_names Optional names for the cognitive-diagnosis attributes.
#' @export
eyeprocess_cdm_attribute_profiles <- function(n_attributes, attribute_names = paste0("A", seq_len(n_attributes))) {
  n_attributes <- as.integer(n_attributes); if (length(n_attributes) != 1L || is.na(n_attributes) || n_attributes < 1L || n_attributes > 20L) stop("n_attributes must be a scalar integer between 1 and 20.", call. = FALSE)
  attribute_names <- as.character(attribute_names); if (length(attribute_names) != n_attributes || anyNA(attribute_names) || any(!nzchar(attribute_names)) || anyDuplicated(attribute_names)) stop("attribute_names must be unique, non-missing, non-empty, and match n_attributes.", call. = FALSE)
  g <- expand.grid(rep(list(c(0L, 1L)), n_attributes), KEEP.OUT.ATTRS = FALSE); names(g) <- attribute_names; g$profile_id <- seq_len(nrow(g)); g[, c("profile_id", attribute_names)]
}

#' Compute deterministic DINA ideal responses from a Q-matrix
#' @param Q Binary item-by-attribute Q-matrix.
#' @param profiles Attribute mastery profiles, with rows representing profiles.
#' @export
eyeprocess_cdm_dina_ideal_response <- function(Q, profiles) {
  Q <- as.matrix(Q); profiles <- as.matrix(profiles); storage.mode(Q) <- storage.mode(profiles) <- "numeric"
  if (ncol(Q) != ncol(profiles) || any(!Q %in% c(0, 1)) || any(!profiles %in% c(0, 1))) stop("Q and profiles must be compatible binary matrices.", call. = FALSE)
  out <- sapply(seq_len(nrow(Q)), function(j) rowSums(sweep(profiles, 2L, Q[j, ], `*`)) == sum(Q[j, ]))
  storage.mode(out) <- "integer"; out
}

#' DINA response probabilities from slip and guess parameters
#' @param ideal_response Ideal-response indicator or matrix implied by the cognitive-diagnosis model.
#' @param slip DINA slip parameter or vector of slip parameters.
#' @param guess DINA guessing parameter or vector of guessing parameters.
#' @export
eyeprocess_cdm_dina_probability <- function(ideal_response, slip = 0.1, guess = 0.2) {
  eta <- as.matrix(ideal_response); if (any(!eta %in% c(0, 1))) stop("ideal_response must be binary.", call. = FALSE)
  slip <- as.numeric(slip); guess <- as.numeric(guess)
  if (length(slip) == 1L) slip <- rep(slip, ncol(eta)); if (length(guess) == 1L) guess <- rep(guess, ncol(eta))
  if (length(slip) != ncol(eta) || length(guess) != ncol(eta) || any(!is.finite(slip)) || any(!is.finite(guess)) || any(slip < 0 | slip >= 1) || any(guess < 0 | guess >= 1)) stop("invalid slip/guess.", call. = FALSE)
  out <- eta
  for (j in seq_len(ncol(eta))) out[, j] <- ifelse(eta[, j] == 1, 1 - slip[j], guess[j])
  out
}

#' Summarise CDM classification uncertainty from profile probabilities
#' @param profile_probabilities Posterior attribute-profile probabilities.
#' @export
eyeprocess_cdm_classification_uncertainty <- function(profile_probabilities) {
  p <- as.matrix(profile_probabilities); storage.mode(p) <- "numeric"
  if (!nrow(p) || ncol(p) < 2L || any(!is.finite(p)) || any(p < 0) || any(abs(rowSums(p) - 1) > 1e-6)) stop("profile probabilities must have at least two columns, be non-negative, finite, and row-normalized.", call. = FALSE)
  entropy <- -rowSums(ifelse(p > 0, p * log(p), 0)); norm_entropy <- entropy / log(ncol(p)); max_prob <- apply(p, 1L, max)
  data.frame(case = seq_len(nrow(p)), max_probability = max_prob, entropy = entropy, normalized_entropy = norm_entropy, stringsAsFactors = FALSE)
}

#' @export
print.eye_cdm_qmatrix_audit <- function(x, ...) {
  cat("eyeprocess CDM Q-matrix audit\n")
  cat("  items     :", nrow(x$item), "\n")
  cat("  attributes:", nrow(x$attribute), "\n")
  cat("  empty items:", sum(x$item$no_attribute), "| unmeasured attributes:", sum(x$attribute$unmeasured), "\n")
  invisible(x)
}
