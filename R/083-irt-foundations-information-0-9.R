# eyeprocess 0.9 Milestone #2: transparent IRT mathematics and information

.ep09m2_logistic <- function(x) stats::plogis(x)

.ep09m2_softmax <- function(eta) {
  eta <- as.numeric(eta)
  eta <- eta - max(eta)
  z <- exp(eta)
  z / sum(z)
}

.ep09m2_theta <- function(theta) {
  theta <- as.numeric(theta)
  if (!length(theta) || any(!is.finite(theta))) stop("theta must contain finite numeric values.", call. = FALSE)
  theta
}

.ep09m2_item_pars <- function(items) {
  items <- .ep09m2_as_df(items, "items")
  .ep09m2_req_cols(items, c("item_id", "a", "b"), "items")
  if (anyNA(items$item_id) || anyDuplicated(items$item_id)) stop("item_id must be unique and non-missing.", call. = FALSE)
  if (any(!is.finite(items$a)) || any(items$a <= 0) || any(!is.finite(items$b))) stop("a must be positive finite and b finite.", call. = FALSE)
  if (!"c" %in% names(items)) items$c <- 0
  if (!"d" %in% names(items)) items$d <- 1
  if (any(!is.finite(items$c)) || any(!is.finite(items$d)) || any(items$c < 0 | items$c >= 1) || any(items$d <= 0 | items$d > 1) || any(items$c >= items$d))
    stop("c and d must satisfy 0 <= c < d <= 1.", call. = FALSE)
  items
}

#' Declare an eyeprocess IRT model specification
#'
#' This specification records a psychometric model contract. Estimation is delegated
#' to explicit engines where required; the specification itself does not fit a model.
#' @param family IRT response family or model family.
#' @param dimensions Value supplied for the dimensions argument.
#' @param identification Identification specification or identification audit.
#' @param engine Requested estimation or analysis engine.
#' @param process_channels Declared process-measure channels.
#' @param status Evidence, model, or governance status.
#' @param notes Value supplied for the notes argument.
#' @export
eyeprocess_irt_model_spec <- function(
    family = c("rasch", "2pl", "3pl", "4pl", "grm", "gpcm", "nominal", "multidimensional", "testlet", "latent_regression", "cdm", "joint_rt"),
    dimensions = 1L,
    identification = c("theta_standard", "item_sum_zero", "anchor"),
    engine = c("native_math", "mirt", "TAM", "GDINA", "LNIRT", "eRm", "custom"),
    process_channels = character(),
    status = c("reference", "experimental", "gated"),
    notes = NULL) {
  family <- match.arg(family)
  identification <- match.arg(identification)
  engine <- match.arg(engine)
  status <- match.arg(status)
  dimensions <- as.integer(dimensions)
  if (length(dimensions) != 1L || is.na(dimensions) || dimensions < 1L) stop("dimensions must be a positive integer.", call. = FALSE)
  process_channels <- unique(as.character(process_channels))
  if (anyNA(process_channels)) stop("process_channels cannot contain missing values.", call. = FALSE)
  if (!is.null(notes) && (length(notes) != 1L || is.na(notes))) stop("notes must be NULL or a scalar string.", call. = FALSE)
  structure(list(family = family, dimensions = dimensions, identification = identification, engine = engine,
                 process_channels = process_channels, status = status, notes = notes), class = "eyeprocess_irt_model_spec")
}

#' Validate an eyeprocess IRT model specification
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @export
validate_eyeprocess_irt_model_spec <- function(x) {
  if (!inherits(x, "eyeprocess_irt_model_spec")) stop("x must inherit from eyeprocess_irt_model_spec.", call. = FALSE)
  required <- c("family", "dimensions", "identification", "engine", "process_channels", "status")
  miss <- setdiff(required, names(x))
  if (length(miss)) stop("IRT specification is missing fields: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

#' Audit IRT scale/location identification
#' @param spec Model, validation, or analysis specification object.
#' @param constraints Identification or model constraints.
#' @param n_items Number of items.
#' @param n_persons Number of persons.
#' @export
eyeprocess_irt_identification_audit <- function(spec, constraints = list(), n_items = NULL, n_persons = NULL) {
  validate_eyeprocess_irt_model_spec(spec)
  if (!is.list(constraints)) stop("constraints must be a list.", call. = FALSE)
  fixed_theta_mean <- isTRUE(constraints$theta_mean_fixed)
  fixed_theta_sd <- isTRUE(constraints$theta_sd_fixed)
  sum_zero_items <- isTRUE(constraints$item_difficulty_sum_zero)
  anchors <- if (is.null(constraints$anchor_items)) character() else unique(as.character(constraints$anchor_items))
  location_identified <- fixed_theta_mean || sum_zero_items || length(anchors) > 0L
  scale_identified <- fixed_theta_sd || length(anchors) >= 2L || spec$family == "rasch"
  if (spec$identification == "theta_standard") {
    location_identified <- location_identified || fixed_theta_mean
    scale_identified <- scale_identified || fixed_theta_sd
  }
  warnings <- character()
  if (!location_identified) warnings <- c(warnings, "No explicit location constraint was supplied.")
  if (!scale_identified) warnings <- c(warnings, "No explicit scale constraint was supplied.")
  if (!is.null(n_items)) {
    n_items <- as.numeric(n_items)
    if (length(n_items) != 1L || !is.finite(n_items) || n_items < 0) stop("n_items must be NULL or a finite non-negative scalar.", call. = FALSE)
    if (n_items < 3L * spec$dimensions) warnings <- c(warnings, "Few items relative to declared dimensionality; identification may be weak.")
  }
  if (!is.null(n_persons)) {
    n_persons <- as.numeric(n_persons)
    if (length(n_persons) != 1L || !is.finite(n_persons) || n_persons < 0) stop("n_persons must be NULL or a finite non-negative scalar.", call. = FALSE)
  }
  structure(list(spec = spec, location_identified = location_identified, scale_identified = scale_identified,
                 anchors = anchors, warnings = warnings, valid = location_identified && scale_identified,
                 n_items = n_items, n_persons = n_persons), class = "eye_irt_identification_audit")
}

#' Audit sparse person-item response coverage
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param person Name of the person identifier column.
#' @param item Name of the item identifier column.
#' @param response Observed item response or response variable.
#' @param min_person_items Minimum number of observed items required per person.
#' @param min_item_persons Minimum number of observed persons required per item.
#' @export
eyeprocess_irt_sparse_design_audit <- function(data, person, item, response = NULL, min_person_items = 3L, min_item_persons = 10L) {
  data <- .ep09m2_as_df(data, "data")
  cols <- c(person, item, response)
  cols <- cols[!is.null(cols) & nzchar(cols)]
  .ep09m2_req_cols(data, cols, "data")
  if (length(person) != 1L || length(item) != 1L || is.na(person) || is.na(item) || !nzchar(person) || !nzchar(item)) stop("person and item must be non-empty scalar column names.", call. = FALSE)
  if (!is.null(response) && (length(response) != 1L || is.na(response) || !nzchar(response))) stop("response must be NULL or a non-empty scalar column name.", call. = FALSE)
  min_person_items <- as.integer(min_person_items); min_item_persons <- as.integer(min_item_persons)
  if (length(min_person_items) != 1L || is.na(min_person_items) || min_person_items < 1L || length(min_item_persons) != 1L || is.na(min_item_persons) || min_item_persons < 1L) stop("minimum counts must be positive scalar integers.", call. = FALSE)
  keep <- if (is.null(response)) rep(TRUE, nrow(data)) else !is.na(data[[response]])
  d <- data[keep, c(person, item), drop = FALSE]
  ptab <- table(d[[person]]); itab <- table(d[[item]])
  possible <- length(unique(data[[person]])) * length(unique(data[[item]]))
  observed <- nrow(d)
  structure(list(
    n_persons = length(ptab), n_items = length(itab), n_observed = observed,
    density = if (possible) observed / possible else NA_real_,
    person_counts = data.frame(person_id = names(ptab), n_items = as.integer(ptab), stringsAsFactors = FALSE),
    item_counts = data.frame(item_id = names(itab), n_persons = as.integer(itab), stringsAsFactors = FALSE),
    sparse_persons = names(ptab)[ptab < min_person_items], sparse_items = names(itab)[itab < min_item_persons],
    min_person_items = min_person_items, min_item_persons = min_item_persons
  ), class = "eye_irt_sparse_design_audit")
}

#' 2PL item-response probability
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param a Item discrimination parameter or vector.
#' @param b Item difficulty or location parameter or vector.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_2pl_probability <- function(theta, a = 1, b = 0, D = 1) {
  theta <- .ep09m2_theta(theta); a <- as.numeric(a); b <- as.numeric(b); D <- as.numeric(D)
  if (length(a) != 1L || !is.finite(a) || a <= 0 || length(b) != 1L || !is.finite(b) || length(D) != 1L || !is.finite(D) || D <= 0)
    stop("a and D must be positive finite scalars; b must be finite.", call. = FALSE)
  .ep09m2_logistic(D * a * (theta - b))
}

#' 3PL item-response probability
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param a Item discrimination parameter or vector.
#' @param b Item difficulty or location parameter or vector.
#' @param c Lower-asymptote parameter or vector.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_3pl_probability <- function(theta, a = 1, b = 0, c = 0.2, D = 1) {
  c <- as.numeric(c)
  if (length(c) != 1L || !is.finite(c) || c < 0 || c >= 1) stop("c must lie in [0,1).", call. = FALSE)
  c + (1 - c) * eyeprocess_irt_2pl_probability(theta, a = a, b = b, D = D)
}

#' 4PL item-response probability
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param a Item discrimination parameter or vector.
#' @param b Item difficulty or location parameter or vector.
#' @param c Lower-asymptote parameter or vector.
#' @param d Upper-asymptote parameter or vector.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_4pl_probability <- function(theta, a = 1, b = 0, c = 0, d = 1, D = 1) {
  c <- as.numeric(c); d <- as.numeric(d)
  if (length(c) != 1L || length(d) != 1L || !is.finite(c) || !is.finite(d) || c < 0 || d > 1 || c >= d)
    stop("c and d must satisfy 0 <= c < d <= 1.", call. = FALSE)
  c + (d - c) * eyeprocess_irt_2pl_probability(theta, a = a, b = b, D = D)
}

#' Graded-response category probabilities
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param a Item discrimination parameter or vector.
#' @param thresholds Ordered response-category thresholds.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_grm_probability <- function(theta, a = 1, thresholds, D = 1) {
  theta <- .ep09m2_theta(theta); thresholds <- as.numeric(thresholds)
  if (!length(thresholds) || any(!is.finite(thresholds)) || is.unsorted(thresholds, strictly = TRUE)) stop("thresholds must be finite and strictly increasing.", call. = FALSE)
  a <- as.numeric(a); D <- as.numeric(D)
  if (length(a) != 1L || !is.finite(a) || a <= 0 || length(D) != 1L || !is.finite(D) || D <= 0) stop("a and D must be positive finite scalars.", call. = FALSE)
  out <- t(vapply(theta, function(th) {
    ge <- .ep09m2_logistic(D * a * (th - thresholds))
    p <- c(1 - ge[1L], if (length(ge) > 1L) ge[-length(ge)] - ge[-1L] else numeric(), ge[length(ge)])
    pmax(0, p) / sum(pmax(0, p))
  }, numeric(length(thresholds) + 1L)))
  colnames(out) <- paste0("category_", 0:length(thresholds))
  out
}

#' Generalized partial-credit category probabilities
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param a Item discrimination parameter or vector.
#' @param steps Step parameters for a generalized partial-credit model.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_gpcm_probability <- function(theta, a = 1, steps, D = 1) {
  theta <- .ep09m2_theta(theta); steps <- as.numeric(steps)
  if (!length(steps) || any(!is.finite(steps))) stop("steps must be finite and non-empty.", call. = FALSE)
  a <- as.numeric(a); D <- as.numeric(D)
  if (length(a) != 1L || !is.finite(a) || a <= 0 || length(D) != 1L || !is.finite(D) || D <= 0) stop("a and D must be positive finite scalars.", call. = FALSE)
  out <- t(vapply(theta, function(th) {
    eta <- c(0, cumsum(D * a * (th - steps)))
    .ep09m2_softmax(eta)
  }, numeric(length(steps) + 1L)))
  colnames(out) <- paste0("category_", 0:length(steps))
  out
}

#' Nominal-response category probabilities
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param slopes Nominal-category slope parameters.
#' @param intercepts Nominal-category intercept parameters.
#' @export
eyeprocess_irt_nominal_probability <- function(theta, slopes, intercepts) {
  theta <- .ep09m2_theta(theta); slopes <- as.numeric(slopes); intercepts <- as.numeric(intercepts)
  if (length(slopes) < 2L || length(slopes) != length(intercepts) || any(!is.finite(slopes)) || any(!is.finite(intercepts)))
    stop("slopes and intercepts must be finite vectors of equal length >= 2.", call. = FALSE)
  out <- t(vapply(theta, function(th) .ep09m2_softmax(intercepts + slopes * th), numeric(length(slopes))))
  colnames(out) <- paste0("category_", seq_along(slopes) - 1L)
  out
}

.ep09m2_numeric_information <- function(prob_fun, theta, h = 1e-5) {
  theta <- .ep09m2_theta(theta)
  t(vapply(theta, function(th) {
    p0 <- as.numeric(prob_fun(th)); pp <- as.numeric(prob_fun(th + h)); pm <- as.numeric(prob_fun(th - h))
    dp <- (pp - pm) / (2 * h)
    p0 <- pmax(p0, .Machine$double.eps)
    sum((dp^2) / p0)
  }, numeric(1L)))
}

#' Compute item information for transparent IRT families
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param family IRT response family or model family.
#' @param ... Additional arguments passed to the selected method or external engine.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_item_information <- function(theta, family = c("2pl", "3pl", "4pl", "grm", "gpcm", "nominal"), ..., D = 1) {
  family <- match.arg(family); theta <- .ep09m2_theta(theta); dots <- list(...)
  if (family %in% c("2pl", "3pl", "4pl")) {
    a <- if (is.null(dots$a)) 1 else dots$a; b <- if (is.null(dots$b)) 0 else dots$b
    c0 <- if (is.null(dots$c)) 0 else dots$c; d0 <- if (is.null(dots$d)) 1 else dots$d
    if (family == "3pl") {
      c0 <- as.numeric(c0)
      if (length(c0) != 1L || !is.finite(c0) || c0 < 0 || c0 >= 1) stop("c must lie in [0,1) for 3PL information.", call. = FALSE)
    }
    if (family == "4pl") {
      c0 <- as.numeric(c0); d0 <- as.numeric(d0)
      if (length(c0) != 1L || length(d0) != 1L || !is.finite(c0) || !is.finite(d0) || c0 < 0 || d0 > 1 || c0 >= d0)
        stop("c and d must satisfy 0 <= c < d <= 1 for 4PL information.", call. = FALSE)
    }
    L <- eyeprocess_irt_2pl_probability(theta, a = a, b = b, D = D)
    p <- switch(family, `2pl` = L, `3pl` = c0 + (1 - c0) * L, `4pl` = c0 + (d0 - c0) * L)
    deriv <- switch(family, `2pl` = D * a * L * (1 - L), `3pl` = (1 - c0) * D * a * L * (1 - L), `4pl` = (d0 - c0) * D * a * L * (1 - L))
    return((deriv^2) / pmax(p * (1 - p), .Machine$double.eps))
  }
  prob_fun <- switch(family,
    grm = function(th) eyeprocess_irt_grm_probability(th, a = .ep09m2_or(dots$a, 1), thresholds = dots$thresholds, D = D),
    gpcm = function(th) eyeprocess_irt_gpcm_probability(th, a = .ep09m2_or(dots$a, 1), steps = dots$steps, D = D),
    nominal = function(th) eyeprocess_irt_nominal_probability(th, slopes = dots$slopes, intercepts = dots$intercepts)
  )
  as.numeric(.ep09m2_numeric_information(prob_fun, theta))
}

.ep09m2_or <- function(x, y) if (is.null(x)) y else x

#' Compute a test information curve from item parameters
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param items Item-parameter data frame or item collection.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_test_information <- function(theta, items, D = 1) {
  theta <- .ep09m2_theta(theta); items <- .ep09m2_item_pars(items)
  mat <- vapply(seq_len(nrow(items)), function(i) {
    eyeprocess_irt_item_information(theta, family = "4pl", a = items$a[i], b = items$b[i], c = items$c[i], d = items$d[i], D = D)
  }, numeric(length(theta)))
  if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1L)
  out <- data.frame(theta = theta, information = rowSums(mat), conditional_sem = 1 / sqrt(pmax(rowSums(mat), .Machine$double.eps)))
  structure(out, class = c("eye_irt_information_profile", "data.frame"))
}

#' Conditional standard error from information
#' @param information Item or test information value or vector.
#' @export
eyeprocess_irt_conditional_sem <- function(information) {
  information <- as.numeric(information)
  if (any(!is.finite(information)) || any(information < 0)) stop("information must be finite and non-negative.", call. = FALSE)
  ifelse(information > 0, 1 / sqrt(information), Inf)
}

#' Expected item score
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param family IRT response family or model family.
#' @param ... Additional arguments passed to the selected method or external engine.
#' @export
eyeprocess_irt_expected_score <- function(theta, family = c("2pl", "3pl", "4pl", "grm", "gpcm", "nominal"), ...) {
  family <- match.arg(family); theta <- .ep09m2_theta(theta); dots <- list(...)
  if (family == "2pl") return(eyeprocess_irt_2pl_probability(theta, a = .ep09m2_or(dots$a, 1), b = .ep09m2_or(dots$b, 0), D = .ep09m2_or(dots$D, 1)))
  if (family == "3pl") return(eyeprocess_irt_3pl_probability(theta, a = .ep09m2_or(dots$a, 1), b = .ep09m2_or(dots$b, 0), c = .ep09m2_or(dots$c, 0.2), D = .ep09m2_or(dots$D, 1)))
  if (family == "4pl") return(eyeprocess_irt_4pl_probability(theta, a = .ep09m2_or(dots$a, 1), b = .ep09m2_or(dots$b, 0), c = .ep09m2_or(dots$c, 0), d = .ep09m2_or(dots$d, 1), D = .ep09m2_or(dots$D, 1)))
  probs <- switch(family,
    grm = eyeprocess_irt_grm_probability(theta, a = .ep09m2_or(dots$a, 1), thresholds = dots$thresholds, D = .ep09m2_or(dots$D, 1)),
    gpcm = eyeprocess_irt_gpcm_probability(theta, a = .ep09m2_or(dots$a, 1), steps = dots$steps, D = .ep09m2_or(dots$D, 1)),
    nominal = eyeprocess_irt_nominal_probability(theta, slopes = dots$slopes, intercepts = dots$intercepts)
  )
  as.numeric(probs %*% (seq_len(ncol(probs)) - 1L))
}

#' Test characteristic curve for dichotomous item parameters
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param items Item-parameter data frame or item collection.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_test_characteristic_curve <- function(theta, items, D = 1) {
  theta <- .ep09m2_theta(theta); items <- .ep09m2_item_pars(items)
  mat <- vapply(seq_len(nrow(items)), function(i) eyeprocess_irt_4pl_probability(theta, items$a[i], items$b[i], items$c[i], items$d[i], D), numeric(length(theta)))
  if (is.null(dim(mat))) mat <- matrix(mat, ncol = 1L)
  structure(data.frame(theta = theta, expected_score = rowSums(mat), max_score = nrow(items)), class = c("eye_irt_test_characteristic_curve", "data.frame"))
}

#' Area under an information curve
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param information Item or test information value or vector.
#' @export
eyeprocess_irt_information_area <- function(theta, information) {
  theta <- as.numeric(theta); information <- as.numeric(information)
  if (length(theta) != length(information) || length(theta) < 2L || any(!is.finite(theta)) || any(!is.finite(information))) stop("theta and information must be finite vectors of equal length >= 2.", call. = FALSE)
  ord <- order(theta); theta <- theta[ord]; information <- information[ord]
  sum(diff(theta) * (head(information, -1L) + tail(information, -1L)) / 2)
}

#' Summarise measurement precision across a theta region
#' @param theta Latent-trait value or vector of latent-trait values.
#' @param items Item-parameter data frame or item collection.
#' @param target Target level, distribution, or criterion.
#' @param D Logistic scaling constant.
#' @export
eyeprocess_irt_measurement_precision_profile <- function(theta, items, target = c(-2, 2), D = 1) {
  target <- as.numeric(target)
  if (length(target) != 2L || any(!is.finite(target)) || target[1] >= target[2]) stop("target must be an increasing finite length-2 vector.", call. = FALSE)
  info <- eyeprocess_irt_test_information(theta, items, D = D)
  keep <- info$theta >= target[1] & info$theta <= target[2]
  if (sum(keep) < 2L) stop("theta must contain at least two finite grid points inside target.", call. = FALSE)
  structure(list(curve = info, target = target, area = eyeprocess_irt_information_area(info$theta[keep], info$information[keep]),
                 min_information = min(info$information[keep]), max_sem = max(info$conditional_sem[keep])), class = "eye_irt_precision_profile")
}

#' @export
print.eyeprocess_irt_model_spec <- function(x, ...) {
  cat("eyeprocess IRT model specification\n")
  cat("  family        :", x$family, "\n")
  cat("  dimensions    :", x$dimensions, "\n")
  cat("  identification:", x$identification, "\n")
  cat("  engine        :", x$engine, "\n")
  cat("  status        :", x$status, "\n")
  invisible(x)
}

#' @export
print.eye_irt_identification_audit <- function(x, ...) {
  cat("eyeprocess IRT identification audit\n")
  cat("  location identified:", x$location_identified, "\n")
  cat("  scale identified   :", x$scale_identified, "\n")
  cat("  valid              :", x$valid, "\n")
  if (length(x$warnings)) cat("  warnings           :", paste(x$warnings, collapse = " | "), "\n")
  invisible(x)
}

#' @export
print.eye_irt_sparse_design_audit <- function(x, ...) {
  cat("eyeprocess sparse IRT design audit\n")
  cat("  persons:", x$n_persons, "| items:", x$n_items, "| observed:", x$n_observed, "\n")
  cat("  density:", if (is.finite(x$density)) sprintf("%.3f", x$density) else "NA", "\n")
  cat("  sparse persons:", length(x$sparse_persons), "| sparse items:", length(x$sparse_items), "\n")
  invisible(x)
}
