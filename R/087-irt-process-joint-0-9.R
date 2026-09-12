# eyeprocess 0.9 Milestone #2: process-aware IRT data contracts and descriptive joint summaries

#' Declare a response/process joint IRT specification
#' @param response_family Response-model family.
#' @param time_model Response-time model specification.
#' @param process_channels Declared process-measure channels.
#' @param person_covariates Person-level covariates.
#' @param item_covariates Item-level covariates.
#' @param missingness Missing-data handling or missingness specification.
#' @param status Evidence, model, or governance status.
#' @export
eyeprocess_joint_process_irt_spec <- function(
    response_family = c("2pl", "rasch", "grm", "gpcm"),
    time_model = c("none", "lognormal", "custom"),
    process_channels = c("dwell", "pupil", "transitions"),
    person_covariates = character(),
    item_covariates = character(),
    missingness = c("ignorable", "modeled", "gated"),
    status = c("experimental", "reference", "gated")) {
  response_family <- match.arg(response_family); time_model <- match.arg(time_model); missingness <- match.arg(missingness); status <- match.arg(status)
  process_channels <- unique(as.character(process_channels)); person_covariates <- unique(as.character(person_covariates)); item_covariates <- unique(as.character(item_covariates))
  if (anyNA(c(process_channels, person_covariates, item_covariates))) stop("channel/covariate names cannot be missing.", call. = FALSE)
  structure(list(response_family = response_family, time_model = time_model, process_channels = process_channels,
                 person_covariates = person_covariates, item_covariates = item_covariates, missingness = missingness, status = status),
            class = "eye_joint_process_irt_spec")
}

#' Validate a joint process IRT specification
#' @param x Object to validate, summarize, verify, or otherwise process.
#' @export
validate_eyeprocess_joint_process_irt_spec <- function(x) {
  if (!inherits(x, "eye_joint_process_irt_spec")) stop("x must inherit from eye_joint_process_irt_spec.", call. = FALSE)
  invisible(TRUE)
}

#' Prepare a sparse response/process bundle for external joint engines
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param person Name of the person identifier column.
#' @param item Name of the item identifier column.
#' @param response Observed item response or response variable.
#' @param response_time Response-time variable or values.
#' @param process Process-measure columns or process object.
#' @param covariates Optional covariate columns or covariate data.
#' @export
eyeprocess_process_irt_data_bundle <- function(data, person, item, response, response_time = NULL, process = character(), covariates = character()) {
  data <- .ep09m2_as_df(data, "data")
  cols <- unique(c(person, item, response, response_time, process, covariates)); cols <- cols[!is.na(cols) & nzchar(cols)]
  .ep09m2_req_cols(data, cols, "data")
  if (any(!is.na(data[[response]]) & !data[[response]] %in% c(0, 1))) stop("response must be binary/NA for this bundle.", call. = FALSE)
  if (!is.null(response_time)) {
    rt <- as.numeric(data[[response_time]])
    if (any(is.finite(rt) & rt <= 0)) stop("response_time must be positive where observed.", call. = FALSE)
  }
  out <- data[, cols, drop = FALSE]
  structure(list(data = out, person = person, item = item, response = response, response_time = response_time,
                 process = process, covariates = covariates, n_persons = length(unique(data[[person]])), n_items = length(unique(data[[item]]))),
            class = "eye_process_irt_data_bundle")
}

#' Summarise response-time structure for joint IRT work
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param person Name of the person identifier column.
#' @param item Name of the item identifier column.
#' @param response_time Response-time variable or values.
#' @export
eyeprocess_response_time_profile <- function(data, person, item, response_time) {
  data <- .ep09m2_as_df(data, "data"); .ep09m2_req_cols(data, c(person, item, response_time), "data")
  rt <- as.numeric(data[[response_time]]); good <- is.finite(rt) & rt > 0
  if (!any(good)) stop("No positive finite response times.", call. = FALSE)
  z <- data[good, c(person, item), drop = FALSE]; z$rt <- rt[good]; z$log_rt <- log(z$rt)
  item_rows <- split(seq_len(nrow(z)), z[[item]])
  item_tab <- do.call(rbind, lapply(item_rows, function(ii) data.frame(item_id = as.character(z[[item]][ii[1L]]), n = length(ii), mean_log_rt = mean(z$log_rt[ii]), sd_log_rt = if (length(ii) > 1L) stats::sd(z$log_rt[ii]) else NA_real_, median_rt = stats::median(z$rt[ii]), stringsAsFactors = FALSE)))
  person_rows <- split(seq_len(nrow(z)), z[[person]])
  person_tab <- do.call(rbind, lapply(person_rows, function(ii) data.frame(person_id = as.character(z[[person]][ii[1L]]), n = length(ii), mean_log_rt = mean(z$log_rt[ii]), median_rt = stats::median(z$rt[ii]), stringsAsFactors = FALSE)))
  structure(list(item = item_tab, person = person_tab, n = nrow(z)), class = "eye_response_time_profile")
}

#' Describe speed-accuracy association without causal interpretation
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param person Name of the person identifier column.
#' @param response Observed item response or response variable.
#' @param response_time Response-time variable or values.
#' @export
eyeprocess_speed_accuracy_profile <- function(data, person, response, response_time) {
  data <- .ep09m2_as_df(data, "data"); .ep09m2_req_cols(data, c(person, response, response_time), "data")
  y <- as.numeric(data[[response]]); rt <- as.numeric(data[[response_time]])
  good <- !is.na(y) & y %in% c(0, 1) & is.finite(rt) & rt > 0
  d <- data[good, c(person), drop = FALSE]; d$y <- y[good]; d$log_rt <- log(rt[good])
  rows <- split(seq_len(nrow(d)), d[[person]])
  tab <- do.call(rbind, lapply(rows, function(ii) {
    z <- d[ii, , drop = FALSE]; corv <- if (length(ii) >= 4L && stats::sd(z$y) > 0 && stats::sd(z$log_rt) > 0) stats::cor(z$y, z$log_rt) else NA_real_
    data.frame(person_id = as.character(z[[person]][1]), n = length(ii), accuracy = mean(z$y), mean_log_rt = mean(z$log_rt), within_person_correlation = corv, stringsAsFactors = FALSE)
  }))
  structure(list(person = tab, pooled_correlation = if (sum(good) >= 4L && stats::sd(y[good]) > 0) stats::cor(y[good], log(rt[good])) else NA_real_,
                 guardrail = "Speed-accuracy associations are descriptive and may differ across levels of aggregation."), class = "eye_speed_accuracy_profile")
}

#' Aggregate process channels by item
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param item Name of the item identifier column.
#' @param channels Names or definitions of measurement channels.
#' @export
eyeprocess_process_item_profile <- function(data, item, channels) {
  data <- .ep09m2_as_df(data, "data"); channels <- as.character(channels); .ep09m2_req_cols(data, c(item, channels), "data")
  rows <- split(seq_len(nrow(data)), data[[item]])
  do.call(rbind, lapply(rows, function(ii) {
    vals <- lapply(channels, function(ch) .ep09m2_finite_mean(data[[ch]][ii])); names(vals) <- paste0("mean_", channels)
    data.frame(item_id = as.character(data[[item]][ii[1L]]), n = length(ii), as.data.frame(vals, check.names = FALSE), stringsAsFactors = FALSE)
  }))
}

#' Aggregate process channels by person
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param person Name of the person identifier column.
#' @param channels Names or definitions of measurement channels.
#' @export
eyeprocess_process_person_profile <- function(data, person, channels) {
  data <- .ep09m2_as_df(data, "data"); channels <- as.character(channels); .ep09m2_req_cols(data, c(person, channels), "data")
  rows <- split(seq_len(nrow(data)), data[[person]])
  do.call(rbind, lapply(rows, function(ii) {
    vals <- lapply(channels, function(ch) .ep09m2_finite_mean(data[[ch]][ii])); names(vals) <- paste0("mean_", channels)
    data.frame(person_id = as.character(data[[person]][ii[1L]]), n = length(ii), as.data.frame(vals, check.names = FALSE), stringsAsFactors = FALSE)
  }))
}

#' Align item parameters with process-channel summaries
#' @param item_parameters Item-parameter data frame.
#' @param process_profile Process-measure profile or summary.
#' @param process_columns Names of process-measure columns to use.
#' @export
eyeprocess_irt_process_alignment <- function(item_parameters, process_profile, process_columns = NULL) {
  item_parameters <- .ep09m2_item_pars(item_parameters); process_profile <- .ep09m2_as_df(process_profile, "process_profile")
  .ep09m2_req_cols(process_profile, "item_id", "process_profile")
  z <- merge(item_parameters, process_profile, by = "item_id", all.x = TRUE)
  if (is.null(process_columns)) process_columns <- setdiff(names(process_profile), c("item_id", "n"))
  cor_tab <- do.call(rbind, lapply(process_columns, function(ch) {
    if (!ch %in% names(z)) return(NULL)
    v <- as.numeric(z[[ch]]); keep_a <- is.finite(v) & is.finite(z$a); keep_b <- is.finite(v) & is.finite(z$b)
    data.frame(channel = ch,
      correlation_discrimination = if (sum(keep_a) >= 3L && stats::sd(v[keep_a]) > 0 && stats::sd(z$a[keep_a]) > 0) stats::cor(v[keep_a], z$a[keep_a]) else NA_real_,
      correlation_difficulty = if (sum(keep_b) >= 3L && stats::sd(v[keep_b]) > 0 && stats::sd(z$b[keep_b]) > 0) stats::cor(v[keep_b], z$b[keep_b]) else NA_real_, stringsAsFactors = FALSE)
  }))
  structure(list(table = z, correlations = cor_tab,
                 guardrail = "Alignment is descriptive response-process evidence and does not identify cognitive mechanisms."), class = "eye_irt_process_alignment")
}

#' Summarise missingness patterns across response and process channels
#' @param data Input data frame, matrix, or compatible analysis object.
#' @param response Observed item response or response variable.
#' @param channels Names or definitions of measurement channels.
#' @export
eyeprocess_process_missingness_pattern <- function(data, response, channels) {
  data <- .ep09m2_as_df(data, "data"); channels <- as.character(channels); .ep09m2_req_cols(data, c(response, channels), "data")
  miss <- data.frame(response_missing = is.na(data[[response]]), stringsAsFactors = FALSE)
  for (ch in channels) miss[[paste0(ch, "_missing")]] <- is.na(data[[ch]])
  pattern <- apply(miss, 1L, function(z) paste(as.integer(z), collapse = "")); tab <- sort(table(pattern), decreasing = TRUE)
  data.frame(pattern = names(tab), n = as.integer(tab), fraction = as.integer(tab) / nrow(data), stringsAsFactors = FALSE)
}

#' Construct a multichannel measurement map
#' @param response Observed item response or response variable.
#' @param channels Names or definitions of measurement channels.
#' @param role Declared role of each measurement channel.
#' @export
eyeprocess_multichannel_measurement_map <- function(response = "accuracy", channels = c("response_time", "dwell", "pupil", "transitions"), role = NULL) {
  channels <- unique(as.character(channels)); if (is.null(role)) role <- rep("response_process_measurement", length(channels))
  if (length(role) != length(channels)) stop("role must match channels.", call. = FALSE)
  data.frame(channel = c(response, channels), role = c("item_response", as.character(role)),
             inference_boundary = c("psychometric response", rep("measurement channel; not a direct mental-state label", length(channels))), stringsAsFactors = FALSE)
}

#' @export
print.eye_joint_process_irt_spec <- function(x, ...) {
  cat("eyeprocess joint process-IRT specification\n")
  cat("  response family :", x$response_family, "\n")
  cat("  response time   :", x$time_model, "\n")
  cat("  process channels:", paste(x$process_channels, collapse = ", "), "\n")
  cat("  status          :", x$status, "\n")
  invisible(x)
}
