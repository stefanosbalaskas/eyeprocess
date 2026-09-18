# Trial-level multilevel mediation preparation ---------------------------------
# Vendor-neutral scientific core. No Bayesian fitting is performed here.

.ep_mediation_stop <- function(...) {
  stop(paste0(...), call. = FALSE)
}

.ep_mediation_assert_data <- function(data) {
  if (!is.data.frame(data)) {
    .ep_mediation_stop("`data` must be a data.frame.")
  }
  data
}

.ep_mediation_assert_columns <- function(data, columns) {
  columns <- columns[!is.na(columns) & nzchar(columns)]
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    .ep_mediation_stop("Missing required columns: ", paste(missing, collapse = ", "), ".")
  }
  invisible(TRUE)
}

.ep_mediation_numeric <- function(data, column) {
  x <- data[[column]]
  if (is.factor(x)) x <- as.character(x)
  out <- suppressWarnings(as.numeric(x))
  invalid <- !is.na(x) & is.na(out)
  if (any(invalid)) {
    values <- utils::head(as.character(x[invalid]), 3L)
    .ep_mediation_stop(
      "`", column, "` must be numeric for mediation preparation; non-numeric examples: ",
      paste(values, collapse = ", "), "."
    )
  }
  out
}

.ep_mediation_indicator <- function(x, name) {
  observed <- x[!is.na(x)]
  if (is.logical(x)) {
    out <- x
    out[is.na(out)] <- FALSE
    return(out)
  }
  if (is.numeric(x) && all(observed %in% c(0, 1))) {
    out <- as.logical(x)
    out[is.na(out)] <- FALSE
    return(out)
  }
  .ep_mediation_stop("`", name, "` must contain only TRUE/FALSE or 0/1 values (plus missing).")
}

.ep_mediation_quality <- function(x, name) {
  original <- x
  if (is.factor(original)) original <- as.character(original)
  out <- suppressWarnings(as.numeric(original))
  invalid <- !is.na(original) & is.na(out)
  if (any(invalid)) {
    examples <- utils::head(as.character(original[invalid]), 3L)
    .ep_mediation_stop(
      "`", name, "` must be numeric when used as a quality variable; non-numeric examples: ",
      paste(examples, collapse = ", "), "."
    )
  }
  out
}

.ep_mediation_check_collisions <- function(data, columns) {
  collisions <- intersect(columns, names(data))
  if (length(collisions)) {
    .ep_mediation_stop(
      "Derived mediation columns already exist: ",
      paste(collisions, collapse = ", "),
      ". Rename or remove them explicitly before preparation."
    )
  }
  invisible(TRUE)
}

.ep_group_mean <- function(values, participant) {
  ave(values, participant, FUN = function(x) {
    if (all(is.na(x))) return(rep(NA_real_, length(x)))
    rep(mean(x, na.rm = TRUE), length(x))
  })
}

#' Center a Trial-Level Variable Within Participant
#'
#' @param data Data frame containing repeated observations.
#' @param value_col Numeric variable to participant-mean center.
#' @param participant_col Participant identifier column.
#' @param output_col Optional output column name.
#' @return A data frame containing the centered variable. Rows and missing
#'   observations are preserved.
#' @export
center_within_participant <- function(
  data,
  value_col,
  participant_col = "participant_id",
  output_col = NULL
) {
  data <- .ep_mediation_assert_data(data)
  .ep_mediation_assert_columns(data, c(participant_col, value_col))
  values <- .ep_mediation_numeric(data, value_col)
  means <- .ep_group_mean(values, data[[participant_col]])
  name <- if (is.null(output_col)) paste0(value_col, "_within") else output_col
  data[[name]] <- values - means
  data
}

#' Decompose Trial-Level Variables Into Within- and Between-Participant Parts
#'
#' @param data Repeated-measures data frame.
#' @param columns One or more numeric column names.
#' @param participant_col Participant identifier column.
#' @param grand_mean_center_between Whether to grand-mean center the participant
#'   mean component.
#' @return Data frame with `<name>_within` and `<name>_between` columns.
#' @export
decompose_within_between <- function(
  data,
  columns,
  participant_col = "participant_id",
  grand_mean_center_between = FALSE
) {
  data <- .ep_mediation_assert_data(data)
  if (!is.character(columns) || !length(columns) || anyNA(columns) || any(!nzchar(columns))) {
    .ep_mediation_stop("`columns` must contain one or more non-empty column names.")
  }
  .ep_mediation_assert_columns(data, c(participant_col, columns))
  if (!is.logical(grand_mean_center_between) || length(grand_mean_center_between) != 1L || is.na(grand_mean_center_between)) {
    .ep_mediation_stop("`grand_mean_center_between` must be TRUE or FALSE.")
  }
  for (column in columns) {
    values <- .ep_mediation_numeric(data, column)
    group_mean <- .ep_group_mean(values, data[[participant_col]])
    between <- group_mean
    if (grand_mean_center_between) between <- between - mean(values, na.rm = TRUE)
    data[[paste0(column, "_within")]] <- values - group_mean
    data[[paste0(column, "_between")]] <- between
  }
  data
}

#' Summarise Within- and Between-Participant Variance
#'
#' @param data Repeated-measures data frame.
#' @param columns Numeric variables to inspect.
#' @param participant_col Participant identifier column.
#' @return A data frame of observed total, within-, and between-participant
#'   variance components.
#' @export
summarise_within_between_variance <- function(
  data,
  columns,
  participant_col = "participant_id"
) {
  data <- .ep_mediation_assert_data(data)
  .ep_mediation_assert_columns(data, c(participant_col, columns))
  rows <- lapply(columns, function(column) {
    values <- .ep_mediation_numeric(data, column)
    group_mean <- .ep_group_mean(values, data[[participant_col]])
    centered <- values - group_mean
    participant_means <- tapply(values, data[[participant_col]], function(x) {
      if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
    })
    n_observed <- sum(!is.na(values))
    total_variance <- if (n_observed > 1L) stats::var(values, na.rm = TRUE) else NA_real_
    within_variance <- if (sum(!is.na(centered)) > 1L) stats::var(centered, na.rm = TRUE) else NA_real_
    between_variance <- if (sum(!is.na(participant_means)) > 1L) stats::var(participant_means, na.rm = TRUE) else NA_real_
    denom <- within_variance + between_variance
    between_share <- if (is.finite(denom) && denom > 0) between_variance / denom else NA_real_
    data.frame(
      variable = column,
      n_observed = n_observed,
      n_missing = sum(is.na(values)),
      n_participants_observed = sum(!is.na(participant_means)),
      total_variance = total_variance,
      within_variance = within_variance,
      between_variance = between_variance,
      between_share = between_share,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Identify Mediation Variable Levels
#'
#' @param data Repeated-measures data frame.
#' @param columns Variables to classify.
#' @param participant_col Participant identifier.
#' @param tolerance Non-negative variance tolerance.
#' @return A data frame classifying variables as within, between, both, or
#'   constant/unidentified.
#' @export
identify_mediation_levels <- function(
  data,
  columns,
  participant_col = "participant_id",
  tolerance = 1e-12
) {
  if (!is.numeric(tolerance) || length(tolerance) != 1L || is.na(tolerance) || !is.finite(tolerance) || tolerance < 0) {
    .ep_mediation_stop("`tolerance` must be one finite non-negative number.")
  }
  summary <- summarise_within_between_variance(data, columns, participant_col)
  has_within <- is.finite(summary$within_variance) & summary$within_variance > tolerance
  has_between <- is.finite(summary$between_variance) & summary$between_variance > tolerance
  level <- ifelse(
    has_within & has_between, "within_and_between",
    ifelse(has_within, "within_only", ifelse(has_between, "between_only", "constant_or_unidentified"))
  )
  data.frame(
    variable = summary$variable,
    has_within_variation = has_within,
    has_between_variation = has_between,
    level = level,
    stringsAsFactors = FALSE
  )
}

#' Audit Mediation Missingness
#'
#' Distinguishes a genuinely observed zero mediator from an unobserved mediator,
#' a poor-quality trial, and a missing behavioural outcome.
#'
#' @export
audit_mediation_missingness <- function(
  data,
  x_col,
  mediator_col,
  outcome_col,
  participant_col = "participant_id",
  trial_col = "trial_id",
  quality_col = NULL,
  minimum_quality = NULL,
  mediator_observed_col = NULL,
  response_observed_col = NULL
) {
  data <- .ep_mediation_assert_data(data)
  .ep_mediation_assert_columns(
    data,
    c(participant_col, trial_col, x_col, mediator_col, outcome_col,
      quality_col, mediator_observed_col, response_observed_col)
  )
  if (xor(is.null(quality_col), is.null(minimum_quality))) {
    .ep_mediation_stop("`quality_col` and `minimum_quality` must be supplied together.")
  }
  if (!is.null(minimum_quality) && (!is.numeric(minimum_quality) || length(minimum_quality) != 1L || is.na(minimum_quality) || !is.finite(minimum_quality))) {
    .ep_mediation_stop("`minimum_quality` must be one finite numeric value.")
  }
  mediator <- .ep_mediation_numeric(data, mediator_col)
  mediator_observed <- !is.na(mediator)
  if (!is.null(mediator_observed_col)) {
    marker <- .ep_mediation_indicator(data[[mediator_observed_col]], mediator_observed_col)
    mediator_observed <- mediator_observed & marker
  }
  response_observed <- !is.na(data[[outcome_col]])
  if (!is.null(response_observed_col)) {
    marker <- .ep_mediation_indicator(data[[response_observed_col]], response_observed_col)
    response_observed <- response_observed & marker
  }
  poor_quality <- rep(FALSE, nrow(data))
  if (!is.null(quality_col)) {
    q <- .ep_mediation_quality(data[[quality_col]], quality_col)
    poor_quality <- is.na(q) | q < minimum_quality
  }
  categories <- list(
    x_missing = is.na(data[[x_col]]),
    mediator_not_observed = !mediator_observed,
    mediator_observed_zero = mediator_observed & mediator == 0,
    mediator_observed_nonzero = mediator_observed & mediator != 0,
    poor_quality_trial = poor_quality,
    response_missing = !response_observed
  )
  data.frame(
    issue = names(categories),
    n = vapply(categories, sum, integer(1)),
    proportion = vapply(categories, function(x) if (length(x)) mean(x) else NA_real_, numeric(1)),
    stringsAsFactors = FALSE
  )
}

#' Check Participant Trial Counts for Mediation
#'
#' @export
check_mediation_trial_counts <- function(
  data,
  participant_col = "participant_id",
  trial_col = "trial_id",
  minimum_trials = 2L
) {
  data <- .ep_mediation_assert_data(data)
  .ep_mediation_assert_columns(data, c(participant_col, trial_col))
  if (!is.numeric(minimum_trials) || length(minimum_trials) != 1L || is.na(minimum_trials) || minimum_trials < 1 || minimum_trials != floor(minimum_trials)) {
    .ep_mediation_stop("`minimum_trials` must be an integer of at least 1.")
  }
  participant <- data[[participant_col]]
  ids <- unique(participant)
  rows <- lapply(ids, function(id) {
    idx <- participant == id
    data.frame(
      participant_value = id,
      n_rows = sum(idx),
      n_trials = length(unique(data[[trial_col]][idx])),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  names(out)[1L] <- participant_col
  out$singleton <- out$n_trials == 1L
  out$below_minimum <- out$n_trials < minimum_trials
  rownames(out) <- NULL
  out
}

#' Validate Trial-Level Multilevel Mediation Data
#'
#' @export
validate_multilevel_mediation_data <- function(
  data,
  x_col,
  mediator_col,
  outcome_col,
  participant_col = "participant_id",
  trial_col = "trial_id",
  require_within_x = TRUE
) {
  data <- .ep_mediation_assert_data(data)
  .ep_mediation_assert_columns(data, c(participant_col, trial_col, x_col, mediator_col, outcome_col))
  issues <- character()
  notes <- character()
  if (!nrow(data)) issues <- c(issues, "data has no rows")
  if (anyNA(data[[participant_col]])) issues <- c(issues, "participant identifiers contain missing values")
  if (anyNA(data[[trial_col]])) issues <- c(issues, "trial identifiers contain missing values")
  key <- paste(data[[participant_col]], data[[trial_col]], sep = "\r")
  if (anyDuplicated(key)) issues <- c(issues, "participant/trial identifiers are not unique")
  invisible(.ep_mediation_numeric(data, mediator_col))
  x_numeric <- suppressWarnings(as.numeric(as.character(data[[x_col]])))
  invalid_x <- !is.na(data[[x_col]]) & is.na(x_numeric)
  if (any(invalid_x)) issues <- c(issues, "X must be numeric or explicitly coded before decomposition")
  levels <- identify_mediation_levels(data, c(x_col, mediator_col), participant_col)
  x_row <- levels[levels$variable == x_col, , drop = FALSE]
  m_row <- levels[levels$variable == mediator_col, , drop = FALSE]
  if (isTRUE(require_within_x) && !isTRUE(x_row$has_within_variation[[1L]])) {
    issues <- c(issues, "X has no within-participant variation")
  }
  if (!isTRUE(m_row$has_within_variation[[1L]])) {
    notes <- c(notes, "mediator has no detectable within-participant variation")
  }
  counts <- check_mediation_trial_counts(data, participant_col, trial_col)
  if (any(counts$singleton)) notes <- c(notes, "one or more participants have only one observed trial")
  if (anyNA(data[[outcome_col]])) notes <- c(notes, "outcome contains missing values; rows are preserved and flagged")
  structure(
    list(
      valid = !length(issues),
      issues = unique(issues),
      warnings = unique(notes),
      levels = levels,
      trial_counts = counts,
      n_rows = nrow(data),
      n_participants = length(unique(data[[participant_col]]))
    ),
    class = "eye_multilevel_mediation_validation"
  )
}

.ep_mediation_fingerprint <- function(data, columns) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(data[columns], path, version = 3)
  unname(tools::md5sum(path))
}

#' Prepare Trial-Level Data for Multilevel Mediation
#'
#' Prepares within- and between-participant components without collapsing trials
#' to participant means. No row is silently dropped and missing gaze is never
#' converted to zero.
#'
#' @param data Trial-level data frame.
#' @param x_col Exposure/manipulation column.
#' @param mediator_col Trial-level mediator, such as AOI dwell.
#' @param outcome_col Trial-level outcome.
#' @param participant_col Participant identifier.
#' @param trial_col Trial identifier.
#' @param quality_col Optional trial-quality variable.
#' @param minimum_quality Required threshold when `quality_col` is supplied.
#' @param mediator_observed_col Optional explicit mediator-observation marker.
#' @param response_observed_col Optional explicit outcome-observation marker.
#' @param quality_action Either `"flag"` or `"mask_mediator"`.
#' @param grand_mean_center_between Whether to center participant means around
#'   the grand mean.
#' @param require_within_x Require within-participant variation in X.
#' @param source_id Optional human-readable source identifier.
#' @param preprocessing_spec Optional preprocessing provenance object.
#' @param event_detector Optional detector provenance object.
#' @param aoi_specification Optional AOI provenance object.
#' @param warn Emit informative warnings captured in the returned object.
#' @return An `eye_multilevel_mediation_data` object containing the full row-level
#'   table, audits, decomposition, and provenance.
#' @export
prepare_multilevel_mediation_data <- function(
  data,
  x_col,
  mediator_col,
  outcome_col,
  participant_col = "participant_id",
  trial_col = "trial_id",
  quality_col = NULL,
  minimum_quality = NULL,
  mediator_observed_col = NULL,
  response_observed_col = NULL,
  quality_action = c("flag", "mask_mediator"),
  grand_mean_center_between = FALSE,
  require_within_x = TRUE,
  source_id = NULL,
  preprocessing_spec = NULL,
  event_detector = NULL,
  aoi_specification = NULL,
  warn = TRUE
) {
  original <- .ep_mediation_assert_data(data)
  data <- original
  quality_action <- match.arg(quality_action)
  .ep_mediation_assert_columns(
    data,
    c(participant_col, trial_col, x_col, mediator_col, outcome_col,
      quality_col, mediator_observed_col, response_observed_col)
  )
  derived <- c(
    paste0(x_col, "_within"), paste0(x_col, "_between"),
    paste0(mediator_col, "_within"), paste0(mediator_col, "_between"),
    "X_within", "X_between", "M_within", "M_between",
    "mediation_mediator_observed", "mediation_mediator_true_zero",
    "mediation_poor_quality", "mediation_response_observed",
    "mediation_mediator_state", "mediation_analysis_eligible"
  )
  .ep_mediation_check_collisions(data, derived)
  if (xor(is.null(quality_col), is.null(minimum_quality))) {
    .ep_mediation_stop("`quality_col` and `minimum_quality` must be supplied together.")
  }
  if (!is.null(minimum_quality) && (!is.numeric(minimum_quality) || length(minimum_quality) != 1L || is.na(minimum_quality) || !is.finite(minimum_quality))) {
    .ep_mediation_stop("`minimum_quality` must be one finite numeric value.")
  }
  validation <- validate_multilevel_mediation_data(
    data,
    x_col = x_col,
    mediator_col = mediator_col,
    outcome_col = outcome_col,
    participant_col = participant_col,
    trial_col = trial_col,
    require_within_x = require_within_x
  )
  if (!validation$valid) {
    .ep_mediation_stop("Invalid multilevel mediation data: ", paste(validation$issues, collapse = "; "))
  }

  mediator_original <- .ep_mediation_numeric(data, mediator_col)
  mediator_observed <- !is.na(mediator_original)
  if (!is.null(mediator_observed_col)) {
    marker <- .ep_mediation_indicator(data[[mediator_observed_col]], mediator_observed_col)
    mediator_observed <- mediator_observed & marker
  }
  poor_quality <- rep(FALSE, nrow(data))
  if (!is.null(quality_col)) {
    q <- .ep_mediation_quality(data[[quality_col]], quality_col)
    poor_quality <- is.na(q) | q < minimum_quality
  }
  if (quality_action == "mask_mediator") {
    data[[mediator_col]][poor_quality] <- NA
    mediator_observed <- mediator_observed & !poor_quality
  }
  mediator <- .ep_mediation_numeric(data, mediator_col)
  response_observed <- !is.na(data[[outcome_col]])
  if (!is.null(response_observed_col)) {
    marker <- .ep_mediation_indicator(data[[response_observed_col]], response_observed_col)
    response_observed <- response_observed & marker
  }

  data <- decompose_within_between(
    data,
    c(x_col, mediator_col),
    participant_col = participant_col,
    grand_mean_center_between = grand_mean_center_between
  )
  data$X_within <- data[[paste0(x_col, "_within")]]
  data$X_between <- data[[paste0(x_col, "_between")]]
  data$M_within <- data[[paste0(mediator_col, "_within")]]
  data$M_between <- data[[paste0(mediator_col, "_between")]]
  data$mediation_mediator_observed <- mediator_observed
  data$mediation_mediator_true_zero <- mediator_observed & mediator == 0
  data$mediation_poor_quality <- poor_quality
  data$mediation_response_observed <- response_observed
  data$mediation_mediator_state <- ifelse(
    poor_quality, "poor_quality",
    ifelse(!mediator_observed, "not_observed", ifelse(mediator == 0, "observed_zero", "observed_nonzero"))
  )
  data$mediation_analysis_eligible <- !is.na(data[[x_col]]) & mediator_observed & response_observed & !poor_quality

  variance <- summarise_within_between_variance(data, c(x_col, mediator_col), participant_col)
  levels <- identify_mediation_levels(data, c(x_col, mediator_col), participant_col)
  missingness <- audit_mediation_missingness(
    data,
    x_col = x_col,
    mediator_col = mediator_col,
    outcome_col = outcome_col,
    participant_col = participant_col,
    trial_col = trial_col,
    quality_col = quality_col,
    minimum_quality = minimum_quality,
    response_observed_col = response_observed_col
  )
  trial_counts <- check_mediation_trial_counts(data, participant_col, trial_col)

  notes <- validation$warnings
  if (any(poor_quality)) {
    notes <- c(notes, paste0(
      "poor-quality trials are flagged; mediator values were ",
      if (quality_action == "mask_mediator") "explicitly masked before decomposition" else "retained"
    ))
  }
  if (any(!mediator_observed)) {
    notes <- c(notes, "missing mediator observations remain missing and were not converted to zero")
  }
  if (any(mediator_observed & mediator == 0, na.rm = TRUE)) {
    notes <- c(notes, "observed zero mediator values are retained and separately identified")
  }
  notes <- unique(notes)
  if (isTRUE(warn)) for (note in notes) warning(note, call. = FALSE)

  source_columns <- unique(c(
    participant_col, trial_col, x_col, mediator_col, outcome_col,
    quality_col, mediator_observed_col, response_observed_col
  ))
  source_columns <- source_columns[!is.na(source_columns) & nzchar(source_columns)]
  provenance <- list(
    source_id = source_id,
    source_fingerprint = .ep_mediation_fingerprint(original, source_columns),
    fingerprint_algorithm = "md5-rds-v3",
    source_columns = source_columns,
    preprocessing_specification = preprocessing_spec,
    event_detector = event_detector,
    aoi_specification = aoi_specification,
    quality_rules = list(
      quality_col = quality_col,
      minimum_quality = minimum_quality,
      quality_action = quality_action,
      mediator_observed_col = mediator_observed_col,
      response_observed_col = response_observed_col
    ),
    preparation_specification = list(
      x_col = x_col,
      mediator_col = mediator_col,
      outcome_col = outcome_col,
      participant_col = participant_col,
      trial_col = trial_col,
      grand_mean_center_between = grand_mean_center_between,
      require_within_x = require_within_x,
      missingness_policy = "preserve_and_flag"
    ),
    row_position_convention = list(base = 1L, meaning = "one-based input row position"),
    software = list(package = "eyeprocess", version = tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) "development"))
  )

  structure(
    list(
      data = data,
      columns = list(
        participant = participant_col,
        trial = trial_col,
        x = x_col,
        mediator = mediator_col,
        outcome = outcome_col,
        quality = quality_col
      ),
      levels = levels,
      variance = variance,
      missingness = missingness,
      trial_counts = trial_counts,
      validation = validation,
      provenance = provenance,
      warnings = notes,
      preparation_version = "0.1",
      fit_performed = FALSE
    ),
    class = "eye_multilevel_mediation_data"
  )
}

#' @export
print.eye_multilevel_mediation_data <- function(x, ...) {
  cat("<eye_multilevel_mediation_data>\n")
  cat("  Rows:", nrow(x$data), "\n")
  cat("  Participants:", length(unique(x$data[[x$columns$participant]])), "\n")
  cat("  Analysis-eligible rows:", sum(x$data$mediation_analysis_eligible), "\n")
  cat("  Missingness policy: preserve_and_flag\n")
  cat("  Fit performed: FALSE\n")
  invisible(x)
}

#' Add an Additional Within/Between Mediation Component
#'
#' Adds a serial mediator or moderator decomposition to an existing prepared
#' mediation object. This keeps scientific decomposition in eyeprocess rather
#' than in the downstream Bayesian modelling package.
#'
#' @param prepared An `eye_multilevel_mediation_data` object.
#' @param value_col Numeric source variable.
#' @param semantic Semantic key such as `"mediator2"` or `"moderator"`.
#' @param within_col Output within-person component name.
#' @param between_col Output between-person component name.
#' @param grand_mean_center_between Optional override of the original centering
#'   convention.
#' @return Updated `eye_multilevel_mediation_data` object.
#' @export
add_multilevel_mediation_component <- function(
  prepared,
  value_col,
  semantic,
  within_col,
  between_col,
  grand_mean_center_between = NULL
) {
  if (!inherits(prepared, "eye_multilevel_mediation_data")) {
    .ep_mediation_stop("`prepared` must inherit from `eye_multilevel_mediation_data`.")
  }
  values <- c(value_col, semantic, within_col, between_col)
  if (!is.character(values) || anyNA(values) || any(!nzchar(values))) {
    .ep_mediation_stop("Component names must be non-empty character scalars.")
  }
  if (identical(within_col, between_col)) {
    .ep_mediation_stop("`within_col` and `between_col` must be different.")
  }
  data <- prepared$data
  .ep_mediation_assert_columns(data, value_col)
  generated <- unique(c(paste0(value_col, "_within"), paste0(value_col, "_between"), within_col, between_col))
  .ep_mediation_check_collisions(data, generated)
  if (is.null(grand_mean_center_between)) {
    grand_mean_center_between <- isTRUE(
      prepared$provenance$preparation_specification$grand_mean_center_between
    )
  }
  data <- decompose_within_between(
    data,
    value_col,
    participant_col = prepared$columns$participant,
    grand_mean_center_between = grand_mean_center_between
  )
  data[[within_col]] <- data[[paste0(value_col, "_within")]]
  data[[between_col]] <- data[[paste0(value_col, "_between")]]
  prepared$data <- data
  prepared$columns[[semantic]] <- value_col
  prepared$columns[[paste0(semantic, "_within")]] <- within_col
  prepared$columns[[paste0(semantic, "_between")]] <- between_col
  additions <- prepared$provenance$additional_mediation_components
  if (is.null(additions)) additions <- list()
  additions[[length(additions) + 1L]] <- list(
    semantic = semantic,
    source_column = value_col,
    within_column = within_col,
    between_column = between_col,
    grand_mean_center_between = grand_mean_center_between
  )
  prepared$provenance$additional_mediation_components <- additions
  prepared
}
