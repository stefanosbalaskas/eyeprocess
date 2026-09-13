# eyeprocess 0.7.0.9000 -------------------------------------------------------
# Semantic-fidelity and independent validation evidence extensions.
# These functions intentionally extend (rather than replace) the existing
# validation corpus, compatibility matrix, and BIDS interoperability APIs.

.ep07_as_data_frame <- function(x, arg = deparse(substitute(x))) {
  if (is.data.frame(x)) return(x)
  if (inherits(x, "eye_dataset")) {
    candidates <- c("samples", "data", "gaze")
    for (nm in candidates) {
      z <- x[[nm]]
      if (is.data.frame(z)) return(z)
    }
  }
  stop(sprintf("`%s` must be a data.frame or an eye_dataset with a sample table.", arg), call. = FALSE)
}

.ep07_req_cols <- function(x, cols, arg = deparse(substitute(x))) {
  missing <- setdiff(cols, names(x))
  if (length(missing)) {
    stop(sprintf("`%s` is missing required column(s): %s.", arg, paste(missing, collapse = ", ")), call. = FALSE)
  }
  invisible(TRUE)
}

.ep07_scalar_chr <- function(x, arg) {
  if (length(x) != 1L || is.na(x) || !nzchar(as.character(x))) {
    stop(sprintf("`%s` must be one non-missing character value.", arg), call. = FALSE)
  }
  as.character(x)
}

.ep07_scale_equal <- function(a, b, tol) {
  ok <- is.finite(a) & is.finite(b)
  if (!any(ok)) return(NA)
  max(abs(a[ok] - b[ok])) <= tol
}

.ep07_safe_cor <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L || stats::sd(x[ok]) == 0 || stats::sd(y[ok]) == 0) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok]))
}

.ep07_fidelity_rank <- function(status) {
  levels <- c(
    "LOSSLESS", "SEMANTICALLY_EQUIVALENT", "UNIT_TRANSFORMED",
    "COORDINATE_TRANSFORMED", "DERIVED", "INTENTIONALLY_DROPPED",
    "UNSUPPORTED", "AMBIGUOUS", "MISSING"
  )
  match(status, levels)
}

#' Detailed validation evidence levels
#'
#' Returns the fine-grained evidence ladder used by the 0.7 validation
#' programme. This is deliberately orthogonal to the existing public support
#' levels (declared / fixture-tested / empirically-validated), so existing
#' compatibility claims remain stable.
#'
#' @return A data frame ordered from weakest to strongest evidence.
#' @export
validation_evidence_levels <- function() {
  data.frame(
    rank = 1:6,
    level = c(
      "declared",
      "synthetic-fixture",
      "vendor-example",
      "independent-public-real",
      "multisession-multidevice-real",
      "semantic-roundtrip-validated"
    ),
    requirement = c(
      "Adapter or schema support is declared.",
      "Deterministic synthetic or package fixture passes the declared contract.",
      "A vendor-provided example export passes import and semantic checks.",
      "An independently produced public real recording passes the declared checks.",
      "Evidence spans repeated sessions and/or more than one device/model context.",
      "Native-to-canonical-to-interchange-to-canonical round trip has field-level semantic-loss evidence."
    ),
    stringsAsFactors = FALSE
  )
}

#' Semantic fidelity specification
#'
#' @param timestamp_tolerance Absolute tolerance after time-unit normalization.
#' @param coordinate_tolerance Absolute tolerance after coordinate normalization.
#' @param pupil_tolerance Absolute tolerance after pupil-unit normalization.
#' @param missingness_tolerance Maximum tolerated absolute change in missingness.
#' @param correlation_floor Correlation floor used when deciding whether a
#'   numeric transformation remains semantically equivalent.
#' @param allow_row_reorder Whether row reordering is permitted when a key is
#'   supplied.
#' @return An object of class `eye_semantic_fidelity_spec`.
#' @export
semantic_fidelity_spec <- function(timestamp_tolerance = 1e-6,
                                   coordinate_tolerance = 1e-6,
                                   pupil_tolerance = 1e-6,
                                   missingness_tolerance = 1e-6,
                                   correlation_floor = 0.999,
                                   allow_row_reorder = TRUE) {
  vals <- c(timestamp_tolerance, coordinate_tolerance, pupil_tolerance, missingness_tolerance)
  if (any(!is.finite(vals) | vals < 0)) stop("Tolerances must be finite and non-negative.", call. = FALSE)
  if (!is.finite(correlation_floor) || correlation_floor < -1 || correlation_floor > 1) {
    stop("`correlation_floor` must lie in [-1, 1].", call. = FALSE)
  }
  structure(
    list(
      timestamp_tolerance = timestamp_tolerance,
      coordinate_tolerance = coordinate_tolerance,
      pupil_tolerance = pupil_tolerance,
      missingness_tolerance = missingness_tolerance,
      correlation_floor = correlation_floor,
      allow_row_reorder = isTRUE(allow_row_reorder)
    ),
    class = "eye_semantic_fidelity_spec"
  )
}

.ep07_align_frames <- function(source, roundtrip, key = NULL, allow_row_reorder = TRUE) {
  if (is.null(key)) {
    n <- min(nrow(source), nrow(roundtrip))
    return(list(
      source = source[seq_len(n), , drop = FALSE],
      roundtrip = roundtrip[seq_len(n), , drop = FALSE],
      source_n = nrow(source), roundtrip_n = nrow(roundtrip), matched_n = n,
      alignment = "row-order"
    ))
  }
  key <- as.character(key)
  .ep07_req_cols(source, key, "source")
  .ep07_req_cols(roundtrip, key, "roundtrip")
  if (!allow_row_reorder && !identical(source[key], roundtrip[key])) {
    stop("Key columns differ and `allow_row_reorder = FALSE`.", call. = FALSE)
  }
  s_key <- do.call(paste, c(source[key], sep = "\r"))
  r_key <- do.call(paste, c(roundtrip[key], sep = "\r"))
  if (anyDuplicated(s_key) || anyDuplicated(r_key)) {
    stop("Round-trip keys must uniquely identify rows.", call. = FALSE)
  }
  idx <- match(s_key, r_key)
  keep <- !is.na(idx)
  list(
    source = source[keep, , drop = FALSE],
    roundtrip = roundtrip[idx[keep], , drop = FALSE],
    source_n = nrow(source), roundtrip_n = nrow(roundtrip), matched_n = sum(keep),
    alignment = paste(key, collapse = "+")
  )
}

#' Field-level semantic fidelity report
#'
#' Classifies canonical fields after a semantic round trip. Character fields
#' are compared exactly after NA preservation; numeric fields are compared
#' directly and also tested for a stable affine transform.
#'
#' @param source Original canonical data.
#' @param roundtrip Canonical data reconstructed after an interchange round trip.
#' @param fields Fields to compare. Defaults to common fields.
#' @param mapping Optional named character vector mapping source field names to
#'   round-trip field names.
#' @param key Optional unique row-alignment fields.
#' @param tolerance Numeric equality tolerance.
#' @param spec A `semantic_fidelity_spec()` object.
#' @return An object of class `eye_field_fidelity_report`.
#' @export
field_fidelity_report <- function(source, roundtrip,
                                  fields = NULL,
                                  mapping = NULL,
                                  key = NULL,
                                  tolerance = 1e-8,
                                  spec = semantic_fidelity_spec()) {
  source <- .ep07_as_data_frame(source, "source")
  roundtrip <- .ep07_as_data_frame(roundtrip, "roundtrip")
  al <- .ep07_align_frames(source, roundtrip, key, spec$allow_row_reorder)
  source <- al$source
  roundtrip <- al$roundtrip

  if (is.null(fields)) {
    common <- intersect(names(source), names(roundtrip))
    preferred <- c(
      "participant_id", "recording_id", "trial_id", "stimulus_id",
      "timestamp", "timestamp_native", "x", "y", "pupil_left",
      "pupil_right", "eye", "event", "event_type", "fixation_id"
    )
    fields <- unique(c(intersect(preferred, common), common))
  }
  fields <- as.character(fields)
  if (is.null(mapping)) mapping <- stats::setNames(fields, fields)
  if (is.null(names(mapping)) || any(!nzchar(names(mapping)))) {
    stop("`mapping` must be a named character vector: source_field = roundtrip_field.", call. = FALSE)
  }

  out <- lapply(fields, function(src) {
    dst <- if (src %in% names(mapping)) unname(mapping[[src]]) else src
    if (!src %in% names(source)) {
      return(data.frame(field = src, roundtrip_field = dst, status = "MISSING",
                        source_present = FALSE, roundtrip_present = dst %in% names(roundtrip),
                        n = 0L, missingness_delta = NA_real_, correlation = NA_real_,
                        max_abs_error = NA_real_, transform_intercept = NA_real_,
                        transform_slope = NA_real_, stringsAsFactors = FALSE))
    }
    if (!dst %in% names(roundtrip)) {
      return(data.frame(field = src, roundtrip_field = dst, status = "UNSUPPORTED",
                        source_present = TRUE, roundtrip_present = FALSE,
                        n = nrow(source), missingness_delta = NA_real_, correlation = NA_real_,
                        max_abs_error = NA_real_, transform_intercept = NA_real_,
                        transform_slope = NA_real_, stringsAsFactors = FALSE))
    }
    a <- source[[src]]
    b <- roundtrip[[dst]]
    n <- min(length(a), length(b))
    a <- a[seq_len(n)]; b <- b[seq_len(n)]
    md <- abs(mean(is.na(a)) - mean(is.na(b)))
    status <- "AMBIGUOUS"
    corv <- err <- int <- slope <- NA_real_

    if (is.numeric(a) && is.numeric(b)) {
      ok <- is.finite(a) & is.finite(b)
      if (any(ok)) {
        err <- max(abs(a[ok] - b[ok]))
        corv <- .ep07_safe_cor(a, b)
      }
      same_na <- identical(is.na(a), is.na(b))
      if (same_na && isTRUE(.ep07_scale_equal(a, b, tolerance))) {
        status <- "LOSSLESS"
      } else if (sum(ok) >= 3L && isTRUE(corv >= spec$correlation_floor)) {
        lmfit <- try(stats::lm(b[ok] ~ a[ok]), silent = TRUE)
        if (!inherits(lmfit, "try-error")) {
          cf <- stats::coef(lmfit)
          int <- unname(cf[[1L]]); slope <- unname(cf[[2L]])
          pred <- int + slope * a[ok]
          residual_error <- max(abs(pred - b[ok]))
          if (is.finite(residual_error) && residual_error <= max(tolerance, spec$coordinate_tolerance)) {
            status <- if (abs(int) <= tolerance && abs(slope - 1) <= tolerance) {
              "SEMANTICALLY_EQUIVALENT"
            } else {
              "UNIT_TRANSFORMED"
            }
          } else {
            status <- "SEMANTICALLY_EQUIVALENT"
          }
        }
      }
    } else {
      aa <- as.character(a); bb <- as.character(b)
      same <- (aa == bb) | (is.na(a) & is.na(b))
      status <- if (all(same, na.rm = TRUE) && identical(is.na(a), is.na(b))) "LOSSLESS" else "AMBIGUOUS"
    }
    if (is.finite(md) && md > spec$missingness_tolerance && status == "LOSSLESS") {
      status <- "SEMANTICALLY_EQUIVALENT"
    }
    data.frame(
      field = src, roundtrip_field = dst, status = status,
      source_present = TRUE, roundtrip_present = TRUE, n = n,
      missingness_delta = md, correlation = corv, max_abs_error = err,
      transform_intercept = int, transform_slope = slope,
      stringsAsFactors = FALSE
    )
  })
  ans <- do.call(rbind, out)
  ans$severity_rank <- .ep07_fidelity_rank(ans$status)
  structure(
    list(
      fields = ans,
      alignment = al$alignment,
      source_n = al$source_n,
      roundtrip_n = al$roundtrip_n,
      matched_n = al$matched_n,
      spec = spec
    ),
    class = "eye_field_fidelity_report"
  )
}

#' Timestamp semantic-fidelity audit
#'
#' @param source Original data.
#' @param roundtrip Round-tripped data.
#' @param source_time Source timestamp column.
#' @param roundtrip_time Round-trip timestamp column.
#' @param source_unit,roundtrip_unit One of seconds, milliseconds,
#'   microseconds, or nanoseconds.
#' @param key Optional alignment key.
#' @param tolerance Seconds-scale tolerance after normalization.
#' @return An `eye_timestamp_fidelity` object.
#' @export
timestamp_fidelity_audit <- function(source, roundtrip,
                                     source_time = "timestamp",
                                     roundtrip_time = source_time,
                                     source_unit = "seconds",
                                     roundtrip_unit = source_unit,
                                     key = NULL,
                                     tolerance = 1e-6) {
  source <- .ep07_as_data_frame(source, "source")
  roundtrip <- .ep07_as_data_frame(roundtrip, "roundtrip")
  .ep07_req_cols(source, source_time, "source")
  .ep07_req_cols(roundtrip, roundtrip_time, "roundtrip")
  al <- .ep07_align_frames(source, roundtrip, key, TRUE)
  mult <- c(seconds = 1, milliseconds = 1e-3, microseconds = 1e-6, nanoseconds = 1e-9)
  if (!source_unit %in% names(mult) || !roundtrip_unit %in% names(mult)) {
    stop("Timestamp units must be seconds, milliseconds, microseconds, or nanoseconds.", call. = FALSE)
  }
  a <- as.numeric(al$source[[source_time]]) * mult[[source_unit]]
  b <- as.numeric(al$roundtrip[[roundtrip_time]]) * mult[[roundtrip_unit]]
  ok <- is.finite(a) & is.finite(b)
  delta <- b[ok] - a[ok]
  offset <- if (length(delta)) stats::median(delta) else NA_real_
  centered_error <- if (length(delta)) max(abs(delta - offset)) else NA_real_
  slope <- intercept <- NA_real_
  if (sum(ok) >= 3L && stats::sd(a[ok]) > 0) {
    fit <- stats::lm(b[ok] ~ a[ok])
    cf <- stats::coef(fit)
    intercept <- unname(cf[[1L]]); slope <- unname(cf[[2L]])
  }
  monotonic_a <- all(diff(a[is.finite(a)]) >= 0)
  monotonic_b <- all(diff(b[is.finite(b)]) >= 0)
  interval_cor <- if (sum(ok) >= 4L) .ep07_safe_cor(diff(a[ok]), diff(b[ok])) else NA_real_
  status <- if (length(delta) && max(abs(delta)) <= tolerance) {
    "LOSSLESS"
  } else if (is.finite(centered_error) && centered_error <= tolerance && monotonic_b) {
    "SEMANTICALLY_EQUIVALENT"
  } else if (is.finite(slope) && abs(slope - 1) <= 1e-6 && monotonic_b) {
    "SEMANTICALLY_EQUIVALENT"
  } else {
    "AMBIGUOUS"
  }
  structure(list(
    status = status,
    matched_n = sum(ok),
    source_monotonic = monotonic_a,
    roundtrip_monotonic = monotonic_b,
    offset_seconds = offset,
    affine_intercept_seconds = intercept,
    affine_slope = slope,
    max_centered_error_seconds = centered_error,
    interval_correlation = interval_cor,
    tolerance_seconds = tolerance
  ), class = "eye_timestamp_fidelity")
}

#' Coordinate semantic-fidelity audit
#'
#' Detects lossless preservation and stable affine coordinate transformations.
#'
#' @param source Original/source representation.
#' @param roundtrip Round-tripped or comparison representation.
#' @param source_x Source horizontal coordinate column.
#' @param source_y Source vertical coordinate column.
#' @param roundtrip_x Round-tripped horizontal coordinate column.
#' @param roundtrip_y Round-tripped vertical coordinate column.
#' @param key Column or columns used to align records.
#' @param tolerance Numerical tolerance used by the comparison.
#' @param correlation_floor Minimum correlation treated as compatible.
#' @return An object of class "eye_coordinate_fidelity", stored as a named list, with components "status", "x", "y", "matched_n", "tolerance", "correlation_floor". It contains coordinate semantic-fidelity audit and associated metadata or diagnostics needed to interpret the result.
#' @export
coordinate_fidelity_audit <- function(source, roundtrip,
                                      source_x = "x", source_y = "y",
                                      roundtrip_x = source_x, roundtrip_y = source_y,
                                      key = NULL,
                                      tolerance = 1e-6,
                                      correlation_floor = 0.999) {
  source <- .ep07_as_data_frame(source, "source")
  roundtrip <- .ep07_as_data_frame(roundtrip, "roundtrip")
  .ep07_req_cols(source, c(source_x, source_y), "source")
  .ep07_req_cols(roundtrip, c(roundtrip_x, roundtrip_y), "roundtrip")
  al <- .ep07_align_frames(source, roundtrip, key, TRUE)
  one_axis <- function(a, b) {
    a <- as.numeric(a); b <- as.numeric(b)
    ok <- is.finite(a) & is.finite(b)
    if (!any(ok)) return(c(intercept = NA, slope = NA, correlation = NA, error = NA))
    corv <- .ep07_safe_cor(a, b)
    if (sum(ok) >= 3L && stats::sd(a[ok]) > 0) {
      fit <- stats::lm(b[ok] ~ a[ok])
      cf <- stats::coef(fit)
      pred <- stats::predict(fit)
      c(intercept = unname(cf[1L]), slope = unname(cf[2L]), correlation = corv,
        error = max(abs(pred - b[ok])))
    } else {
      c(intercept = NA, slope = NA, correlation = corv, error = max(abs(a[ok] - b[ok])))
    }
  }
  ax <- one_axis(al$source[[source_x]], al$roundtrip[[roundtrip_x]])
  ay <- one_axis(al$source[[source_y]], al$roundtrip[[roundtrip_y]])
  direct <- isTRUE(.ep07_scale_equal(al$source[[source_x]], al$roundtrip[[roundtrip_x]], tolerance)) &&
    isTRUE(.ep07_scale_equal(al$source[[source_y]], al$roundtrip[[roundtrip_y]], tolerance))
  transformed <- all(is.finite(c(ax["correlation"], ay["correlation"]))) &&
    all(c(ax["correlation"], ay["correlation"]) >= correlation_floor) &&
    all(c(ax["error"], ay["error"]) <= tolerance)
  status <- if (direct) "LOSSLESS" else if (transformed) "COORDINATE_TRANSFORMED" else "AMBIGUOUS"
  structure(list(status = status, x = ax, y = ay, matched_n = al$matched_n,
                 tolerance = tolerance, correlation_floor = correlation_floor),
            class = "eye_coordinate_fidelity")
}

#' Pupil-unit semantic-fidelity audit
#'
#' @param source Original/source representation.
#' @param roundtrip Round-tripped or comparison representation.
#' @param source_pupil Source pupil-measure column.
#' @param roundtrip_pupil Round-tripped pupil-measure column.
#' @param key Column or columns used to align records.
#' @param tolerance Numerical tolerance used by the comparison.
#' @param correlation_floor Minimum correlation treated as compatible.
#' @return An object of class "eye_pupil_fidelity", stored as a named list, with components "status", "matched_n", "correlation", "estimated_scale_ratio", "scaled_max_error", "tolerance". It contains pupil-unit semantic-fidelity audit and associated metadata or diagnostics needed to interpret the result.
#' @export
pupil_unit_fidelity_audit <- function(source, roundtrip,
                                      source_pupil = "pupil_size",
                                      roundtrip_pupil = source_pupil,
                                      key = NULL,
                                      tolerance = 1e-6,
                                      correlation_floor = 0.995) {
  source <- .ep07_as_data_frame(source, "source")
  roundtrip <- .ep07_as_data_frame(roundtrip, "roundtrip")
  .ep07_req_cols(source, source_pupil, "source")
  .ep07_req_cols(roundtrip, roundtrip_pupil, "roundtrip")
  al <- .ep07_align_frames(source, roundtrip, key, TRUE)
  a <- as.numeric(al$source[[source_pupil]])
  b <- as.numeric(al$roundtrip[[roundtrip_pupil]])
  ok <- is.finite(a) & is.finite(b)
  corv <- .ep07_safe_cor(a, b)
  ratio <- b[ok] / a[ok]
  ratio <- ratio[is.finite(ratio) & abs(a[ok]) > .Machine$double.eps]
  scale_ratio <- if (length(ratio)) stats::median(ratio) else NA_real_
  direct <- isTRUE(.ep07_scale_equal(a, b, tolerance))
  scaled_error <- if (is.finite(scale_ratio) && any(ok)) max(abs(b[ok] - scale_ratio * a[ok])) else NA_real_
  status <- if (direct) {
    "LOSSLESS"
  } else if (is.finite(corv) && corv >= correlation_floor && is.finite(scaled_error) && scaled_error <= tolerance) {
    "UNIT_TRANSFORMED"
  } else {
    "AMBIGUOUS"
  }
  structure(list(status = status, matched_n = sum(ok), correlation = corv,
                 estimated_scale_ratio = scale_ratio, scaled_max_error = scaled_error,
                 tolerance = tolerance), class = "eye_pupil_fidelity")
}

#' Audit preservation of monocular/binocular stream semantics
#'
#' @param source Original/source representation.
#' @param roundtrip Round-tripped or comparison representation.
#' @param source_eye Source recorded-eye column.
#' @param roundtrip_eye Round-tripped recorded-eye column.
#' @param key Column or columns used to align records.
#' @return An object of class "eye_stream_fidelity", stored as a named list, with components "status", "matched_n", "source_streams", "roundtrip_streams", "confusion". It contains preservation of monocular/binocular stream semantics and associated metadata or diagnostics needed to interpret the result.
#' @export
eye_stream_fidelity_audit <- function(source, roundtrip,
                                      source_eye = "eye",
                                      roundtrip_eye = source_eye,
                                      key = NULL) {
  source <- .ep07_as_data_frame(source, "source")
  roundtrip <- .ep07_as_data_frame(roundtrip, "roundtrip")
  .ep07_req_cols(source, source_eye, "source")
  .ep07_req_cols(roundtrip, roundtrip_eye, "roundtrip")
  al <- .ep07_align_frames(source, roundtrip, key, TRUE)
  normalize <- function(x) {
    x <- tolower(trimws(as.character(x)))
    x[x %in% c("l", "left_eye", "lefteye")] <- "left"
    x[x %in% c("r", "right_eye", "righteye")] <- "right"
    x[x %in% c("both", "binocular", "combined", "cyclopean")] <- "cyclopean"
    x
  }
  a <- normalize(al$source[[source_eye]])
  b <- normalize(al$roundtrip[[roundtrip_eye]])
  same <- (a == b) | (is.na(a) & is.na(b))
  confusion <- table(source = a, roundtrip = b, useNA = "ifany")
  status <- if (all(same, na.rm = TRUE) && identical(is.na(a), is.na(b))) "LOSSLESS" else "AMBIGUOUS"
  structure(list(status = status, matched_n = al$matched_n,
                 source_streams = sort(unique(a)), roundtrip_streams = sort(unique(b)),
                 confusion = confusion), class = "eye_stream_fidelity")
}

#' Audit event semantic preservation
#'
#' @param source_events,roundtrip_events Event tables.
#' @param label Event-label field.
#' @param time Event-time field; set `NULL` to compare labels only.
#' @param key Optional event identity key.
#' @param tolerance Timestamp tolerance.
#' @return An object of class "eye_event_semantics", stored as a named list, with components "status", "source_n", "roundtrip_n", "matched_n", "exact_label_fraction", "max_time_error". It contains event semantic preservation and associated metadata or diagnostics needed to interpret the result.
#' @export
event_semantics_audit <- function(source_events, roundtrip_events,
                                  label = "event",
                                  time = "timestamp",
                                  key = NULL,
                                  tolerance = 1e-6) {
  source_events <- .ep07_as_data_frame(source_events, "source_events")
  roundtrip_events <- .ep07_as_data_frame(roundtrip_events, "roundtrip_events")
  .ep07_req_cols(source_events, label, "source_events")
  .ep07_req_cols(roundtrip_events, label, "roundtrip_events")
  al <- .ep07_align_frames(source_events, roundtrip_events, key, TRUE)
  a <- as.character(al$source[[label]])
  b <- as.character(al$roundtrip[[label]])
  label_ok <- (a == b) | (is.na(a) & is.na(b))
  time_error <- NA_real_
  if (!is.null(time) && time %in% names(al$source) && time %in% names(al$roundtrip)) {
    ta <- as.numeric(al$source[[time]]); tb <- as.numeric(al$roundtrip[[time]])
    ok <- is.finite(ta) & is.finite(tb)
    if (any(ok)) time_error <- max(abs(ta[ok] - tb[ok]))
  }
  status <- if (all(label_ok, na.rm = TRUE) &&
                (is.na(time_error) || time_error <= tolerance) &&
                al$matched_n == al$source_n && al$matched_n == al$roundtrip_n) {
    "LOSSLESS"
  } else if (all(label_ok, na.rm = TRUE)) {
    "SEMANTICALLY_EQUIVALENT"
  } else {
    "AMBIGUOUS"
  }
  structure(list(status = status, source_n = al$source_n, roundtrip_n = al$roundtrip_n,
                 matched_n = al$matched_n, exact_label_fraction = mean(label_ok, na.rm = TRUE),
                 max_time_error = time_error), class = "eye_event_semantics")
}

#' Minimal HED annotation audit for event tables
#'
#' This function checks presence, non-empty annotations and balanced grouping.
#' It is deliberately a structural audit rather than a full HED validator; use
#' the official HED validation tooling when formal schema validation is needed.
#'
#' @param events Value supplied to `events`; see Details for its model-specific role.
#' @param hed_column Column containing HED annotations.
#' @return A data frame containing minimal HED annotation audit for event tables. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
validate_hed_event_semantics <- function(events, hed_column = "HED") {
  events <- .ep07_as_data_frame(events, "events")
  .ep07_req_cols(events, hed_column, "events")
  x <- as.character(events[[hed_column]])
  nonempty <- !is.na(x) & nzchar(trimws(x))
  balanced_one <- function(z) {
    chars <- strsplit(z, "", fixed = TRUE)[[1L]]
    depth <- 0L
    for (ch in chars) {
      if (ch == "(") depth <- depth + 1L
      if (ch == ")") depth <- depth - 1L
      if (depth < 0L) return(FALSE)
    }
    depth == 0L
  }
  balanced <- rep(FALSE, length(x))
  balanced[nonempty] <- vapply(x[nonempty], balanced_one, logical(1))
  data.frame(
    row = seq_along(x),
    nonempty = nonempty,
    balanced_parentheses = balanced,
    structurally_valid = nonempty & balanced,
    stringsAsFactors = FALSE
  )
}

#' Validate BIDS eye-tracking semantics
#'
#' Implements a lightweight contract for BIDS 1.11.1 eye-tracking physiology
#' data. It does not replace the BIDS Validator.
#'
#' @param data Eye-tracking physiology table.
#' @param metadata Parsed JSON sidecar as a named list.
#' @param events_metadata Optional event-sidecar metadata containing
#'   `StimulusPresentation` information for gaze-on-screen recordings.
#' @return An `eye_bids_semantic_audit` object.
#' @export
validate_bids_eye_semantics <- function(data, metadata, events_metadata = NULL) {
  data <- .ep07_as_data_frame(data, "data")
  if (!is.list(metadata)) stop("`metadata` must be a named list.", call. = FALSE)
  required_cols <- c("timestamp", "x_coordinate", "y_coordinate")
  checks <- list()
  add <- function(name, pass, detail) {
    checks[[length(checks) + 1L]] <<- data.frame(check = name, pass = isTRUE(pass), detail = detail,
                                                 stringsAsFactors = FALSE)
  }
  add("required_columns", all(required_cols %in% names(data)),
      paste(setdiff(required_cols, names(data)), collapse = ", "))
  add("column_order", length(names(data)) >= 3L && identical(names(data)[1:3], required_cols),
      paste(utils::head(names(data), 3L), collapse = ", "))
  add("PhysioType", identical(metadata$PhysioType, "eyetrack"), as.character(metadata$PhysioType %||% NA_character_))
  eye <- metadata$RecordedEye
  add("RecordedEye", length(eye) == 1L && eye %in% c("left", "right", "cyclopean"), as.character(eye %||% NA_character_))
  cs <- metadata$SampleCoordinateSystem
  valid_cs <- c("gaze-on-screen", "eye-in-head", "gaze-in-world", "custom")
  add("SampleCoordinateSystem", length(cs) == 1L && cs %in% valid_cs, as.character(cs %||% NA_character_))
  if (identical(cs, "gaze-on-screen")) {
    sp <- if (is.list(events_metadata)) events_metadata$StimulusPresentation else NULL
    needed <- c("ScreenDistance", "ScreenOrigin", "ScreenResolution", "ScreenSize")
    present <- is.list(sp) && all(needed %in% names(sp))
    add("gaze_on_screen_stimulus_metadata", present,
        if (present) "complete" else paste("requires", paste(needed, collapse = ", ")))
  }
  if ("pupil_size" %in% names(data)) {
    pupil_meta <- metadata$pupil_size
    unit_known <- is.list(pupil_meta) && !is.null(pupil_meta$Units)
    add("pupil_units_described", unit_known, if (unit_known) as.character(pupil_meta$Units) else "missing")
  }
  tab <- do.call(rbind, checks)
  structure(list(valid = all(tab$pass), checks = tab, metadata = metadata),
            class = "eye_bids_semantic_audit")
}

# `%||%` is already used by eyeprocess. Keep a local fallback only if the
# package baseline does not define it yet when this file is sourced standalone.
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
}

#' Audit a complete semantic round trip
#'
#' @param source Original canonical samples.
#' @param roundtrip Canonical samples reconstructed after interchange.
#' @param key Optional row identity key.
#' @param fields Fields for field-level comparison.
#' @param timestamp Optional list of arguments forwarded to
#'   `timestamp_fidelity_audit()`.
#' @param coordinates Optional list of arguments forwarded to
#'   `coordinate_fidelity_audit()`.
#' @param pupil Optional list of arguments forwarded to
#'   `pupil_unit_fidelity_audit()`.
#' @param eye Optional list of arguments forwarded to
#'   `eye_stream_fidelity_audit()`.
#' @param source_events,roundtrip_events Optional event tables.
#' @param event_args Optional event-audit arguments.
#' @return An `eye_semantic_roundtrip` object.
#' @export
semantic_roundtrip_audit <- function(source, roundtrip, key = NULL, fields = NULL,
                                     timestamp = list(), coordinates = list(), pupil = NULL,
                                     eye = NULL, source_events = NULL, roundtrip_events = NULL,
                                     event_args = list()) {
  field <- field_fidelity_report(source, roundtrip, fields = fields, key = key)
  time_res <- try(do.call(timestamp_fidelity_audit,
                          c(list(source = source, roundtrip = roundtrip, key = key), timestamp)), silent = TRUE)
  coord_res <- try(do.call(coordinate_fidelity_audit,
                           c(list(source = source, roundtrip = roundtrip, key = key), coordinates)), silent = TRUE)
  pupil_res <- if (is.null(pupil)) NULL else try(do.call(pupil_unit_fidelity_audit,
                           c(list(source = source, roundtrip = roundtrip, key = key), pupil)), silent = TRUE)
  eye_res <- if (is.null(eye)) NULL else try(do.call(eye_stream_fidelity_audit,
                           c(list(source = source, roundtrip = roundtrip, key = key), eye)), silent = TRUE)
  event_res <- NULL
  if (!is.null(source_events) && !is.null(roundtrip_events)) {
    event_res <- try(do.call(event_semantics_audit,
                             c(list(source_events = source_events,
                                    roundtrip_events = roundtrip_events), event_args)), silent = TRUE)
  }
  component_status <- c(
    fields = if (all(field$fields$status == "LOSSLESS")) "LOSSLESS" else
      if (all(field$fields$status %in% c("LOSSLESS", "SEMANTICALLY_EQUIVALENT", "UNIT_TRANSFORMED", "COORDINATE_TRANSFORMED"))) "SEMANTICALLY_EQUIVALENT" else "AMBIGUOUS",
    timestamp = if (inherits(time_res, "try-error")) "UNSUPPORTED" else time_res$status,
    coordinates = if (inherits(coord_res, "try-error")) "UNSUPPORTED" else coord_res$status
  )
  if (!is.null(pupil_res)) component_status <- c(component_status, pupil = if (inherits(pupil_res, "try-error")) "UNSUPPORTED" else pupil_res$status)
  if (!is.null(eye_res)) component_status <- c(component_status, eye = if (inherits(eye_res, "try-error")) "UNSUPPORTED" else eye_res$status)
  if (!is.null(event_res)) component_status <- c(component_status, events = if (inherits(event_res, "try-error")) "UNSUPPORTED" else event_res$status)
  good <- component_status %in% c("LOSSLESS", "SEMANTICALLY_EQUIVALENT", "UNIT_TRANSFORMED", "COORDINATE_TRANSFORMED")
  overall <- if (all(component_status == "LOSSLESS")) "LOSSLESS" else if (all(good)) "SEMANTICALLY_EQUIVALENT" else "AMBIGUOUS"
  structure(list(overall = overall, component_status = component_status,
                 fields = field, timestamp = time_res, coordinates = coord_res,
                 pupil = pupil_res, eye = eye_res, events = event_res),
            class = "eye_semantic_roundtrip")
}

#' Convert a semantic round-trip audit into a loss map
#'
#' @param x Object to print, plot, summarize, or audit.
#' @return A tabular R object containing a semantic round-trip audit into a loss map; rows represent analysis units and columns contain the returned quantities.
#' @export
semantic_loss_map <- function(x) {
  if (!inherits(x, "eye_semantic_roundtrip")) stop("`x` must be an eye_semantic_roundtrip object.", call. = FALSE)
  field <- x$fields$fields
  component <- data.frame(
    scope = "component",
    name = names(x$component_status),
    status = unname(x$component_status),
    severity_rank = .ep07_fidelity_rank(unname(x$component_status)),
    stringsAsFactors = FALSE
  )
  fld <- data.frame(
    scope = "field",
    name = field$field,
    status = field$status,
    severity_rank = field$severity_rank,
    stringsAsFactors = FALSE
  )
  rbind(component, fld)
}

#' Public validation-corpus registry
#'
#' Returns conservative metadata only. The package deliberately does not
#' auto-download third-party human-participant data; users must review the
#' source licence/terms and obtain data from the authoritative repository.
#'
#' @return A data frame containing public validation-corpus registry. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
public_validation_corpus <- function() {
  data.frame(
    ecosystem = c(
      "Gazepoint", "Gazepoint", "EyeLink", "EyeLink", "EyeLink", "Tobii", "Tobii", "Pupil Labs"
    ),
    device = c(
      "GP3 HD 150 Hz", "GP3 HD 150 Hz", "EyeLink (raw EDF; device metadata verify in corpus)",
      "EyeLink 1000 1000 Hz", "EyeLink Portable Duo 1000 Hz",
      "Tobii Pro Fusion 120 Hz", "Tobii Pro Glasses 3 ~50 Hz", "Neon"
    ),
    corpus = c(
      "Pavia free observation of moving elements",
      "Pavia symmetric dynamic stimuli (2026)",
      "Raw eye-tracking data (EyeLink EDF; 10 subjects, 2026)",
      "GazeBase",
      "Eye movement benchmark data for smooth-pursuit classification (2026)",
      "MCFW-Gaze v3",
      "GroupAffect-4 v3",
      "Pupil Labs official Neon sample recording"
    ),
    evidence_goal = c(
      "independent-public-real; multisession-real; raw Gazepoint CSV",
      "independent-public-real; multisession-real; raw Gazepoint CSV",
      "raw-native-format; EDF parser validation",
      "large multisession longitudinal EyeLink benchmark",
      "raw EDF plus ASC conversion; event-parser benchmark",
      "remote-screen Tobii; binocular continuous raw gaze",
      "wearable Tobii; synchronized multimodal streams",
      "vendor-example; native/CSV semantic roundtrip"
    ),
    url = c(
      "https://vision.unipv.it/research/etanim/",
      "https://vision.unipv.it/research/etsymanim/",
      "https://zenodo.org/records/20780576",
      "https://figshare.com/articles/dataset/GazeBase_Data_Repository/12912257",
      "https://osf.io/zx7hc/",
      "https://zenodo.org/records/20300972",
      "https://zenodo.org/records/20796290",
      "https://docs.pupil-labs.com/neon/neon-player/getting-started/"
    ),
    access = c(
      "public", "public; research/education/non-commercial terms", "public",
      "public CC BY 4.0", "public OSF", "public Zenodo", "public Zenodo; some audio restricted",
      "vendor example"
    ),
    auto_download = FALSE,
    review_terms_before_use = TRUE,
    stringsAsFactors = FALSE
  )
}

#' Build a detailed compatibility evidence matrix
#'
#' @param compatibility Existing output from `build_compatibility_matrix()` or
#'   a compatible data frame.
#' @param evidence Case-level evidence with columns `ecosystem`, `device`,
#'   `evidence_level`, and optionally `semantic_roundtrip_pass`.
#' @return An `eye_compatibility_evidence_matrix` object.
#' @export
compatibility_evidence_matrix <- function(compatibility, evidence = NULL) {
  if (is.list(compatibility) && !is.data.frame(compatibility)) {
    candidates <- c("matrix", "compatibility", "data", "table")
    hit <- candidates[vapply(candidates, function(nm) is.data.frame(compatibility[[nm]]), logical(1))]
    if (length(hit)) compatibility <- compatibility[[hit[[1L]]]]
  }
  if (!is.data.frame(compatibility)) stop("`compatibility` must resolve to a data frame.", call. = FALSE)
  if (is.null(evidence)) {
    out <- compatibility
    out$detailed_evidence_level <- "declared"
    out$semantic_roundtrip_validated <- FALSE
    return(structure(out, class = c("eye_compatibility_evidence_matrix", class(out))))
  }
  if (!is.data.frame(evidence)) stop("`evidence` must be a data frame.", call. = FALSE)
  .ep07_req_cols(evidence, c("ecosystem", "device", "evidence_level"), "evidence")
  lv <- validation_evidence_levels()
  if (any(!evidence$evidence_level %in% lv$level)) {
    stop("Unknown `evidence_level`; use validation_evidence_levels() for allowed values.", call. = FALSE)
  }
  evidence$.rank <- lv$rank[match(evidence$evidence_level, lv$level)]
  if (!"semantic_roundtrip_pass" %in% names(evidence)) evidence$semantic_roundtrip_pass <- FALSE
  key <- paste(evidence$ecosystem, evidence$device, sep = "\r")
  split_e <- split(evidence, key)
  agg <- do.call(rbind, lapply(split_e, function(z) {
    best <- z[which.max(z$.rank), , drop = FALSE]
    data.frame(ecosystem = best$ecosystem[[1L]], device = best$device[[1L]],
               detailed_evidence_level = best$evidence_level[[1L]],
               semantic_roundtrip_validated = any(z$semantic_roundtrip_pass %in% TRUE),
               evidence_cases = nrow(z), stringsAsFactors = FALSE)
  }))
  rownames(agg) <- NULL
  join_cols <- intersect(c("ecosystem", "device"), names(compatibility))
  if (length(join_cols) < 2L) {
    stop("`compatibility` must contain `ecosystem` and `device` columns for detailed evidence joining.", call. = FALSE)
  }
  out <- merge(compatibility, agg, by = join_cols, all.x = TRUE, sort = FALSE)
  out$detailed_evidence_level[is.na(out$detailed_evidence_level)] <- "declared"
  out$semantic_roundtrip_validated[is.na(out$semantic_roundtrip_validated)] <- FALSE
  out$evidence_cases[is.na(out$evidence_cases)] <- 0L
  structure(out, class = c("eye_compatibility_evidence_matrix", class(out)))
}

#' Validate vendor-specific timestamp semantics
#'
#' The audit records expected semantics rather than silently coercing clocks.
#' Tobii data can carry device/system timing; Pupil Labs Neon timestamps are
#' high-resolution UTC nanoseconds in native recordings; Gazepoint may expose
#' native monotonic and media-relative clocks depending on export type.
#'
#' @param data Input data frame or compatible tabular object.
#' @param vendor Vendor identifier.
#' @param device_time Device timestamp column.
#' @param system_time System timestamp column.
#' @param media_time Media/stimulus timestamp column.
#' @return An object of class "eye_vendor_timestamp_semantics", stored as a named list, with components "vendor", "pass", "clocks". It contains vendor-specific timestamp semantics and associated metadata or diagnostics needed to interpret the result.
#' @export
validate_vendor_timestamp_semantics <- function(data, vendor,
                                                device_time = NULL,
                                                system_time = NULL,
                                                media_time = NULL) {
  data <- .ep07_as_data_frame(data, "data")
  vendor <- tolower(.ep07_scalar_chr(vendor, "vendor"))
  rows <- list()
  add <- function(clock, column, expected, required = FALSE) {
    present <- !is.null(column) && column %in% names(data)
    mono <- NA
    if (present && is.numeric(data[[column]])) {
      z <- data[[column]][is.finite(data[[column]])]
      mono <- length(z) < 2L || all(diff(z) >= 0)
    }
    rows[[length(rows) + 1L]] <<- data.frame(
      clock = clock, column = if (is.null(column)) NA_character_ else column,
      present = present, monotonic = mono, required = required,
      expected_semantics = expected, stringsAsFactors = FALSE
    )
  }
  if (grepl("tobii", vendor)) {
    add("device", device_time, "device-origin timestamp preserved without replacement", FALSE)
    add("system", system_time, "host/system timestamp retained separately when exported", FALSE)
  } else if (grepl("pupil|neon", vendor)) {
    add("native", device_time, "native high-resolution recording timestamp retained; unit/origin documented", TRUE)
  } else if (grepl("gazepoint", vendor)) {
    add("native", device_time, "native monotonic recording clock retained", TRUE)
    add("media", media_time, "media-relative clock retained separately when present", FALSE)
  } else {
    add("native", device_time, "native timestamp retained with unit and origin metadata", TRUE)
    add("system", system_time, "system timestamp retained separately when available", FALSE)
  }
  tab <- do.call(rbind, rows)
  structure(list(vendor = vendor, pass = all(!tab$required | tab$present), clocks = tab),
            class = "eye_vendor_timestamp_semantics")
}

#' Plot semantic round-trip fidelity
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot semantic round-trip fidelity.
#' @export
plot.eye_semantic_roundtrip <- function(x, ...) {
  loss <- semantic_loss_map(x)
  op <- graphics::par(mar = c(5, max(8, min(18, max(nchar(loss$name)) / 1.7)), 3, 1))
  on.exit(graphics::par(op), add = TRUE)
  score <- 10 - pmin(loss$severity_rank, 9)
  names(score) <- loss$name
  graphics::barplot(rev(score), horiz = TRUE, las = 1,
                    xlab = "Semantic fidelity (higher is better)",
                    main = sprintf("Semantic round trip: %s", x$overall), ...)
  invisible(x)
}

#' Plot detailed compatibility evidence
#' @param x Object to print, plot, summarize, or audit.
#' @param ... Additional arguments passed to the selected model, engine, or method.
#' @return Invisibly returns the plotting result when available; the primary effect is drawing plot detailed compatibility evidence.
#' @export
plot.eye_compatibility_evidence_matrix <- function(x, ...) {
  lv <- validation_evidence_levels()
  rank <- lv$rank[match(x$detailed_evidence_level, lv$level)]
  labels <- if (all(c("ecosystem", "device") %in% names(x))) paste(x$ecosystem, x$device, sep = " - ") else seq_len(nrow(x))
  op <- graphics::par(mar = c(5, max(9, min(22, max(nchar(labels)) / 1.6)), 3, 1))
  on.exit(graphics::par(op), add = TRUE)
  graphics::barplot(rev(rank), names.arg = rev(labels), horiz = TRUE, las = 1,
                    xlim = c(0, max(lv$rank) + 0.5),
                    xlab = "Detailed validation evidence level", ...)
  graphics::axis(1, at = lv$rank, labels = lv$level, las = 2, cex.axis = 0.7)
  invisible(x)
}
