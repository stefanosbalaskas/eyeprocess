register_coordinate_space <- function(x, space, overwrite = FALSE) {
  .assert_eye_dataset(x)
  .assert_data_frame(space, "space")
  space <- standardize_eye_table(space, "coordinate_spaces")
  ids <- space$coordinate_space_id
  if (!isTRUE(overwrite) && any(ids %in% x$coordinate_spaces$coordinate_space_id)) {
    .eye_stop("Coordinate-space id already exists: ", paste(intersect(ids, x$coordinate_spaces$coordinate_space_id), collapse = ", "), ".")
  }
  if (isTRUE(overwrite)) x$coordinate_spaces <- x$coordinate_spaces[!x$coordinate_spaces$coordinate_space_id %in% ids, , drop = FALSE]
  x$coordinate_spaces <- standardize_eye_table(.bind_rows_base(x$coordinate_spaces, space), "coordinate_spaces")
  add_provenance(x, "register_coordinate_space", "coordinate_spaces", paste(ids, collapse = ","))
}

coordinate_space <- function(x, id) {
  .assert_eye_dataset(x)
  idx <- match(id, x$coordinate_spaces$coordinate_space_id)
  if (anyNA(idx)) .eye_stop("Unknown coordinate-space id: ", paste(id[is.na(idx)], collapse = ", "), ".")
  x$coordinate_spaces[idx, , drop = FALSE]
}

.list_supported_coordinate_transforms <- function() {
  c(
    "display_normalized_top_left",
    "display_pixels_top_left",
    "surface_normalized_bottom_left",
    "world_camera_pixels",
    "reference_image_pixels"
  )
}

convert_xy <- function(x, y, from, to, from_width = NA_real_, from_height = NA_real_, to_width = NA_real_, to_height = NA_real_, clip = FALSE) {
  supported <- .list_supported_coordinate_transforms()
  if (!from %in% supported || !to %in% supported) .eye_stop("Automatic conversion is limited to supported 2D coordinate spaces.")
  x <- as.numeric(x); y <- as.numeric(y)
  # Convert source to normalized top-left.
  if (from %in% c("display_pixels_top_left", "world_camera_pixels", "reference_image_pixels")) {
    if (!is.finite(from_width) || !is.finite(from_height)) .eye_stop("Source width and height are required for pixel conversion.")
    xn <- x / from_width
    yn <- y / from_height
  } else if (from == "surface_normalized_bottom_left") {
    xn <- x
    yn <- 1 - y
  } else {
    xn <- x; yn <- y
  }
  # Convert normalized top-left to destination.
  if (to %in% c("display_pixels_top_left", "world_camera_pixels", "reference_image_pixels")) {
    if (!is.finite(to_width) || !is.finite(to_height)) .eye_stop("Destination width and height are required for pixel conversion.")
    xo <- xn * to_width
    yo <- yn * to_height
  } else if (to == "surface_normalized_bottom_left") {
    xo <- xn
    yo <- 1 - yn
  } else {
    xo <- xn; yo <- yn
  }
  if (isTRUE(clip)) {
    if (to %in% c("display_normalized_top_left", "surface_normalized_bottom_left")) {
      xo <- pmin(1, pmax(0, xo)); yo <- pmin(1, pmax(0, yo))
    } else {
      xo <- pmin(to_width, pmax(0, xo)); yo <- pmin(to_height, pmax(0, yo))
    }
  }
  data.frame(x = xo, y = yo)
}

convert_coordinates <- function(
    x,
    from,
    to,
    components = c("gaze_samples", "episodes", "aoi_geometry"),
    clip = FALSE,
    overwrite = FALSE) {
  .assert_eye_dataset(x)
  from_row <- coordinate_space(x, from)
  to_row <- coordinate_space(x, to)
  for (component in components) {
    d <- x[[component]]
    if (!nrow(d) || !"coordinate_space_id" %in% names(d)) next
    idx <- which(d$coordinate_space_id == from)
    if (!length(idx)) next
    if (component == "gaze_samples") {
      xy <- convert_xy(d$gaze_x[idx], d$gaze_y[idx], from_row$space_type, to_row$space_type,
        from_row$width, from_row$height, to_row$width, to_row$height, clip)
      if (overwrite) {
        d$gaze_x[idx] <- xy$x; d$gaze_y[idx] <- xy$y; d$coordinate_space_id[idx] <- to
      } else {
        copy <- d[idx, , drop = FALSE]
        copy$sample_id <- paste0(copy$sample_id, "_", to)
        copy$gaze_x <- xy$x; copy$gaze_y <- xy$y; copy$coordinate_space_id <- to
        d <- .bind_rows_base(d, copy)
      }
    } else if (component == "episodes") {
      for (pair in list(c("centroid_x", "centroid_y"), c("start_x", "start_y"), c("end_x", "end_y"))) {
        xy <- convert_xy(d[[pair[1L]]][idx], d[[pair[2L]]][idx], from_row$space_type, to_row$space_type,
          from_row$width, from_row$height, to_row$width, to_row$height, clip)
        d[[pair[1L]]][idx] <- xy$x; d[[pair[2L]]][idx] <- xy$y
      }
      d$coordinate_space_id[idx] <- to
    } else if (component == "aoi_geometry") {
      xy <- convert_xy(d$x[idx], d$y[idx], from_row$space_type, to_row$space_type,
        from_row$width, from_row$height, to_row$width, to_row$height, clip)
      d$x[idx] <- xy$x; d$y[idx] <- xy$y
      if (all(is.finite(c(from_row$width, from_row$height, to_row$width, to_row$height)))) {
        d$width[idx] <- d$width[idx] / from_row$width * to_row$width
        d$height[idx] <- d$height[idx] / from_row$height * to_row$height
      }
      d$coordinate_space_id[idx] <- to
    }
    x[[component]] <- standardize_eye_table(d, component)
  }
  add_provenance(
    x, "convert_coordinates", paste(components, collapse = ","),
    paste0(from, " -> ", to, "; clip=", clip, "; overwrite=", overwrite),
    reversible = !clip
  )
}

audit_coordinate_spaces <- function(x) {
  .assert_eye_dataset(x)
  used <- unique(c(
    x$gaze_samples$coordinate_space_id,
    x$episodes$coordinate_space_id,
    x$aoi_geometry$coordinate_space_id
  ))
  used <- stats::na.omit(used)
  registered <- x$coordinate_spaces$coordinate_space_id
  data.frame(
    coordinate_space_id = used,
    registered = used %in% registered,
    n_gaze = vapply(used, function(id) sum(x$gaze_samples$coordinate_space_id == id, na.rm = TRUE), integer(1)),
    n_episodes = vapply(used, function(id) sum(x$episodes$coordinate_space_id == id, na.rm = TRUE), integer(1)),
    n_aoi_geometry = vapply(used, function(id) sum(x$aoi_geometry$coordinate_space_id == id, na.rm = TRUE), integer(1)),
    status = ifelse(used %in% registered, "ok", "error"),
    stringsAsFactors = FALSE
  )
}

estimate_sampling_rate <- function(timestamp_seconds, trim = 0.05) {
  t <- sort(unique(as.numeric(timestamp_seconds[is.finite(timestamp_seconds)])))
  if (length(t) < 2L) return(NA_real_)
  dt <- diff(t)
  dt <- dt[dt > 0 & is.finite(dt)]
  if (!length(dt)) return(NA_real_)
  if (length(dt) > 10L && trim > 0) {
    q <- stats::quantile(dt, c(trim, 1 - trim), na.rm = TRUE)
    dt <- dt[dt >= q[1L] & dt <= q[2L]]
  }
  1 / stats::median(dt, na.rm = TRUE)
}

normalize_timebase <- function(
    x,
    component = c("gaze_samples", "eye_samples", "events", "biometrics"),
    native_unit = NULL,
    origin = c("recording_start", "absolute", "first_observation"),
    overwrite = TRUE) {
  .assert_eye_dataset(x)
  origin <- match.arg(origin)
  components <- match.arg(component, several.ok = TRUE)
  for (nm in components) {
    d <- x[[nm]]
    if (!nrow(d) || !"timestamp_native" %in% names(d)) next
    unit <- native_unit
    if (is.null(unit) && nrow(x$streams)) {
      unit <- .first_nonmissing(x$streams$timestamp_unit, "seconds")
    }
    unit <- unit %||% "seconds"
    seconds <- .safe_numeric(d$timestamp_native) * .time_multiplier(unit)
    if (origin %in% c("recording_start", "first_observation")) {
      groups <- split(seq_len(nrow(d)), d$recording_id)
      for (idx in groups) {
        first <- min(seconds[idx], na.rm = TRUE)
        if (is.finite(first)) seconds[idx] <- seconds[idx] - first
      }
    }
    if (overwrite || all(is.na(d$timestamp_seconds))) d$timestamp_seconds <- seconds
    x[[nm]] <- d
  }
  add_provenance(x, "normalize_timebase", paste(components, collapse = ","), paste0("unit=", native_unit %||% "stream/default", ";origin=", origin))
}

audit_timebase <- function(x, component = "gaze_samples") {
  .assert_eye_dataset(x)
  d <- x[[component]]
  if (!nrow(d)) return(data.frame())
  .assert_columns(d, c("recording_id", "timestamp_seconds"), component)
  groups <- split(d, d$recording_id)
  do.call(rbind, lapply(groups, function(z) {
    t <- z$timestamp_seconds
    ord <- order(t, na.last = NA)
    dt <- diff(t[ord])
    data.frame(
      recording_id = z$recording_id[1L], component = component,
      n = nrow(z), n_missing = sum(!is.finite(t)),
      n_nonmonotonic = sum(diff(t[is.finite(t)]) < 0, na.rm = TRUE),
      n_duplicate_time = sum(duplicated(t[is.finite(t)])),
      median_interval_ms = if (length(dt)) stats::median(dt[dt > 0], na.rm = TRUE) * 1000 else NA_real_,
      estimated_hz = estimate_sampling_rate(t),
      max_gap_ms = if (length(dt)) max(dt, na.rm = TRUE) * 1000 else NA_real_,
      status = if (any(diff(t[is.finite(t)]) < 0)) "warning" else "ok",
      stringsAsFactors = FALSE
    )
  }))
}

align_clock <- function(timestamp, offset = 0, slope = 1) {
  as.numeric(timestamp) * as.numeric(slope) + as.numeric(offset)
}

estimate_clock_transform <- function(source_times, target_times, method = c("linear", "offset")) {
  method <- match.arg(method)
  source_times <- as.numeric(source_times); target_times <- as.numeric(target_times)
  ok <- is.finite(source_times) & is.finite(target_times)
  if (sum(ok) < 1L) .eye_stop("No valid marker pairs for clock alignment.")
  if (method == "offset" || sum(ok) < 2L) {
    slope <- 1
    offset <- stats::median(target_times[ok] - source_times[ok])
    residuals <- target_times[ok] - align_clock(source_times[ok], offset, slope)
  } else {
    fit <- stats::lm(target_times[ok] ~ source_times[ok])
    offset <- unname(stats::coef(fit)[1L]); slope <- unname(stats::coef(fit)[2L])
    residuals <- stats::residuals(fit)
  }
  structure(list(
    method = method, offset = offset, slope = slope,
    n_markers = sum(ok), residual_sd = stats::sd(residuals),
    max_abs_residual = max(abs(residuals), na.rm = TRUE)
  ), class = "eye_clock_transform")
}

print.eye_clock_transform <- function(x, ...) {
  cat("<eye_clock_transform>\n")
  cat("  Method:   ", x$method, "\n", sep = "")
  cat("  Offset:   ", format(x$offset, digits = 8), " seconds\n", sep = "")
  cat("  Slope:    ", format(x$slope, digits = 8), "\n", sep = "")
  cat("  Markers:  ", x$n_markers, "\n", sep = "")
  invisible(x)
}

apply_clock_transform <- function(x, transform, components = c("biometrics"), source_clock = NULL) {
  .assert_eye_dataset(x)
  if (!inherits(transform, "eye_clock_transform") && !is.list(transform)) .eye_stop("`transform` must be an eye clock transform.")
  for (nm in components) {
    d <- x[[nm]]
    if (!nrow(d) || !"timestamp_seconds" %in% names(d)) next
    d$timestamp_seconds <- align_clock(d$timestamp_seconds, transform$offset, transform$slope)
    x[[nm]] <- d
  }
  add_provenance(
    x, "apply_clock_transform", paste(components, collapse = ","),
    paste0("offset=", transform$offset, ";slope=", transform$slope),
    reversible = is.finite(transform$slope) && transform$slope != 0
  )
}

synchronize_eye_biometrics <- function(
    gaze,
    biometrics,
    source_markers = NULL,
    target_markers = NULL,
    method = c("linear", "offset", "none"),
    resolve_ids = FALSE) {
  .assert_eye_dataset(gaze); .assert_eye_dataset(biometrics)
  method <- match.arg(method)
  transform <- structure(list(offset = 0, slope = 1, method = "none"), class = "eye_clock_transform")
  if (method != "none") {
    if (is.null(source_markers) || is.null(target_markers)) {
      .eye_warn("Marker pairs were not supplied; estimating offset from first observations.")
      source_markers <- min(biometrics$biometrics$timestamp_seconds, na.rm = TRUE)
      target_markers <- min(gaze$gaze_samples$timestamp_seconds, na.rm = TRUE)
      method <- "offset"
    }
    transform <- estimate_clock_transform(source_markers, target_markers, method)
    biometrics <- apply_clock_transform(biometrics, transform, components = c("biometrics", "events", "eye_samples"))
  }
  out <- combine_eye_datasets(gaze, biometrics, resolve_ids = resolve_ids)
  out <- add_provenance(out, "synchronize_eye_biometrics", "biometrics", paste0("method=", method), reversible = method != "linear" || is.finite(transform$slope))
  out
}

audit_clock_sync <- function(x, channel = NULL) {
  .assert_eye_dataset(x)
  if (!nrow(x$gaze_samples) || !nrow(x$biometrics)) return(data.frame(status = "unavailable", message = "Gaze and biometric streams are both required."))
  b <- x$biometrics
  if (!is.null(channel)) b <- b[b$channel %in% channel, , drop = FALSE]
  recs <- intersect(unique(x$gaze_samples$recording_id), unique(b$recording_id))
  do.call(rbind, lapply(recs, function(id) {
    gt <- range(x$gaze_samples$timestamp_seconds[x$gaze_samples$recording_id == id], na.rm = TRUE)
    bt <- range(b$timestamp_seconds[b$recording_id == id], na.rm = TRUE)
    overlap <- max(0, min(gt[2L], bt[2L]) - max(gt[1L], bt[1L]))
    union <- max(gt[2L], bt[2L]) - min(gt[1L], bt[1L])
    data.frame(
      recording_id = id, gaze_start = gt[1L], gaze_end = gt[2L],
      biometric_start = bt[1L], biometric_end = bt[2L],
      overlap_seconds = overlap, overlap_fraction = if (union > 0) overlap / union else NA_real_,
      start_offset_seconds = bt[1L] - gt[1L], end_offset_seconds = bt[2L] - gt[2L],
      status = if (overlap > 0) "overlap" else "no_overlap", stringsAsFactors = FALSE
    )
  }))
}
