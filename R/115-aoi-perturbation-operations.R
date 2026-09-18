#' Convert AOI margins from pixels to degrees of visual angle
#' @export
convert_aoi_margin_to_degrees <- function(margin_pixels, screen_width_px, screen_height_px, viewing_distance, physical_screen_size) {
  unname(.aoi_pair(margin_pixels, "margin_pixels") *
           .aoi_screen_geometry(screen_width_px, screen_height_px, viewing_distance, physical_screen_size))
}

#' Convert AOI margins from degrees of visual angle to pixels
#' @export
convert_aoi_margin_to_pixels <- function(margin_degrees, screen_width_px, screen_height_px, viewing_distance, physical_screen_size) {
  unname(.aoi_pair(margin_degrees, "margin_degrees") /
           .aoi_screen_geometry(screen_width_px, screen_height_px, viewing_distance, physical_screen_size))
}

#' Create one AOI perturbation specification
#' @export
aoi_perturbation_spec <- function(
    perturbation_id, operation = "baseline", margin_x = 0, margin_y = margin_x,
    translation_x = 0, translation_y = 0, unit = "px", screen_width_px = NULL,
    screen_height_px = NULL, viewing_distance = NULL, physical_screen_size = NULL,
    degrees_per_pixel = NULL, seed = NULL, boundary_policy = "warn") {
  if (!is.character(perturbation_id) || length(perturbation_id) != 1L || !nzchar(perturbation_id)) .aoi_stop("`perturbation_id` must be a non-empty string.")
  operation <- match.arg(operation, c("baseline", "dilation", "erosion", "translate", "jitter", "anisotropic_expansion"))
  unit <- match.arg(unit, c("px", "deg")); boundary_policy <- match.arg(boundary_policy, c("warn", "clip", "error", "allow"))
  mx <- .aoi_num(margin_x, "margin_x"); my <- .aoi_num(margin_y, "margin_y")
  tx <- .aoi_num(translation_x, "translation_x"); ty <- .aoi_num(translation_y, "translation_y")
  if (operation %in% c("dilation", "erosion") && (mx < 0 || my < 0)) .aoi_stop("Dilation/erosion margins must be non-negative.")
  if (operation %in% c("dilation", "erosion") && !isTRUE(all.equal(mx, my, tolerance = 1e-12))) {
    .aoi_stop("Polygon-safe dilation/erosion uses a uniform margin; use anisotropic expansion for x/y differences.")
  }
  dpp <- NULL
  if (!is.null(degrees_per_pixel)) {
    dpp <- .aoi_pair(degrees_per_pixel, "degrees_per_pixel", TRUE)
    if (any(dpp <= 0)) .aoi_stop("`degrees_per_pixel` must be positive.")
  } else if (!is.null(screen_width_px) && !is.null(screen_height_px) && !is.null(viewing_distance) && !is.null(physical_screen_size)) {
    dpp <- .aoi_screen_geometry(screen_width_px, screen_height_px, viewing_distance, physical_screen_size)
  }
  if (unit == "deg" && is.null(dpp)) .aoi_stop("Degree-based perturbations require `degrees_per_pixel` or complete screen geometry.")
  if (!is.null(seed)) {
    seed <- as.integer(seed)
    if (length(seed) != 1L || is.na(seed) || seed < 0) .aoi_stop("`seed` must be a non-negative integer.")
  }
  .aoi_result("eye_aoi_perturbation_spec", perturbation_id = perturbation_id, operation = operation,
              margin_x = mx, margin_y = my, translation_x = tx, translation_y = ty, unit = unit,
              screen_width_px = screen_width_px, screen_height_px = screen_height_px,
              viewing_distance = viewing_distance, physical_screen_size = physical_screen_size,
              degrees_per_pixel = dpp, seed = seed, boundary_policy = boundary_policy)
}

.aoi_spec_px <- function(spec) {
  out <- as.list(spec)
  if (identical(out$unit, "deg")) {
    out$margin_x <- out$margin_x / out$degrees_per_pixel[1L]; out$margin_y <- out$margin_y / out$degrees_per_pixel[2L]
    out$translation_x <- out$translation_x / out$degrees_per_pixel[1L]; out$translation_y <- out$translation_y / out$degrees_per_pixel[2L]
  }
  out
}

.aoi_apply_boundary <- function(geometry, spec) {
  if (is.null(spec$screen_width_px) || is.null(spec$screen_height_px)) return(geometry)
  width <- as.numeric(spec$screen_width_px); height <- as.numeric(spec$screen_height_px)
  outside <- vapply(seq_len(nrow(geometry)), function(i) {
    b <- .aoi_bounds(geometry[i, , drop = FALSE]); b[1L] < 0 || b[3L] < 0 || b[2L] > width || b[4L] > height
  }, logical(1))
  if (!any(outside)) return(geometry)
  msg <- paste0("Perturbed AOIs extend beyond the declared screen: ", paste(geometry$aoi_id[outside], collapse = ", "))
  if (spec$boundary_policy == "error") .aoi_stop(msg)
  if (spec$boundary_policy == "warn") { .aoi_warn(msg); return(geometry) }
  if (spec$boundary_policy == "allow") return(geometry)
  .aoi_warn(msg, "; clipping was explicitly requested.")
  for (i in which(outside)) {
    if (geometry$shape_type[i] == "rectangle") {
      geometry$xmin[i] <- max(0, min(width, geometry$xmin[i])); geometry$xmax[i] <- max(0, min(width, geometry$xmax[i]))
      geometry$ymin[i] <- max(0, min(height, geometry$ymin[i])); geometry$ymax[i] <- max(0, min(height, geometry$ymax[i]))
      if (geometry$xmin[i] >= geometry$xmax[i] || geometry$ymin[i] >= geometry$ymax[i]) .aoi_stop("Screen clipping collapsed AOI `", geometry$aoi_id[i], "`.")
    } else {
      p <- .aoi_polygon(geometry$polygon[[i]]); p[, 1L] <- pmin(width, pmax(0, p[, 1L])); p[, 2L] <- pmin(height, pmax(0, p[, 2L]))
      if (.aoi_self_intersects(p) || abs(.aoi_signed_area(p)) <= 1e-12) .aoi_stop("Screen clipping invalidated polygon AOI `", geometry$aoi_id[i], "`.")
      geometry$polygon[[i]] <- p
    }
  }
  geometry
}

.aoi_transform <- function(aois, spec) {
  geometry <- validate_aoi_geometry(aois)$geometry; sp <- .aoi_spec_px(spec)
  if (sp$operation == "baseline") return(.aoi_apply_boundary(geometry, sp))
  if (sp$operation == "jitter") {
    old <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit(if (is.null(old)) {
      if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) rm(".Random.seed", envir = .GlobalEnv)
    } else assign(".Random.seed", old, envir = .GlobalEnv), add = TRUE)
    set.seed(sp$seed)
  }
  for (i in seq_len(nrow(geometry))) {
    mx <- sp$margin_x; my <- sp$margin_y; tx <- sp$translation_x; ty <- sp$translation_y
    if (sp$operation == "jitter") {
      tx <- stats::runif(1L, -abs(if (tx != 0) tx else mx), abs(if (tx != 0) tx else mx))
      ty <- stats::runif(1L, -abs(if (ty != 0) ty else my), abs(if (ty != 0) ty else my))
    }
    if (geometry$shape_type[i] == "rectangle") {
      xmin <- geometry$xmin[i]; xmax <- geometry$xmax[i]; ymin <- geometry$ymin[i]; ymax <- geometry$ymax[i]
      if (sp$operation %in% c("dilation", "anisotropic_expansion")) {
        xmin <- xmin - mx; xmax <- xmax + mx; ymin <- ymin - my; ymax <- ymax + my
      } else if (sp$operation == "erosion") {
        xmin <- xmin + mx; xmax <- xmax - mx; ymin <- ymin + my; ymax <- ymax - my
      } else if (sp$operation %in% c("translate", "jitter")) {
        xmin <- xmin + tx; xmax <- xmax + tx; ymin <- ymin + ty; ymax <- ymax + ty
      }
      if (xmin >= xmax || ymin >= ymax) .aoi_stop("Perturbation `", sp$perturbation_id, "` collapsed AOI `", geometry$aoi_id[i], "`.")
      geometry$xmin[i] <- xmin; geometry$xmax[i] <- xmax; geometry$ymin[i] <- ymin; geometry$ymax[i] <- ymax
    } else {
      p <- .aoi_polygon(geometry$polygon[[i]])
      if (sp$operation %in% c("dilation", "erosion")) {
        if (identical(spec$unit, "deg")) {
          if (is.null(spec$degrees_per_pixel)) .aoi_stop("Degree-based polygon perturbation lacks degrees-per-pixel provenance.")
          p_angle <- p
          p_angle[, 1L] <- p_angle[, 1L] * spec$degrees_per_pixel[1L]
          p_angle[, 2L] <- p_angle[, 2L] * spec$degrees_per_pixel[2L]
          margin_angle <- spec$margin_x * if (sp$operation == "erosion") -1 else 1
          p_angle <- .aoi_offset_convex_polygon(p_angle, margin_angle)
          p <- p_angle
          p[, 1L] <- p[, 1L] / spec$degrees_per_pixel[1L]
          p[, 2L] <- p[, 2L] / spec$degrees_per_pixel[2L]
        } else {
          p <- .aoi_offset_convex_polygon(p, if (sp$operation == "dilation") mx else -mx)
        }
      }
      if (sp$operation %in% c("translate", "jitter")) { p[, 1L] <- p[, 1L] + tx; p[, 2L] <- p[, 2L] + ty }
      if (sp$operation == "anisotropic_expansion") {
        cx <- mean(p[, 1L]); cy <- mean(p[, 2L]); w <- diff(range(p[, 1L])); h <- diff(range(p[, 2L]))
        if (w <= 0 || h <= 0 || w + 2 * mx <= 0 || h + 2 * my <= 0) .aoi_stop("Anisotropic expansion collapsed polygon AOI `", geometry$aoi_id[i], "`.")
        p[, 1L] <- cx + (p[, 1L] - cx) * ((w + 2 * mx) / w); p[, 2L] <- cy + (p[, 2L] - cy) * ((h + 2 * my) / h)
      }
      if (.aoi_self_intersects(p) || abs(.aoi_signed_area(p)) <= 1e-12) .aoi_stop("Perturbation invalidated polygon AOI `", geometry$aoi_id[i], "`.")
      geometry$polygon[[i]] <- p
    }
  }
  geometry <- .aoi_apply_boundary(geometry, sp); validate_aoi_geometry(geometry); geometry
}

#' Dilate AOI geometry
#' @export
dilate_aoi <- function(aois, margin, ...) {
  m <- .aoi_pair(margin, "margin", TRUE)
  .aoi_transform(aois, aoi_perturbation_spec("dilation", "dilation", margin_x = m[1L], margin_y = m[2L], ...))
}

#' Erode AOI geometry
#' @export
erode_aoi <- function(aois, margin, ...) {
  m <- .aoi_pair(margin, "margin", TRUE)
  .aoi_transform(aois, aoi_perturbation_spec("erosion", "erosion", margin_x = m[1L], margin_y = m[2L], ...))
}

#' Translate AOI geometry
#' @export
translate_aoi <- function(aois, x = 0, y = 0, ...) .aoi_transform(aois, aoi_perturbation_spec("translation", "translate", translation_x = x, translation_y = y, ...))

#' Jitter AOI geometry reproducibly
#' @export
jitter_aoi <- function(aois, x, y = x, seed = 20260918L, ...) .aoi_transform(aois, aoi_perturbation_spec("jitter", "jitter", translation_x = x, translation_y = y, seed = seed, ...))

#' Apply one AOI perturbation
#' @export
perturb_aoi_geometry <- function(aois, spec) {
  nominal <- validate_aoi_geometry(aois); transformed <- .aoi_transform(nominal$geometry, spec)
  .aoi_result("eye_aoi_perturbation", perturbation_id = spec$perturbation_id, specification = as.list(spec),
              nominal_geometry = nominal$geometry, perturbed_geometry = transformed,
              reference_geometry_hash = nominal$source_hash, perturbed_geometry_hash = .aoi_hash(transformed))
}

#' Create a deterministic AOI perturbation grid
#' @export
create_aoi_perturbation_grid <- function(
    dilations = NULL, erosions = NULL, translations_x = NULL, translations_y = NULL,
    translations_xy = NULL, jitters = NULL, anisotropic = NULL,
    unit = "px", include_baseline = TRUE,
    seed = 20260918L, ...) {
  specs <- list(); add <- function(x) specs[[length(specs) + 1L]] <<- x
  if (isTRUE(include_baseline)) add(aoi_perturbation_spec("baseline", "baseline", unit = unit, ...))
  for (v in if (is.null(dilations)) numeric() else dilations) add(aoi_perturbation_spec(paste0("dilate_", v, "_", unit), "dilation", margin_x = v, unit = unit, ...))
  for (v in if (is.null(erosions)) numeric() else erosions) add(aoi_perturbation_spec(paste0("erode_", v, "_", unit), "erosion", margin_x = v, unit = unit, ...))
  for (v in if (is.null(translations_x)) numeric() else translations_x) add(aoi_perturbation_spec(paste0("shift_x_", v, "_", unit), "translate", translation_x = v, unit = unit, ...))
  for (v in if (is.null(translations_y)) numeric() else translations_y) add(aoi_perturbation_spec(paste0("shift_y_", v, "_", unit), "translate", translation_y = v, unit = unit, ...))
  normalise_pairs <- function(x, arg) {
    if (is.null(x)) return(list())
    if (is.matrix(x) || is.data.frame(x)) {
      if (ncol(x) != 2L) .aoi_stop(arg, " must have exactly two columns.")
      return(lapply(seq_len(nrow(x)), function(i) as.numeric(x[i, , drop = TRUE])))
    }
    if (is.numeric(x) && length(x) == 2L) return(list(as.numeric(x)))
    if (!is.list(x)) .aoi_stop(arg, " must be a length-two numeric vector, two-column matrix/data frame, or list of pairs.")
    lapply(x, function(v) .aoi_pair(v, arg))
  }

  for (p in normalise_pairs(translations_xy, "translations_xy")) {
    p <- .aoi_pair(p, "translations_xy")
    add(aoi_perturbation_spec(
      paste0("shift_xy_", p[1L], "_", p[2L], "_", unit),
      "translate", translation_x = p[1L], translation_y = p[2L], unit = unit, ...
    ))
  }
  if (!is.null(jitters)) for (i in seq_along(jitters)) {
    v <- jitters[[i]]
    add(aoi_perturbation_spec(paste0("jitter_", v, "_", unit, "_", i), "jitter", translation_x = v, translation_y = v, unit = unit, seed = seed + i - 1L, ...))
  }
  for (p in normalise_pairs(anisotropic, "anisotropic")) {
    p <- .aoi_pair(p, "anisotropic")
    add(aoi_perturbation_spec(paste0("anisotropic_", p[1L], "_", p[2L], "_", unit), "anisotropic_expansion", margin_x = p[1L], margin_y = p[2L], unit = unit, ...))
  }
  if (!length(specs)) .aoi_stop("The perturbation grid is empty.")
  ids <- vapply(specs, function(x) x$perturbation_id, character(1)); if (anyDuplicated(ids)) .aoi_stop("Generated perturbation IDs are not unique.")
  table <- do.call(rbind, lapply(specs, function(x) data.frame(
    perturbation_id = x$perturbation_id, operation = x$operation, margin_x = x$margin_x, margin_y = x$margin_y,
    translation_x = x$translation_x, translation_y = x$translation_y, unit = x$unit,
    seed = if (is.null(x$seed)) NA_integer_ else x$seed, boundary_policy = x$boundary_policy, stringsAsFactors = FALSE)))
  .aoi_result("eye_aoi_perturbation_grid", specifications = specs, table = table)
}

#' Apply a perturbation grid to AOIs
#' @export
apply_aoi_perturbation_grid <- function(aois, grid) {
  if (!inherits(grid, "eye_aoi_perturbation_grid")) .aoi_stop("`grid` must be created by `create_aoi_perturbation_grid()`.")
  geometries <- list(); audit <- list()
  for (spec in grid$specifications) {
    pid <- spec$perturbation_id; out <- tryCatch(.aoi_transform(aois, spec), error = identity)
    if (inherits(out, "error")) audit[[length(audit) + 1L]] <- data.frame(perturbation_id = pid, status = "failed", message = conditionMessage(out), geometry_hash = NA_character_)
    else {
      geometries[[pid]] <- out
      audit[[length(audit) + 1L]] <- data.frame(perturbation_id = pid, status = "completed", message = NA_character_, geometry_hash = .aoi_hash(out))
    }
  }
  .aoi_result("eye_aoi_perturbation_grid_result", geometries = geometries, audit = do.call(rbind, audit), grid = grid,
              nominal_hash = validate_aoi_geometry(aois)$source_hash)
}

.aoi_assign_points <- function(data, geometry, x_col, y_col, overlap_policy = "ambiguous") {
  if (!is.data.frame(data) || !all(c(x_col, y_col) %in% names(data))) .aoi_stop("Coordinate columns are absent from `data`.")
  overlap_policy <- match.arg(overlap_policy, c("ambiguous", "all", "error"))
  geom <- validate_aoi_geometry(geometry)$geometry; x <- suppressWarnings(as.numeric(data[[x_col]])); y <- suppressWarnings(as.numeric(data[[y_col]]))
  missing <- !is.finite(x) | !is.finite(y); hit <- matrix(FALSE, nrow(data), nrow(geom))
  for (j in seq_len(nrow(geom))) hit[, j] <- .aoi_contains(geom[j, , drop = FALSE], x, y) & !missing
  counts <- rowSums(hit); labels <- rep(.aoi_outside, nrow(data)); labels[missing] <- NA_character_
  for (i in which(counts == 1L)) labels[i] <- geom$aoi_id[which(hit[i, ])[1L]]
  amb <- which(counts > 1L)
  if (length(amb)) {
    if (overlap_policy == "error") .aoi_stop(length(amb), " observations have ambiguous overlapping AOI membership.")
    if (overlap_policy == "ambiguous") labels[amb] <- .aoi_ambiguous else for (i in amb) labels[i] <- paste(geom$aoi_id[hit[i, ]], collapse = "|")
  }
  labels
}

