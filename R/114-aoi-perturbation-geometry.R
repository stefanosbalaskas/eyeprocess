# AOI perturbation and uncertainty analysis ----------------------------------

.aoi_outside <- "__outside__"
.aoi_ambiguous <- "__ambiguous__"

.aoi_stop <- function(...) stop(..., call. = FALSE)
.aoi_warn <- function(...) warning(..., call. = FALSE)
.aoi_result <- function(class_name, ...) structure(list(...), class = c(class_name, "list"))

.aoi_num <- function(x, name, nonnegative = FALSE) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) != 1L || !is.finite(out)) .aoi_stop("`", name, "` must be a finite numeric scalar.")
  if (isTRUE(nonnegative) && out < 0) .aoi_stop("`", name, "` must be non-negative.")
  out
}

.aoi_pair <- function(x, name, nonnegative = FALSE) {
  out <- suppressWarnings(as.numeric(x))
  if (length(out) == 1L) out <- rep(out, 2L)
  if (length(out) != 2L || any(!is.finite(out))) .aoi_stop("`", name, "` must be a finite scalar or length-2 numeric vector.")
  if (isTRUE(nonnegative) && any(out < 0)) .aoi_stop("`", name, "` must be non-negative.")
  out
}

.aoi_hash <- function(x) {
  raw <- serialize(x, NULL, version = 2L)
  paste0(format(sum(as.integer(raw)) %% 2147483647L, scientific = FALSE), "-", length(raw))
}

.aoi_polygon <- function(x) {
  if (is.list(x) && length(x) == 1L && is.matrix(x[[1L]])) x <- x[[1L]]
  if (!is.matrix(x) || ncol(x) != 2L || nrow(x) < 3L) {
    .aoi_stop("Polygon geometry must be an n x 2 numeric matrix with at least three vertices.")
  }
  storage.mode(x) <- "double"
  if (any(!is.finite(x))) .aoi_stop("Polygon vertices must be finite.")
  if (nrow(x) >= 2L && isTRUE(all.equal(x[1L, ], x[nrow(x), ], tolerance = 1e-12))) x <- x[-nrow(x), , drop = FALSE]
  if (nrow(x) < 3L) .aoi_stop("Polygon geometry must contain at least three unique vertices.")
  x
}

.aoi_signed_area <- function(poly) {
  x <- poly[, 1L]; y <- poly[, 2L]; n <- nrow(poly)
  0.5 * sum(x * y[c(2:n, 1L)] - x[c(2:n, 1L)] * y)
}

.aoi_orientation <- function(a, b, c) (b[1L] - a[1L]) * (c[2L] - a[2L]) - (b[2L] - a[2L]) * (c[1L] - a[1L])

.aoi_on_segment <- function(a, b, p, tol = 1e-12) {
  p[1L] >= min(a[1L], b[1L]) - tol && p[1L] <= max(a[1L], b[1L]) + tol &&
    p[2L] >= min(a[2L], b[2L]) - tol && p[2L] <= max(a[2L], b[2L]) + tol &&
    abs(.aoi_orientation(a, b, p)) <= tol
}

.aoi_segments_intersect <- function(a, b, c, d) {
  o1 <- .aoi_orientation(a, b, c); o2 <- .aoi_orientation(a, b, d)
  o3 <- .aoi_orientation(c, d, a); o4 <- .aoi_orientation(c, d, b); tol <- 1e-12
  if (((o1 > tol && o2 < -tol) || (o1 < -tol && o2 > tol)) &&
      ((o3 > tol && o4 < -tol) || (o3 < -tol && o4 > tol))) return(TRUE)
  if (abs(o1) <= tol && .aoi_on_segment(a, b, c)) return(TRUE)
  if (abs(o2) <= tol && .aoi_on_segment(a, b, d)) return(TRUE)
  if (abs(o3) <= tol && .aoi_on_segment(c, d, a)) return(TRUE)
  if (abs(o4) <= tol && .aoi_on_segment(c, d, b)) return(TRUE)
  FALSE
}

.aoi_self_intersects <- function(poly) {
  n <- nrow(poly)
  if (n < 4L) return(FALSE)
  for (i in seq_len(n)) {
    i2 <- if (i == n) 1L else i + 1L
    for (j in seq_len(n)) {
      j2 <- if (j == n) 1L else j + 1L
      if (j <= i || j %in% c(i, i2) || j2 %in% c(i, i2)) next
      if (i == 1L && j2 == 1L) next
      if (.aoi_segments_intersect(poly[i, ], poly[i2, ], poly[j, ], poly[j2, ])) return(TRUE)
    }
  }
  FALSE
}

.aoi_is_convex <- function(poly) {
  n <- nrow(poly); signs <- integer()
  for (i in seq_len(n)) {
    j <- if (i == n) 1L else i + 1L
    k <- if (j == n) 1L else j + 1L
    cross <- .aoi_orientation(poly[i, ], poly[j, ], poly[k, ])
    if (abs(cross) > 1e-12) signs <- c(signs, if (cross > 0) 1L else -1L)
  }
  length(signs) > 0L && length(unique(signs)) == 1L
}

.aoi_line_intersection <- function(p1, d1, p2, d2) {
  cross <- d1[1L] * d2[2L] - d1[2L] * d2[1L]
  if (abs(cross) <= 1e-12) .aoi_stop("Polygon offset produced parallel adjacent edges; simplify the polygon.")
  q <- p2 - p1
  t <- (q[1L] * d2[2L] - q[2L] * d2[1L]) / cross
  p1 + t * d1
}

.aoi_offset_convex_polygon <- function(poly, distance) {
  if (!.aoi_is_convex(poly)) {
    .aoi_stop("True dilation/erosion is supported for convex polygons only. Concave polygons are not silently convexified or approximated.")
  }
  area <- .aoi_signed_area(poly)
  if (abs(area) <= 1e-12) .aoi_stop("Polygon area must be greater than zero.")
  ccw <- area > 0; n <- nrow(poly); shifted <- vector("list", n)
  for (i in seq_len(n)) {
    j <- if (i == n) 1L else i + 1L
    edge <- poly[j, ] - poly[i, ]; len <- sqrt(sum(edge^2))
    if (len <= 1e-12) .aoi_stop("Polygon contains a zero-length edge.")
    outward <- if (ccw) c(edge[2L], -edge[1L]) / len else c(-edge[2L], edge[1L]) / len
    shifted[[i]] <- list(point = poly[i, ] + distance * outward, direction = edge)
  }
  out <- matrix(NA_real_, n, 2L)
  for (i in seq_len(n)) {
    prev <- if (i == 1L) n else i - 1L
    out[i, ] <- .aoi_line_intersection(shifted[[prev]]$point, shifted[[prev]]$direction,
                                      shifted[[i]]$point, shifted[[i]]$direction)
  }
  new_area <- .aoi_signed_area(out)
  if (abs(new_area) <= 1e-12 || .aoi_self_intersects(out) || sign(new_area) != sign(area)) {
    .aoi_stop("Erosion collapsed or invalidated polygon geometry.")
  }
  out
}

.aoi_shape <- function(row) {
  has_shape <- "shape_type" %in% names(row) &&
    length(row[["shape_type"]]) &&
    !is.na(row[["shape_type"]]) &&
    nzchar(as.character(row[["shape_type"]]))
  polygon_value <- if ("polygon" %in% names(row)) row[["polygon"]] else NULL
  has_polygon <- !is.null(polygon_value) &&
    !(is.list(polygon_value) && length(polygon_value) == 1L && is.null(polygon_value[[1L]]))
  shape <- if (has_shape) {
    tolower(as.character(row[["shape_type"]]))
  } else if (has_polygon) {
    "polygon"
  } else {
    "rectangle"
  }
  if (!shape %in% c("rectangle", "polygon")) .aoi_stop("AOI shape_type must be rectangle or polygon.")
  shape
}

.aoi_point_in_polygon <- function(x, y, polygon) {
  n <- nrow(polygon); inside <- rep(FALSE, length(x)); j <- n
  for (i in seq_len(n)) {
    xi <- polygon[i, 1L]; yi <- polygon[i, 2L]; xj <- polygon[j, 1L]; yj <- polygon[j, 2L]
    hit <- ((yi > y) != (yj > y)) & (x < (xj - xi) * (y - yi) / ((yj - yi) + .Machine$double.eps) + xi)
    inside <- xor(inside, hit); j <- i
  }
  inside
}

.aoi_contains <- function(row, x, y) {
  if (identical(row$shape_type, "rectangle")) {
    x >= row$xmin & x <= row$xmax & y >= row$ymin & y <= row$ymax
  } else .aoi_point_in_polygon(x, y, .aoi_polygon(row$polygon[[1L]]))
}

.aoi_bounds <- function(row) {
  if (identical(row$shape_type, "rectangle")) c(row$xmin, row$xmax, row$ymin, row$ymax) else {
    p <- .aoi_polygon(row$polygon[[1L]])
    c(min(p[, 1L]), max(p[, 1L]), min(p[, 2L]), max(p[, 2L]))
  }
}

.aoi_pairwise_overlap <- function(geometry) {
  if (nrow(geometry) < 2L) return(data.frame(aoi_1 = character(), aoi_2 = character(), overlap = logical()))
  rows <- list()
  for (i in seq_len(nrow(geometry) - 1L)) for (j in seq.int(i + 1L, nrow(geometry))) {
    a <- geometry[i, , drop = FALSE]; b <- geometry[j, , drop = FALSE]
    if (a$shape_type == "rectangle" && b$shape_type == "rectangle") {
      flag <- min(a$xmax, b$xmax) - max(a$xmin, b$xmin) > 0 &&
        min(a$ymax, b$ymax) - max(a$ymin, b$ymin) > 0
    } else {
      ab <- .aoi_bounds(a); bb <- .aoi_bounds(b)
      x0 <- max(ab[1L], bb[1L]); x1 <- min(ab[2L], bb[2L]); y0 <- max(ab[3L], bb[3L]); y1 <- min(ab[4L], bb[4L])
      if (x1 <= x0 || y1 <= y0) flag <- FALSE else {
        grd <- expand.grid(x = seq(x0, x1, length.out = 21L), y = seq(y0, y1, length.out = 21L))
        flag <- any(.aoi_contains(a, grd$x, grd$y) & .aoi_contains(b, grd$x, grd$y))
      }
    }
    rows[[length(rows) + 1L]] <- data.frame(aoi_1 = a$aoi_id, aoi_2 = b$aoi_id, overlap = flag)
  }
  do.call(rbind, rows)
}

#' Validate AOI geometry
#' @param aois AOI data frame.
#' @param allow_overlap Whether overlapping AOIs are permitted.
#' @return An \`eye_aoi_geometry_validation\` object.
#' @export
validate_aoi_geometry <- function(aois, allow_overlap = TRUE) {
  if (!is.data.frame(aois) || !nrow(aois)) .aoi_stop("`aois` must be a non-empty data frame.")
  ids_found <- intersect(c("aoi_id", "aoi", "name", "label"), names(aois))
  if (!length(ids_found)) .aoi_stop("`aois` must contain an AOI identifier column.")
  ids <- as.character(aois[[ids_found[1L]]])
  if (any(is.na(ids) | !nzchar(ids)) || anyDuplicated(ids)) .aoi_stop("AOI identifiers must be unique, non-missing, and non-empty.")
  out <- aois; out$aoi_id <- ids
  if (!"shape_type" %in% names(out)) out$shape_type <- NA_character_
  if (!"polygon" %in% names(out)) out$polygon <- I(rep(list(NULL), nrow(out)))
  audit <- vector("list", nrow(out))
  for (i in seq_len(nrow(out))) {
    shape <- .aoi_shape(as.list(out[i, , drop = FALSE])); out$shape_type[i] <- shape
    if (shape == "rectangle") {
      required <- c("xmin", "xmax", "ymin", "ymax")
      if (!all(required %in% names(out))) .aoi_stop("Rectangular AOIs require xmin, xmax, ymin, and ymax.")
      vals <- vapply(required, function(nm) .aoi_num(out[[nm]][i], paste0(ids[i], ".", nm)), numeric(1))
      if (!(vals["xmin"] < vals["xmax"] && vals["ymin"] < vals["ymax"])) .aoi_stop("AOI `", ids[i], "` has zero or negative rectangle area.")
      for (nm in required) out[[nm]][i] <- vals[nm]
      area <- (vals["xmax"] - vals["xmin"]) * (vals["ymax"] - vals["ymin"])
    } else {
      p <- .aoi_polygon(out$polygon[[i]])
      if (.aoi_self_intersects(p)) .aoi_stop("AOI `", ids[i], "` polygon self-intersects.")
      area <- abs(.aoi_signed_area(p))
      if (area <= 1e-12) .aoi_stop("AOI `", ids[i], "` polygon has zero area.")
      out$polygon[[i]] <- p
    }
    audit[[i]] <- data.frame(aoi_id = ids[i], shape_type = shape, area = area, valid = TRUE)
  }
  overlap <- .aoi_pairwise_overlap(out); overlap_present <- nrow(overlap) > 0L && any(overlap$overlap)
  if (overlap_present && !isTRUE(allow_overlap)) {
    bad <- overlap[overlap$overlap, , drop = FALSE]
    .aoi_stop("AOIs overlap but `allow_overlap = FALSE`: ", paste(paste(bad$aoi_1, bad$aoi_2, sep = "/"), collapse = ", "))
  }
  version <- tryCatch(as.character(utils::packageVersion("eyeprocess")), error = function(e) NA_character_)
  .aoi_result("eye_aoi_geometry_validation", geometry = out, audit = do.call(rbind, audit),
              overlap = overlap, overlap_present = overlap_present, source_hash = .aoi_hash(out),
              software = list(package = "eyeprocess", version = version),
              status = if (overlap_present) "valid_with_overlap" else "valid")
}

.aoi_screen_geometry <- function(screen_width_px, screen_height_px, viewing_distance, physical_screen_size) {
  sw <- .aoi_num(screen_width_px, "screen_width_px", TRUE); sh <- .aoi_num(screen_height_px, "screen_height_px", TRUE)
  vd <- .aoi_num(viewing_distance, "viewing_distance", TRUE); ps <- .aoi_pair(physical_screen_size, "physical_screen_size", TRUE)
  if (min(sw, sh, vd, ps) <= 0) .aoi_stop("Screen dimensions and viewing distance must be positive.")
  c(x = atan((ps[1L] / sw) / (2 * vd)) * 360 / pi,
    y = atan((ps[2L] / sh) / (2 * vd)) * 360 / pi)
}

