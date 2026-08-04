.eye_env <- new.env(parent = emptyenv())
.eye_env$adapters <- list()
.eye_env$schema_version <- "0.1.0"
.eye_env$id_counters <- new.env(parent = emptyenv())

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

.eye_stop <- function(..., call. = FALSE) stop(..., call. = call.)
.eye_warn <- function(..., call. = FALSE) warning(..., call. = call.)
.eye_message <- function(..., quiet = FALSE) if (!isTRUE(quiet)) message(...)

.assert_scalar_character <- function(x, name, allow_na = FALSE) {
  ok <- is.character(x) && length(x) == 1L && (allow_na || !is.na(x))
  if (!ok) .eye_stop("`", name, "` must be a single character value.")
  invisible(x)
}

.assert_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .eye_stop("`", name, "` must be TRUE or FALSE.")
  }
  invisible(x)
}

.assert_data_frame <- function(x, name) {
  if (!is.data.frame(x)) .eye_stop("`", name, "` must be a data frame.")
  invisible(x)
}

.assert_eye_dataset <- function(x) {
  if (!inherits(x, "eye_dataset")) {
    .eye_stop("Expected an `eye_dataset` object.")
  }
  invisible(x)
}

.assert_columns <- function(data, columns, table_name = "data") {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    .eye_stop(
      "Missing required columns in `", table_name, "`: ",
      paste(missing, collapse = ", "), "."
    )
  }
  invisible(data)
}

.empty_df <- function(columns = character()) {
  out <- as.data.frame(
    setNames(replicate(length(columns), logical(0), simplify = FALSE), columns),
    stringsAsFactors = FALSE
  )
  out
}

.as_character_id <- function(x) {
  if (is.factor(x)) x <- as.character(x)
  as.character(x)
}

.safe_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x), fixed = TRUE)))
}

.safe_logical <- function(x) {
  if (is.logical(x)) return(x)
  z <- tolower(trimws(as.character(x)))
  out <- rep(NA, length(z))
  out[z %in% c("1", "true", "t", "yes", "y", "valid")] <- TRUE
  out[z %in% c("0", "false", "f", "no", "n", "invalid")] <- FALSE
  out
}

.unique_id <- function(prefix, n, start = 1L) {
  paste0(prefix, sprintf("%08d", seq.int(start, length.out = n)))
}

.next_id <- function(prefix, n = 1L) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 0L) .eye_stop("`n` must be a non-negative integer.")
  if (n == 0L) return(character())
  key <- gsub("[^A-Za-z0-9_]", "_", as.character(prefix))
  current <- .eye_env$id_counters[[key]] %||% 0L
  sequence_ids <- seq.int(current + 1L, length.out = n)
  .eye_env$id_counters[[key]] <- max(sequence_ids)
  stamp <- format(Sys.time(), "%Y%m%d%H%M%OS6", tz = "UTC")
  paste0(prefix, "_", stamp, "_", sprintf("%09d", sequence_ids))
}

.now_utc <- function() {
  format(Sys.time(), tz = "UTC", usetz = TRUE)
}

.file_md5 <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  out <- rep(NA_character_, length(path))
  ok <- file.exists(path) & !dir.exists(path)
  if (any(ok)) out[ok] <- unname(tools::md5sum(path[ok]))
  out
}

.deep_copy <- function(x) unserialize(serialize(x, NULL))

.bind_rows_base <- function(...) {
  xs <- list(...)
  xs <- xs[!vapply(xs, is.null, logical(1))]
  xs <- xs[vapply(xs, is.data.frame, logical(1))]
  if (!length(xs)) return(data.frame())

  all_names <- unique(unlist(lapply(xs, names), use.names = FALSE))
  prototypes <- setNames(lapply(all_names, function(nm) {
    idx <- which(vapply(xs, function(d) nm %in% names(d), logical(1)))[1L]
    xs[[idx]][[nm]][0]
  }), all_names)

  xs <- lapply(xs, function(d) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    missing <- setdiff(all_names, names(d))
    for (nm in missing) {
      # Preserve the column type and support zero-row canonical tables.
      d[[nm]] <- rep(prototypes[[nm]][NA_integer_], nrow(d))
    }
    d[all_names]
  })

  out <- do.call(rbind, xs)
  rownames(out) <- NULL
  out
}

.safe_max <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  max(x)
}

.safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  min(x)
}

.safe_span <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  max(x) - min(x)
}

.read_delimited <- function(path, delimiter = NULL, encoding = "UTF-8", ...) {
  if (!file.exists(path)) .eye_stop("File does not exist: ", path)
  if (is.null(delimiter)) {
    line <- readLines(path, n = 1L, warn = FALSE, encoding = encoding)
    candidates <- c("\t", ",", ";", "|")
    counts <- vapply(candidates, function(s) lengths(regmatches(line, gregexpr(s, line, fixed = TRUE))), numeric(1))
    delimiter <- candidates[which.max(counts)]
  }
  utils::read.table(
    path,
    header = TRUE,
    sep = delimiter,
    quote = '"',
    comment.char = "",
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = encoding,
    fill = TRUE,
    ...
  )
}

.first_existing <- function(names_vec, candidates, ignore_case = TRUE) {
  if (!length(names_vec)) return(NULL)
  if (ignore_case) {
    idx <- match(tolower(candidates), tolower(names_vec), nomatch = 0L)
  } else {
    idx <- match(candidates, names_vec, nomatch = 0L)
  }
  idx <- idx[idx > 0L]
  if (!length(idx)) return(NULL)
  names_vec[idx[1L]]
}

.first_nonmissing <- function(x, default = NA) {
  idx <- which(!is.na(x) & nzchar(as.character(x)))
  if (!length(idx)) default else x[idx[1L]]
}

.group_split <- function(data, keys) {
  if (!nrow(data)) return(list())
  .assert_columns(data, keys)
  interaction_key <- interaction(data[keys], drop = TRUE, lex.order = TRUE)
  split(data, interaction_key, drop = TRUE)
}

.aggregate_rows <- function(data, by, funs) {
  if (!nrow(data)) return(data.frame())
  groups <- .group_split(data, by)
  out <- lapply(groups, function(d) {
    base <- d[1L, by, drop = FALSE]
    vals <- lapply(funs, function(f) f(d))
    cbind(base, as.data.frame(vals, stringsAsFactors = FALSE), stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

.require_namespace <- function(pkg, reason = NULL) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    msg <- paste0("Package `", pkg, "` is required")
    if (!is.null(reason)) msg <- paste0(msg, " ", reason)
    .eye_stop(msg, ". Install it with install.packages(\"", pkg, "\").")
  }
  invisible(TRUE)
}

.valid_choice <- function(x, choices, name = deparse(substitute(x))) {
  if (length(x) != 1L || is.na(x) || !x %in% choices) {
    .eye_stop("`", name, "` must be one of: ", paste(choices, collapse = ", "), ".")
  }
  x
}

.quantile_safe <- function(x, probs, na.rm = TRUE) {
  x <- x[is.finite(x)]
  if (!length(x)) return(rep(NA_real_, length(probs)))
  as.numeric(stats::quantile(x, probs = probs, na.rm = na.rm, names = FALSE, type = 7))
}

.trapz <- function(x, y) {
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  if (length(x) < 2L) return(NA_real_)
  ord <- order(x)
  x <- x[ord]
  y <- y[ord]
  sum(diff(x) * (head(y, -1L) + tail(y, -1L)) / 2)
}

.mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

.copy_attrs <- function(from, to, exclude = c("names", "row.names", "class")) {
  at <- attributes(from)
  at[exclude] <- NULL
  for (nm in names(at)) attr(to, nm) <- at[[nm]]
  to
}
