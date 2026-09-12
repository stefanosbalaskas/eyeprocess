.null_coalesce <- function(x, y) if (is.null(x)) y else x

.eye_stop <- function(...) stop(..., call. = FALSE)
.eye_warn <- function(...) warning(..., call. = FALSE)

.as_character_scalar <- function(x, name) {
  if (length(x) != 1L || is.na(x)) .eye_stop("`", name, "` must be a single non-missing value.")
  as.character(x)
}

.assert_data_frame <- function(x, name = deparse(substitute(x))) {
  if (!is.data.frame(x)) .eye_stop("`", name, "` must be a data.frame.")
  invisible(TRUE)
}

.assert_columns <- function(data, columns, name = deparse(substitute(data))) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    .eye_stop("Missing required columns in `", name, "`: ", paste(missing, collapse = ", "), ".")
  }
  invisible(TRUE)
}

.numeric_or_na <- function(x) {
  suppressWarnings(as.numeric(as.character(x)))
}

.clean_names <- function(x) {
  x <- trimws(as.character(x))
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  tolower(x)
}

.unique_names <- function(x) make.unique(x, sep = "_")

.normalize_names <- function(data) {
  names(data) <- .unique_names(.clean_names(names(data)))
  data
}

.read_delimited <- function(path, sep = NULL, ...) {
  if (is.null(sep)) {
    ext <- tolower(tools::file_ext(path))
    sep <- if (ext %in% c("tsv", "txt")) "\t" else ","
  }
  utils::read.table(path, header = TRUE, sep = sep, quote = "\"", comment.char = "", check.names = FALSE,
                    stringsAsFactors = FALSE, fill = TRUE, ...)
}

.write_delimited <- function(data, path, sep = ",", ...) {
  utils::write.table(data, path, sep = sep, row.names = FALSE, col.names = TRUE, quote = TRUE, ...)
  invisible(path)
}

.as_numeric_matrix <- function(x, columns) {
  out <- vapply(columns, function(col) .numeric_or_na(x[[col]]), numeric(nrow(x)))
  if (!is.matrix(out)) out <- matrix(out, nrow = nrow(x), dimnames = list(NULL, columns))
  out
}

.first_existing <- function(names_vec, candidates) {
  idx <- match(candidates, names_vec, nomatch = 0L)
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
    .eye_stop(msg, ". Please install it before using this feature.")
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

.safe_cor <- function(x, y, method = "pearson") {
  ok <- is.finite(x) & is.finite(y)
  if (sum(ok) < 3L) return(NA_real_)
  suppressWarnings(stats::cor(x[ok], y[ok], method = method))
}

.hash_text <- function(x) {
  raw <- charToRaw(paste(x, collapse = "\n"))
  if (requireNamespace("openssl", quietly = TRUE)) return(as.character(openssl::sha256(raw)))
  ints <- as.integer(raw)
  if (!length(ints)) return("00000000")
  h <- 0
  for (value in ints) h <- (h * 131 + value) %% 2147483647
  sprintf("%08x", as.integer(h))
}

.hash_object <- function(x) {
  raw <- serialize(x, NULL, version = 2)
  if (requireNamespace("openssl", quietly = TRUE)) return(as.character(openssl::sha256(raw)))
  ints <- as.integer(raw)
  h <- 0
  for (value in ints) h <- (h * 131 + value) %% 2147483647
  sprintf("%08x", as.integer(h))
}

# Scope a reproducible random-number seed to the current function call without
# package code reading from, assigning to, or removing objects in .GlobalEnv.
# `withr::local_seed()` restores the caller's RNG state automatically on exit.
.eye_local_seed <- function(seed, env = parent.frame()) {
  if (!is.null(seed)) withr::local_seed(seed, .local_envir = env)
  invisible(seed)
}
