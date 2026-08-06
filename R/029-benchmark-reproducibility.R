# Public benchmark and reproducibility assets ---------------------------------

.br_stop <- function(message, class = "eyeprocess_benchmark_error") {
  condition <- structure(list(message = message, call = NULL), class = c(class, "error", "condition"))
  stop(condition)
}

.br_root <- function(path = NULL) {
  if (!is.null(path)) return(normalizePath(path, winslash = "/", mustWork = TRUE))
  installed <- system.file("extdata", "benchmark-study", package = "eyeprocess")
  if (nzchar(installed)) return(installed)
  candidate <- file.path("inst", "extdata", "benchmark-study")
  if (dir.exists(candidate)) return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
  .br_stop("The bundled benchmark study was not found.")
}

#' Locate the bundled public benchmark study
#'
#' @param path Optional alternative benchmark root.
#' @return An `eye_benchmark_study` object.
#' @export
eyeprocess_benchmark_study <- function(path = NULL) {
  root <- .br_root(path)
  manifest_path <- file.path(root, "manifest.csv")
  if (!file.exists(manifest_path)) .br_stop("Benchmark manifest is missing.")
  manifest <- utils::read.csv(manifest_path, stringsAsFactors = FALSE)
  out <- list(path = root, manifest = manifest, licence = if (file.exists(file.path(root, "LICENSE.txt"))) paste(readLines(file.path(root, "LICENSE.txt"), warn = FALSE), collapse = "\n") else NA_character_)
  class(out) <- "eye_benchmark_study"
  out
}

#' @export
print.eye_benchmark_study <- function(x, ...) {
  cat("eyeprocess public benchmark study\n")
  cat("Path:   ", x$path, "\n", sep = "")
  cat("Files:  ", nrow(x$manifest), "\n", sep = "")
  cat("Status: synthetic, openly redistributable benchmark - not empirical vendor validation\n")
  invisible(x)
}

# Normalize case-insensitive logical tokens produced by external CSV writers.
#
# Python, Julia, and several data-export tools commonly serialize logical values
# as `True` and `False`. Base R's CSV type conversion recognizes `TRUE` and
# `FALSE`, but may retain title-case tokens as character data. Benchmark tables
# use explicit logical contracts, so normalize columns containing only these
# tokens while leaving identifiers and arbitrary text untouched.
.br_normalize_csv_types <- function(data) {
  for (name in names(data)) {
    column <- data[[name]]
    if (!is.character(column)) next

    token <- trimws(column)
    missing <- is.na(token) | !nzchar(token)
    observed <- unique(tolower(token[!missing]))

    if (length(observed) && all(observed %in% c("true", "false"))) {
      value <- rep(NA, length(token))
      value[!missing] <- tolower(token[!missing]) == "true"
      data[[name]] <- value
    }
  }
  data
}

#' Read a benchmark table
#'
#' @param study Benchmark object or path.
#' @param table Table name.
#' @return Data frame.
#' @export
read_benchmark_table <- function(study = eyeprocess_benchmark_study(), table) {
  if (is.character(study)) study <- eyeprocess_benchmark_study(study)
  if (!inherits(study, "eye_benchmark_study")) .br_stop("Expected an eye benchmark study.")
  row <- study$manifest[study$manifest$table == table, , drop = FALSE]
  if (!nrow(row)) .br_stop(sprintf("Benchmark table `%s` is absent.", table))
  path <- file.path(study$path, row$file[1L])
  data <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  .br_normalize_csv_types(data)
}

#' Return expected benchmark outputs
#'
#' @param study Benchmark object or path.
#' @return Expected-output data frame.
#' @export
benchmark_expected_outputs <- function(study = eyeprocess_benchmark_study()) {
  if (is.character(study)) study <- eyeprocess_benchmark_study(study)
  path <- file.path(study$path, "expected_outputs.csv")
  if (!file.exists(path)) .br_stop("Expected benchmark outputs are missing.")
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Build an eye dataset from the public benchmark
#'
#' @param study Benchmark object or path.
#' @return An `eye_dataset` where package constructors are available, otherwise a named list.
#' @export
import_benchmark_study <- function(study = eyeprocess_benchmark_study()) {
  if (is.character(study)) study <- eyeprocess_benchmark_study(study)
  tables <- setNames(lapply(study$manifest$table, function(table) read_benchmark_table(study, table)), study$manifest$table)
  if (exists("new_eye_dataset", mode = "function")) {
    accepted <- intersect(names(formals(new_eye_dataset)), names(tables))
    args <- tables[accepted]
    result <- tryCatch(do.call(new_eye_dataset, args), error = function(e) NULL)
    if (!is.null(result)) return(result)
  }
  class(tables) <- c("eye_benchmark_tables", "list")
  tables
}

#' Validate benchmark integrity and relational constraints
#'
#' @param study Benchmark object or path.
#' @param verify_hashes Whether to verify MD5 hashes.
#' @return An `eye_benchmark_validation` object.
#' @export
validate_benchmark_study <- function(study = eyeprocess_benchmark_study(), verify_hashes = TRUE) {
  if (is.character(study)) study <- eyeprocess_benchmark_study(study)
  manifest <- study$manifest
  paths <- file.path(study$path, manifest$file)
  exists <- file.exists(paths)
  bytes <- ifelse(exists, file.info(paths)$size, NA_real_)
  hash <- rep(NA_character_, length(paths))
  if (verify_hashes && any(exists)) hash[exists] <- unname(tools::md5sum(paths[exists]))
  files <- data.frame(
    table = manifest$table, file = manifest$file, exists = exists,
    bytes_match = exists & bytes == manifest$bytes,
    hash_match = if (verify_hashes) exists & hash == manifest$md5 else NA,
    stringsAsFactors = FALSE
  )
  tables <- if (all(exists)) setNames(lapply(manifest$table, function(table) read_benchmark_table(study, table)), manifest$table) else list()
  relations <- data.frame(check = character(), passed = logical(), detail = character(), stringsAsFactors = FALSE)
  add <- function(check, passed, detail = "") relations <<- rbind(relations, data.frame(check = check, passed = passed, detail = detail, stringsAsFactors = FALSE))
  if (length(tables)) {
    participants <- unique(tables$participants$participant_id)
    items <- unique(tables$items$item_id)
    add("responses_participants", all(tables$responses$participant_id %in% participants))
    add("responses_items", all(tables$responses$item_id %in% items))
    add("gaze_trials", all(tables$gaze_samples$trial_id %in% tables$responses$trial_id))
    add("pupil_trials", all(tables$pupil_samples$trial_id %in% tables$responses$trial_id))
    add("event_trials", all(tables$events$trial_id %in% tables$responses$trial_id))
    add("unique_response_key", !anyDuplicated(tables$responses[c("participant_id", "item_id", "trial_id")]))
    add("positive_response_time", all(tables$responses$response_time > 0))
    add("bounded_gaze", all(tables$gaze_samples$x >= 0 & tables$gaze_samples$x <= 1 & tables$gaze_samples$y >= 0 & tables$gaze_samples$y <= 1))
  }
  valid <- all(files$exists & files$bytes_match & (is.na(files$hash_match) | files$hash_match)) && all(relations$passed)
  out <- list(valid = valid, files = files, relations = relations, benchmark_version = "1.0.0")
  class(out) <- "eye_benchmark_validation"
  out
}

#' @export
print.eye_benchmark_validation <- function(x, ...) {
  cat("Benchmark validation\n")
  cat("Valid: ", x$valid, "\n", sep = "")
  print(x$files, row.names = FALSE)
  if (nrow(x$relations)) print(x$relations, row.names = FALSE)
  invisible(x)
}

#' Derive reproducible benchmark summaries
#'
#' @param study Benchmark object or path.
#' @return Named scalar outputs.
#' @export
run_benchmark_reproduction <- function(study = eyeprocess_benchmark_study()) {
  if (is.character(study)) study <- eyeprocess_benchmark_study(study)
  responses <- read_benchmark_table(study, "responses")
  gaze <- read_benchmark_table(study, "gaze_samples")
  pupil <- read_benchmark_table(study, "pupil_samples")
  quality <- read_benchmark_table(study, "quality")
  aoi <- read_benchmark_table(study, "aoi_definitions")
  if (!is.logical(gaze$valid)) {
    .br_stop("Benchmark `gaze_samples$valid` must be logical after import.")
  }
  if (anyNA(gaze$valid)) {
    .br_stop("Benchmark `gaze_samples$valid` contains missing values.")
  }

  outputs <- c(
    participants = length(unique(responses$participant_id)),
    items = length(unique(responses$item_id)),
    trials = nrow(responses),
    accuracy = mean(responses$score),
    mean_response_time = mean(responses$response_time),
    gaze_samples = nrow(gaze),
    pupil_samples = nrow(pupil),
    valid_gaze_fraction = mean(gaze$valid),
    mean_pupil = mean(pupil$pupil, na.rm = TRUE),
    quality_rows = nrow(quality),
    aoi_count = nrow(aoi)
  )
  expected <- benchmark_expected_outputs(study)
  comparison <- merge(
    data.frame(metric = names(outputs), observed = as.numeric(outputs), stringsAsFactors = FALSE),
    expected, by = "metric", all.x = TRUE
  )
  if (anyNA(comparison$expected) || anyNA(comparison$tolerance)) {
    missing <- comparison$metric[is.na(comparison$expected) | is.na(comparison$tolerance)]
    .br_stop(sprintf(
      "Expected benchmark values or tolerances are missing for: %s.",
      paste(missing, collapse = ", ")
    ))
  }
  comparison$absolute_error <- abs(comparison$observed - comparison$expected)
  comparison$passed <- !is.na(comparison$absolute_error) &
    comparison$absolute_error <= comparison$tolerance
  out <- list(
    outputs = outputs,
    comparison = comparison,
    passed = isTRUE(all(comparison$passed)),
    study = study
  )
  class(out) <- "eye_benchmark_reproduction"
  out
}

#' @export
print.eye_benchmark_reproduction <- function(x, ...) {
  cat("Benchmark reproduction\n")
  cat("Passed: ", x$passed, "\n", sep = "")
  print(x$comparison, row.names = FALSE)
  invisible(x)
}

#' Write the benchmark data dictionary
#'
#' @param study Benchmark object or path.
#' @param path Output Markdown file.
#' @return Output path.
#' @export
write_benchmark_data_dictionary <- function(study = eyeprocess_benchmark_study(), path = "benchmark-data-dictionary.md") {
  if (is.character(study)) study <- eyeprocess_benchmark_study(study)
  lines <- c("# eyeprocess benchmark data dictionary", "", "This benchmark is synthetic and openly redistributable. It is not evidence of compatibility with any commercial eye-tracker export.", "")
  for (table in study$manifest$table) {
    data <- read_benchmark_table(study, table)
    lines <- c(lines, paste0("## `", table, "`"), "", paste0("Rows: ", nrow(data), "; columns: ", ncol(data), "."), "", "| Column | R class | Missing |", "|---|---|---:|")
    rows <- vapply(names(data), function(name) sprintf("| `%s` | %s | %d |", name, paste(class(data[[name]]), collapse = "/"), sum(is.na(data[[name]]))), character(1))
    lines <- c(lines, rows, "")
  }
  writeLines(lines, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

#' Create a reproducibility manifest for files and software
#'
#' @param paths Files/directories.
#' @param include_session Whether to capture session information.
#' @return An `eye_reproducibility_manifest` object.
#' @export
package_reproducibility_manifest <- function(paths, include_session = TRUE) {
  paths <- unique(paths)
  files <- unlist(lapply(paths, function(path) if (dir.exists(path)) list.files(path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE) else path), use.names = FALSE)
  files <- files[file.exists(files) & !dir.exists(files)]
  manifest <- data.frame(path = normalizePath(files, winslash = "/", mustWork = TRUE), bytes = file.info(files)$size, md5 = unname(tools::md5sum(files)), stringsAsFactors = FALSE)
  out <- list(files = manifest, created_utc = format(Sys.time(), tz = "UTC", usetz = TRUE), R = R.version.string, platform = R.version$platform, session = if (include_session) utils::capture.output(utils::sessionInfo()) else character())
  class(out) <- "eye_reproducibility_manifest"
  out
}

#' Verify a reproducibility manifest
#'
#' @param manifest Manifest.
#' @return Verification data frame.
#' @export
verify_reproducibility_manifest <- function(manifest) {
  if (!inherits(manifest, "eye_reproducibility_manifest")) .br_stop("Expected a reproducibility manifest.")
  files <- manifest$files
  exists <- file.exists(files$path)
  current <- rep(NA_character_, nrow(files)); current[exists] <- unname(tools::md5sum(files$path[exists]))
  data.frame(path = files$path, exists = exists, expected_md5 = files$md5, current_md5 = current, unchanged = exists & current == files$md5, stringsAsFactors = FALSE)
}

#' Write a complete software-paper reproduction scaffold
#'
#' @param directory Output directory.
#' @param study Benchmark study.
#' @param overwrite Whether to replace existing files.
#' @return Output manifest.
#' @export
write_software_paper_reproduction <- function(directory, study = eyeprocess_benchmark_study(), overwrite = FALSE) {
  directory <- normalizePath(directory, winslash = "/", mustWork = FALSE)
  if (dir.exists(directory) && length(list.files(directory)) && !overwrite) .br_stop("Output directory is not empty; use `overwrite = TRUE`.")
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  data_dir <- file.path(directory, "data"); result_dir <- file.path(directory, "results"); script_dir <- file.path(directory, "scripts")
  dir.create(data_dir, recursive = TRUE); dir.create(result_dir); dir.create(script_dir)
  file.copy(list.files(study$path, full.names = TRUE), data_dir, recursive = TRUE, overwrite = TRUE)
  script <- c(
    "library(eyeprocess)",
    "study <- eyeprocess_benchmark_study('data')",
    "validation <- validate_benchmark_study(study)",
    "stopifnot(validation$valid)",
    "result <- run_benchmark_reproduction(study)",
    "stopifnot(result$passed)",
    "write.csv(result$comparison, 'results/reproduction-comparison.csv', row.names = FALSE)",
    "saveRDS(package_reproducibility_manifest(c('data', 'scripts')), 'results/reproducibility-manifest.rds')"
  )
  writeLines(script, file.path(script_dir, "run-reproduction.R"), useBytes = TRUE)
  readme <- c(
    "# eyeprocess software-paper reproduction", "",
    "This directory contains the synthetic public benchmark, an executable reproduction script, expected outputs, and file fingerprints.", "",
    "Run from this directory with:", "", "```r", "source('scripts/run-reproduction.R')", "```", "",
    "The benchmark demonstrates reproducibility and data-contract behavior; it does not establish empirical vendor compatibility or scientific validity of experimental model families."
  )
  writeLines(readme, file.path(directory, "README.md"), useBytes = TRUE)
  package_reproducibility_manifest(directory)
}

#' Audit whether benchmark assets are ready for public release
#'
#' @param study Benchmark study.
#' @return Release-audit findings.
#' @export
audit_benchmark_release <- function(study = eyeprocess_benchmark_study()) {
  validation <- validate_benchmark_study(study)
  reproduction <- if (validation$valid) run_benchmark_reproduction(study) else NULL
  required <- c("participants", "items", "responses", "gaze_samples", "events", "aoi_definitions", "pupil_samples", "quality", "provenance")
  findings <- data.frame(
    check = c("integrity", "reproduction", "required_tables", "open_licence", "synthetic_label"),
    passed = c(validation$valid, !is.null(reproduction) && reproduction$passed, all(required %in% study$manifest$table), file.exists(file.path(study$path, "LICENSE.txt")), file.exists(file.path(study$path, "SYNTHETIC_DATA.txt"))),
    stringsAsFactors = FALSE
  )
  list(ready = all(findings$passed), findings = findings, validation = validation, reproduction = reproduction)
}
