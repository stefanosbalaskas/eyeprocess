# Independent multi-vendor validation corpus ---------------------------------

.vc_stop <- function(...) {
  if (exists(".eye_stop", mode = "function", inherits = TRUE)) .eye_stop(...) else stop(paste0(...), call. = FALSE)
}

.vc_or <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

.vc_vendor <- function(x) {
  value <- as.character(x)[1L]
  if (length(value) != 1L || is.na(value) || !nzchar(trimws(value))) .vc_stop("Vendor must be non-empty.")
  value <- gsub("[^a-z0-9]+", "", tolower(value))
  aliases <- c(
    gazepointanalysis = "gazepoint", gazepointbiometrics = "gazepoint",
    tobiiprolab = "tobii", pupillabs = "pupillabs", pupilneon = "pupillabs",
    pupilcore = "pupillabs", srresearch = "eyelink", dataviewer = "eyelink",
    smibegaze = "smi"
  )
  if (value %in% names(aliases)) value <- aliases[[value]]
  if (!nzchar(value)) .vc_stop("Vendor must be non-empty.")
  value
}

.vc_case_id <- function(vendor, device_model, software_version, source_path) {
  key <- paste(vendor, device_model, software_version, normalizePath(source_path, winslash = "/", mustWork = FALSE), sep = "|")
  paste0(vendor, "-", sprintf("%010d", .ve_hash_int(key)))
}

.vc_registry_path <- function(corpus_path) file.path(corpus_path, "vendor-cases.csv")
.vc_semantics_path <- function(corpus_path) file.path(corpus_path, "vendor-semantics.csv")

.vc_registry_columns <- function() c(
  "case_id", "vendor", "support_level", "device_model", "hardware_version",
  "software_name", "software_version", "export_profile", "sampling_rate_hz",
  "coordinate_system", "timebase", "event_semantics", "ocular_structure",
  "missingness_convention", "vendor_fixations", "package_transformations",
  "unsupported_fields", "independent_source", "licence_reviewed",
  "redistribution_allowed", "source_path", "registered_utc", "status", "notes"
)

.vc_empty_registry <- function() {
  out <- as.data.frame(setNames(replicate(length(.vc_registry_columns()), character(), simplify = FALSE), .vc_registry_columns()), stringsAsFactors = FALSE)
  out$sampling_rate_hz <- numeric()
  out$independent_source <- logical()
  out$licence_reviewed <- logical()
  out$redistribution_allowed <- logical()
  out
}

#' Create or validate a multi-vendor corpus directory
#'
#' @param path Corpus directory.
#' @param overwrite Reinitialize an existing empty corpus.
#' @return Normalized corpus path.
#' @export
init_vendor_corpus <- function(path, overwrite = FALSE) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (dir.exists(path) && !isTRUE(overwrite) && file.exists(.vc_registry_path(path))) return(path)
  if (dir.exists(path) && isTRUE(overwrite)) unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "cases"), showWarnings = FALSE)
  dir.create(file.path(path, "fingerprints"), showWarnings = FALSE)
  dir.create(file.path(path, "reports"), showWarnings = FALSE)
  dir.create(file.path(path, "redacted"), showWarnings = FALSE)
  .ve_atomic_write_csv(.vc_empty_registry(), .vc_registry_path(path))
  semantics <- data.frame(
    vendor = character(), native_field = character(), native_meaning = character(),
    canonical_table = character(), canonical_field = character(), unit = character(),
    transformation = character(), loss_risk = character(), evidence_case_id = character(),
    stringsAsFactors = FALSE
  )
  .ve_atomic_write_csv(semantics, .vc_semantics_path(path))
  writeLines(c(
    "# eyeprocess independent multi-vendor validation corpus", "",
    "This directory records declared, fixture-tested, and empirically validated support separately.",
    "No real-export case is classified as empirically validated without independent source evidence, version/device metadata, completed licensing review, and successful validation artifacts."
  ), file.path(path, "README.md"), useBytes = TRUE)
  path
}

#' Read the multi-vendor case registry
#'
#' @param corpus_path Corpus directory.
#' @return Case registry data frame.
#' @export
read_vendor_registry <- function(corpus_path) {
  corpus_path <- normalizePath(corpus_path, winslash = "/", mustWork = TRUE)
  path <- .vc_registry_path(corpus_path)
  if (!file.exists(path)) .vc_stop("Vendor registry is missing: ", path)
  out <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  missing <- setdiff(.vc_registry_columns(), names(out))
  for (nm in missing) out[[nm]] <- NA
  out[.vc_registry_columns()]
}

#' Write the multi-vendor case registry
#'
#' @param x Registry data frame.
#' @param corpus_path Corpus directory.
#' @return Registry path.
#' @export
write_vendor_registry <- function(x, corpus_path) {
  if (!is.data.frame(x)) .vc_stop("Registry must be a data frame.")
  missing <- setdiff(.vc_registry_columns(), names(x))
  for (nm in missing) x[[nm]] <- NA
  x <- x[.vc_registry_columns()]
  .ve_atomic_write_csv(x, .vc_registry_path(normalizePath(corpus_path, winslash = "/", mustWork = TRUE)))
}

#' Fingerprint every file in a validation case
#'
#' @param path File or directory.
#' @param algorithms Hash algorithms. Base R always supplies MD5; SHA-256 is
#'   added when `openssl` is installed.
#' @param include_hidden Include hidden files.
#' @return An `eye_validation_case_fingerprint` data frame.
#' @export
fingerprint_validation_case <- function(path, algorithms = c("md5", "sha256"), include_hidden = FALSE) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  files <- if (dir.exists(path)) list.files(path, recursive = TRUE, full.names = TRUE, all.files = include_hidden, no.. = TRUE) else path
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!length(files)) .vc_stop("No files were found in the validation case.")
  info <- file.info(files)
  relative <- if (dir.exists(path)) substring(normalizePath(files, winslash = "/"), nchar(path) + 2L) else basename(files)
  out <- data.frame(
    relative_path = relative,
    extension = tolower(tools::file_ext(files)),
    bytes = info$size,
    modified_utc = format(info$mtime, tz = "UTC", usetz = TRUE),
    md5 = unname(tools::md5sum(files)),
    stringsAsFactors = FALSE
  )
  if ("sha256" %in% algorithms && requireNamespace("openssl", quietly = TRUE)) {
    out$sha256 <- vapply(files, function(path) {
      connection <- base::file(path, "rb")
      on.exit(close(connection), add = TRUE)
      paste(format(openssl::sha256(connection)), collapse = "")
    }, character(1))
  }
  out$case_fingerprint <- sprintf("case-%010d", .ve_hash_int(paste(out$relative_path, out$bytes, out$md5, collapse = "|")))
  class(out) <- c("eye_validation_case_fingerprint", "data.frame")
  out
}

#' @export
print.eye_validation_case_fingerprint <- function(x, ...) {
  cat("Validation-case fingerprint\n")
  cat("Files:       ", nrow(x), "\n", sep = "")
  cat("Bytes:       ", sum(x$bytes), "\n", sep = "")
  cat("Fingerprint: ", unique(x$case_fingerprint)[1L], "\n", sep = "")
  invisible(x)
}

.vc_copy_case <- function(source, target, mode) {
  if (mode == "reference") return(normalizePath(source, winslash = "/", mustWork = TRUE))
  if (file.exists(target) || dir.exists(target)) .vc_stop("Case target already exists: ", target)
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(source)) {
    dir.create(target, recursive = TRUE, showWarnings = FALSE)
    entries <- list.files(source, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE, include.dirs = TRUE)
    relative <- substring(normalizePath(entries, winslash = "/", mustWork = TRUE), nchar(normalizePath(source, winslash = "/", mustWork = TRUE)) + 2L)
    directories <- entries[dir.exists(entries)]
    if (length(directories)) {
      directory_relative <- relative[dir.exists(entries)]
      invisible(vapply(file.path(target, directory_relative), dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
    }
    files <- entries[!dir.exists(entries)]
    if (length(files)) {
      file_relative <- relative[!dir.exists(entries)]
      destinations <- file.path(target, file_relative)
      invisible(vapply(dirname(destinations), dir.create, logical(1), recursive = TRUE, showWarnings = FALSE))
      ok <- file.copy(files, destinations, recursive = FALSE, copy.mode = TRUE, copy.date = TRUE)
      if (!all(ok)) .vc_stop("Could not copy the complete validation case.")
    }
  } else {
    ok <- file.copy(source, target, recursive = FALSE, copy.mode = TRUE, copy.date = TRUE)
    if (!isTRUE(ok)) .vc_stop("Could not copy the validation case file.")
  }
  normalizePath(target, winslash = "/", mustWork = TRUE)
}

#' Register an independent validation case
#'
#' @param corpus_path Corpus directory.
#' @param source_path Real export file or directory.
#' @param vendor Vendor name.
#' @param device_model Hardware model.
#' @param software_name,software_version Export software and version.
#' @param hardware_version Optional hardware/firmware version.
#' @param export_profile Export options/profile.
#' @param sampling_rate_hz Nominal or observed rate.
#' @param coordinate_system,timebase,event_semantics Semantics metadata.
#' @param ocular_structure Monocular/binocular structure.
#' @param missingness_convention Vendor missing-value convention.
#' @param vendor_fixations Description of vendor-derived fixation fields.
#' @param package_transformations Declared package transformations.
#' @param unsupported_fields Known unsupported fields.
#' @param independent_source Whether independently obtained.
#' @param licence_reviewed Whether licensing review is complete.
#' @param redistribution_allowed Whether redacted material may be redistributed.
#' @param support_level Declared support level.
#' @param mode Reference source in place or copy it into the private corpus.
#' @param case_id Optional explicit identifier.
#' @param notes Notes.
#' @return Registered case row.
#' @export
register_validation_case <- function(
    corpus_path,
    source_path,
    vendor,
    device_model,
    software_name,
    software_version,
    hardware_version = NA_character_,
    export_profile = NA_character_,
    sampling_rate_hz = NA_real_,
    coordinate_system = NA_character_,
    timebase = NA_character_,
    event_semantics = NA_character_,
    ocular_structure = NA_character_,
    missingness_convention = NA_character_,
    vendor_fixations = NA_character_,
    package_transformations = NA_character_,
    unsupported_fields = NA_character_,
    independent_source = TRUE,
    licence_reviewed = FALSE,
    redistribution_allowed = FALSE,
    support_level = c("declared", "fixture-tested", "empirically-validated"),
    mode = c("reference", "copy"),
    case_id = NULL,
    notes = NA_character_) {
  corpus_path <- init_vendor_corpus(corpus_path)
  source_path <- normalizePath(source_path, winslash = "/", mustWork = TRUE)
  vendor <- .vc_vendor(vendor)
  support_level <- match.arg(support_level)
  mode <- match.arg(mode)
  required_text <- c(device_model = device_model, software_name = software_name, software_version = software_version)
  if (anyNA(required_text) || any(!nzchar(trimws(as.character(required_text))))) .vc_stop("Device model, software name, and software version are required.")
  sampling_rate_hz <- suppressWarnings(as.numeric(sampling_rate_hz))
  if (length(sampling_rate_hz) != 1L || (!is.na(sampling_rate_hz) && (!is.finite(sampling_rate_hz) || sampling_rate_hz <= 0))) .vc_stop("`sampling_rate_hz` must be missing or a positive finite value.")
  if (support_level == "empirically-validated" && (!isTRUE(independent_source) || !isTRUE(licence_reviewed))) {
    .vc_stop("Empirically validated cases require independent-source and licence-review evidence.")
  }
  if (is.null(case_id)) case_id <- .vc_case_id(vendor, device_model, software_version, source_path)
  case_id <- .ve_safe_name(case_id, paste0(vendor, "-case"))
  registry <- read_vendor_registry(corpus_path)
  if (case_id %in% registry$case_id) .vc_stop("Case identifier already exists: ", case_id)
  stored_path <- if (mode == "copy") .vc_copy_case(source_path, file.path(corpus_path, "cases", case_id), mode) else source_path
  fingerprint <- fingerprint_validation_case(stored_path)
  .ve_atomic_write_csv(fingerprint, file.path(corpus_path, "fingerprints", paste0(case_id, ".csv")))
  row <- data.frame(
    case_id = case_id,
    vendor = vendor,
    support_level = support_level,
    device_model = as.character(device_model),
    hardware_version = as.character(hardware_version),
    software_name = as.character(software_name),
    software_version = as.character(software_version),
    export_profile = as.character(export_profile),
    sampling_rate_hz = as.numeric(sampling_rate_hz),
    coordinate_system = as.character(coordinate_system),
    timebase = as.character(timebase),
    event_semantics = as.character(event_semantics),
    ocular_structure = as.character(ocular_structure),
    missingness_convention = as.character(missingness_convention),
    vendor_fixations = as.character(vendor_fixations),
    package_transformations = as.character(package_transformations),
    unsupported_fields = as.character(unsupported_fields),
    independent_source = isTRUE(independent_source),
    licence_reviewed = isTRUE(licence_reviewed),
    redistribution_allowed = isTRUE(redistribution_allowed),
    source_path = stored_path,
    registered_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    status = "registered",
    notes = as.character(notes),
    stringsAsFactors = FALSE
  )
  write_vendor_registry(rbind(registry, row), corpus_path)
  class(row) <- c("eye_validation_case", "eye_vendor_case", "data.frame")
  row
}

#' @export
print.eye_vendor_case <- function(x, ...) {
  cat("Vendor validation case\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

.vc_hash_ids <- function(x, salt) {
  x <- as.character(x)
  vapply(x, function(value) paste0("ID", sprintf("%010d", .ve_hash_int(paste(salt, value, sep = "|")))), character(1))
}

.vc_read_delimited <- function(path) {
  first <- readLines(path, n = 3L, warn = FALSE)
  delimiter <- if (sum(lengths(regmatches(first, gregexpr("\t", first)))) > sum(lengths(regmatches(first, gregexpr(",", first))))) "\t" else ","
  utils::read.table(path, header = TRUE, sep = delimiter, quote = "\"", comment.char = "", stringsAsFactors = FALSE, check.names = FALSE, fill = TRUE)
}

.vc_write_delimited <- function(x, path, delimiter) {
  utils::write.table(x, path, sep = delimiter, row.names = FALSE, quote = TRUE, na = "", fileEncoding = "UTF-8")
}

#' Redact a validation case without inventing replacement data
#'
#' Tabular identifiers are deterministically pseudonymized. Columns explicitly
#' listed for removal are dropped. Unsupported binary/media files are excluded
#' unless `copy_non_tabular` is explicitly enabled.
#'
#' @param source_path Source file/directory.
#' @param output_path Redacted output directory.
#' @param id_columns Identifier columns to pseudonymize.
#' @param remove_columns Columns to remove.
#' @param text_redactor Optional function applied to character columns.
#' @param salt Required project-specific salt.
#' @param copy_non_tabular Copy unsupported files unchanged.
#' @param overwrite Replace output.
#' @return An `eye_redaction_result`.
#' @export
redact_validation_case <- function(
    source_path,
    output_path,
    id_columns = c("participant_id", "subject", "participant", "recording_id", "session_id"),
    remove_columns = c("name", "email", "address", "birthdate", "date_of_birth"),
    text_redactor = NULL,
    salt,
    copy_non_tabular = FALSE,
    overwrite = FALSE) {
  source_path <- normalizePath(source_path, winslash = "/", mustWork = TRUE)
  output_path <- normalizePath(output_path, winslash = "/", mustWork = FALSE)
  if (dir.exists(source_path) && (identical(output_path, source_path) || startsWith(paste0(output_path, "/"), paste0(source_path, "/")))) .vc_stop("`output_path` must not be inside the source case directory.")
  if (missing(salt) || length(salt) != 1L || !nzchar(as.character(salt))) .vc_stop("A non-empty project-specific `salt` is required.")
  if (dir.exists(output_path) && !isTRUE(overwrite)) .vc_stop("Output already exists: ", output_path)
  if (dir.exists(output_path) && isTRUE(overwrite)) unlink(output_path, recursive = TRUE, force = TRUE)
  dir.create(output_path, recursive = TRUE, showWarnings = FALSE)
  files <- if (dir.exists(source_path)) list.files(source_path, recursive = TRUE, full.names = TRUE, all.files = TRUE, no.. = TRUE) else source_path
  files <- files[file.exists(files) & !dir.exists(files)]
  rows <- list(); k <- 0L
  for (file in files) {
    relative <- if (dir.exists(source_path)) substring(normalizePath(file, winslash = "/"), nchar(source_path) + 2L) else basename(file)
    target <- file.path(output_path, relative)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    extension <- tolower(tools::file_ext(file))
    status <- "excluded"
    if (extension %in% c("csv", "tsv", "txt")) {
      data <- tryCatch(.vc_read_delimited(file), error = identity)
      if (inherits(data, "error")) {
        status <- paste0("read-error: ", conditionMessage(data))
      } else {
        drop <- intersect(tolower(remove_columns), tolower(names(data)))
        if (length(drop)) data <- data[, !tolower(names(data)) %in% drop, drop = FALSE]
        ids <- names(data)[tolower(names(data)) %in% tolower(id_columns)]
        for (column in ids) data[[column]] <- .vc_hash_ids(data[[column]], salt)
        if (is.function(text_redactor)) {
          character_columns <- names(data)[vapply(data, is.character, logical(1))]
          for (column in character_columns) data[[column]] <- text_redactor(data[[column]], column)
        }
        delimiter <- if (extension == "tsv") "\t" else ","
        .vc_write_delimited(data, target, delimiter)
        status <- "redacted"
      }
    } else if (isTRUE(copy_non_tabular)) {
      file.copy(file, target, copy.mode = TRUE, copy.date = TRUE)
      status <- "copied-unchanged"
    }
    k <- k + 1L
    rows[[k]] <- data.frame(relative_path = relative, extension = extension, status = status, stringsAsFactors = FALSE)
  }
  manifest <- do.call(rbind, rows)
  .ve_atomic_write_csv(manifest, file.path(output_path, "redaction-manifest.csv"))
  fingerprint <- fingerprint_validation_case(output_path)
  out <- list(source_path = source_path, output_path = output_path, manifest = manifest, fingerprint = fingerprint, warning = "Redaction is field-based and must be reviewed before redistribution; copied binary/media files may retain identifying information.")
  class(out) <- "eye_redaction_result"
  out
}

#' @export
print.eye_redaction_result <- function(x, ...) {
  cat("Validation-case redaction\n")
  print(table(x$manifest$status, useNA = "ifany"))
  cat(x$warning, "\n")
  invisible(x)
}

#' Register vendor-field semantics
#'
#' @param corpus_path Corpus directory.
#' @param vendor Vendor.
#' @param native_field,native_meaning Native field and meaning.
#' @param canonical_table,canonical_field Canonical destination.
#' @param unit Unit.
#' @param transformation Transformation description.
#' @param loss_risk None, low, moderate, high, or unsupported.
#' @param evidence_case_id Supporting case.
#' @return Updated semantics registry.
#' @export
register_vendor_semantics <- function(
    corpus_path,
    vendor,
    native_field,
    native_meaning,
    canonical_table,
    canonical_field,
    unit = NA_character_,
    transformation = "identity",
    loss_risk = c("none", "low", "moderate", "high", "unsupported"),
    evidence_case_id = NA_character_) {
  corpus_path <- init_vendor_corpus(corpus_path)
  path <- .vc_semantics_path(corpus_path)
  registry <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  row <- data.frame(
    vendor = .vc_vendor(vendor), native_field = as.character(native_field), native_meaning = as.character(native_meaning),
    canonical_table = as.character(canonical_table), canonical_field = as.character(canonical_field), unit = as.character(unit),
    transformation = as.character(transformation), loss_risk = match.arg(loss_risk), evidence_case_id = as.character(evidence_case_id),
    stringsAsFactors = FALSE
  )
  key <- paste(row$vendor, row$native_field, row$canonical_table, row$canonical_field, sep = "|")
  existing_key <- if (nrow(registry)) paste(registry$vendor, registry$native_field, registry$canonical_table, registry$canonical_field, sep = "|") else character()
  if (key %in% existing_key) registry[match(key, existing_key), ] <- row else registry <- rbind(registry, row)
  .ve_atomic_write_csv(registry, path)
  registry
}

#' Compare semantic mappings between vendors
#'
#' @param x Corpus path or semantics data frame.
#' @param vendors Optional vendor subset.
#' @return An `eye_vendor_semantic_comparison` data frame.
#' @export
compare_vendor_semantics <- function(x, vendors = NULL) {
  d <- if (is.character(x) && length(x) == 1L) utils::read.csv(.vc_semantics_path(normalizePath(x, winslash = "/", mustWork = TRUE)), stringsAsFactors = FALSE) else x
  if (!is.data.frame(d)) .vc_stop("Expected a corpus path or semantics data frame.")
  required <- c("vendor", "native_field", "canonical_table", "canonical_field", "loss_risk")
  if (!all(required %in% names(d))) .vc_stop("Semantics registry is incomplete.")
  if (!is.null(vendors)) d <- d[d$vendor %in% vapply(vendors, .vc_vendor, character(1)), , drop = FALSE]
  if (!nrow(d)) {
    out <- data.frame(canonical_table = character(), canonical_field = character(), vendors = integer(), vendor_names = character(), native_fields = character(), transformations = character(), maximum_loss_risk = character(), stringsAsFactors = FALSE)
    class(out) <- c("eye_vendor_semantic_comparison", "data.frame")
    return(out)
  }
  keys <- unique(d[c("canonical_table", "canonical_field")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    z <- d[d$canonical_table == keys$canonical_table[i] & d$canonical_field == keys$canonical_field[i], , drop = FALSE]
    data.frame(
      canonical_table = keys$canonical_table[i], canonical_field = keys$canonical_field[i],
      vendors = length(unique(z$vendor)), vendor_names = paste(sort(unique(z$vendor)), collapse = ", "),
      native_fields = paste(paste(z$vendor, z$native_field, sep = ":"), collapse = "; "),
      transformations = paste(unique(z$transformation), collapse = "; "),
      maximum_loss_risk = {
        risk_order <- c(none = 0L, low = 1L, moderate = 2L, high = 3L, unsupported = 4L)
        observed <- unname(risk_order[z$loss_risk])
        observed <- observed[is.finite(observed)]
        if (!length(observed)) NA_character_ else names(risk_order)[which.max(risk_order == max(observed))]
      },
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  class(out) <- c("eye_vendor_semantic_comparison", "data.frame")
  out
}

#' @export
print.eye_vendor_semantic_comparison <- function(x, ...) {
  cat("Cross-vendor semantic comparison\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

.vc_table_key <- function(table, name) {
  candidates <- switch(name,
    recordings = c("recording_id"),
    gaze_samples = c("recording_id", "sample_id"),
    eye_samples = c("recording_id", "eye_sample_id"),
    events = c("recording_id", "event_id"),
    intervals = c("recording_id", "interval_id"),
    responses = c("participant_id", "item_id", "trial_id"),
    features = c("participant_id", "item_id", "trial_id", "feature_name"),
    intersect(c("recording_id", "participant_id", "trial_id", "item_id", "sample_id", "event_id"), names(table))
  )
  intersect(candidates, names(table))
}

.vc_compare_columns <- function(a, b, keys, tolerance) {
  shared <- intersect(setdiff(names(a), keys), setdiff(names(b), keys))
  if (!length(shared)) return(data.frame())
  if (length(keys)) {
    add_occurrence <- function(x) {
      key <- do.call(paste, c(lapply(x[keys], function(z) ifelse(is.na(z), "<NA>", as.character(z))), sep = "\r"))
      x$.occurrence <- ave(seq_along(key), key, FUN = seq_along)
      x
    }
    aa <- add_occurrence(a[c(keys, shared), drop = FALSE])
    bb <- add_occurrence(b[c(keys, shared), drop = FALSE])
    merged <- merge(aa, bb, by = c(keys, ".occurrence"), all = TRUE, suffixes = c(".source", ".roundtrip"), sort = FALSE)
  } else {
    n <- max(nrow(a), nrow(b)); merged <- data.frame(.row = seq_len(n))
    for (column in shared) {
      merged[[paste0(column, ".source")]] <- c(a[[column]], rep(NA, n - nrow(a)))
      merged[[paste0(column, ".roundtrip")]] <- c(b[[column]], rep(NA, n - nrow(b)))
    }
  }
  rows <- lapply(shared, function(column) {
    x <- merged[[paste0(column, ".source")]]; y <- merged[[paste0(column, ".roundtrip")]]
    numeric <- is.numeric(x) && is.numeric(y)
    comparable <- !is.na(x) & !is.na(y)
    difference <- if (numeric) abs(x - y) else as.numeric(as.character(x) != as.character(y))
    data.frame(
      column = column,
      source_nonmissing = sum(!is.na(x)),
      roundtrip_nonmissing = sum(!is.na(y)),
      missingness_change = mean(is.na(y)) - mean(is.na(x)),
      comparable = sum(comparable),
      mismatch_rate = if (any(comparable)) mean(difference[comparable] > tolerance) else NA_real_,
      max_absolute_difference = if (numeric && any(comparable)) max(difference[comparable], na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

#' Audit semantic and numerical loss after a round trip
#'
#' @param source,roundtrip Source and re-imported `eye_dataset` objects.
#' @param tables Canonical tables to compare.
#' @param tolerance Numeric tolerance.
#' @return An `eye_roundtrip_loss_audit` object.
#' @export
audit_roundtrip_loss <- function(source, roundtrip, tables = canonical_table_names(), tolerance = 1e-8) {
  if (!inherits(source, "eye_dataset") || !inherits(roundtrip, "eye_dataset")) .vc_stop("Both inputs must be `eye_dataset` objects.")
  tolerance <- suppressWarnings(as.numeric(tolerance))
  if (length(tolerance) != 1L || !is.finite(tolerance) || tolerance < 0) .vc_stop("`tolerance` must be a non-negative finite number.")
  tables <- intersect(as.character(tables), canonical_table_names())
  rows <- list(); details <- list()
  for (table in tables) {
    a <- source[[table]]; b <- roundtrip[[table]]
    if (!is.data.frame(a) || !is.data.frame(b)) next
    keys <- .vc_table_key(a, table)
    detail <- .vc_compare_columns(a, b, keys, tolerance)
    if (nrow(detail)) detail$table <- table
    details[[table]] <- detail
    rows[[table]] <- data.frame(
      table = table,
      source_rows = nrow(a), roundtrip_rows = nrow(b), row_difference = nrow(b) - nrow(a),
      source_columns = ncol(a), roundtrip_columns = ncol(b),
      missing_source_columns = paste(setdiff(names(a), names(b)), collapse = ","),
      extra_roundtrip_columns = paste(setdiff(names(b), names(a)), collapse = ","),
      maximum_mismatch_rate = if (nrow(detail) && any(is.finite(detail$mismatch_rate))) max(detail$mismatch_rate, na.rm = TRUE) else NA_real_,
      status = if (nrow(a) == nrow(b) && !length(setdiff(names(a), names(b))) && (!nrow(detail) || all(is.na(detail$mismatch_rate) | detail$mismatch_rate == 0))) "lossless" else "review",
      stringsAsFactors = FALSE
    )
  }
  summary <- if (length(rows)) do.call(rbind, rows) else data.frame(table = character(), source_rows = integer(), roundtrip_rows = integer(), row_difference = integer(), source_columns = integer(), roundtrip_columns = integer(), missing_source_columns = character(), extra_roundtrip_columns = character(), maximum_mismatch_rate = numeric(), status = character(), stringsAsFactors = FALSE)
  out <- list(summary = summary, details = .ve_bind_rows(details), tolerance = tolerance)
  class(out) <- "eye_roundtrip_loss_audit"
  out
}

#' @export
print.eye_roundtrip_loss_audit <- function(x, ...) {
  cat("Round-trip loss audit\n")
  print(x$summary, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_roundtrip_loss_audit <- function(x, ...) {
  values <- x$summary$maximum_mismatch_rate
  values[!is.finite(values)] <- 0
  names(values) <- x$summary$table
  graphics::barplot(values, las = 2, ylab = "Maximum mismatch rate", main = "Round-trip loss", ...)
  invisible(x$summary)
}

#' Audit vendor field coverage against canonical semantics
#'
#' @param semantics Semantics registry or corpus path.
#' @param required_fields Data frame with canonical table/field pairs.
#' @return Vendor-by-field coverage table.
#' @export
audit_vendor_field_coverage <- function(semantics, required_fields) {
  d <- if (is.character(semantics) && length(semantics) == 1L) utils::read.csv(.vc_semantics_path(normalizePath(semantics, winslash = "/", mustWork = TRUE)), stringsAsFactors = FALSE) else semantics
  if (!is.data.frame(d) || !is.data.frame(required_fields)) .vc_stop("Semantics and required fields must be data frames.")
  if (!all(c("canonical_table", "canonical_field") %in% names(required_fields))) .vc_stop("Required fields need canonical table and field columns.")
  vendors <- sort(unique(d$vendor))
  rows <- lapply(vendors, function(vendor) {
    z <- d[d$vendor == vendor, , drop = FALSE]
    keys <- paste(z$canonical_table, z$canonical_field, sep = "::")
    data.frame(
      vendor = vendor,
      canonical_table = required_fields$canonical_table,
      canonical_field = required_fields$canonical_field,
      supported = paste(required_fields$canonical_table, required_fields$canonical_field, sep = "::") %in% keys,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  class(out) <- c("eye_vendor_field_coverage", "data.frame")
  out
}

#' Promote a case support level only when evidence is supplied
#'
#' @param corpus_path Corpus directory.
#' @param case_id Case identifier.
#' @param level New support level.
#' @param validation Validation result/audit.
#' @param reviewer Reviewer identifier.
#' @param notes Notes.
#' @return Updated case row.
#' @export
promote_vendor_support <- function(corpus_path, case_id, level = c("fixture-tested", "empirically-validated"), validation, reviewer, notes = NA_character_) {
  level <- match.arg(level)
  reviewer <- as.character(reviewer)[1L]
  if (is.na(reviewer) || !nzchar(trimws(reviewer))) .vc_stop("A non-empty reviewer identifier is required.")
  registry <- read_vendor_registry(corpus_path)
  index <- match(case_id, registry$case_id)
  if (is.na(index)) .vc_stop("Unknown case: ", case_id)
  pass <- if (inherits(validation, "eye_format_validation")) identical(validation$status, "pass") else if (is.list(validation) && !is.null(validation$status)) tolower(validation$status[1L]) %in% c("pass", "passed", "success") else if (is.logical(validation)) isTRUE(validation) else FALSE
  if (!pass) .vc_stop("Support cannot be promoted without passing validation evidence.")
  if (level == "empirically-validated" && (!isTRUE(registry$independent_source[index]) || !isTRUE(registry$licence_reviewed[index]))) .vc_stop("Empirical promotion requires independent-source and licensing evidence.")
  registry$support_level[index] <- level
  registry$status[index] <- "validated"
  registry$notes[index] <- paste(na.omit(c(registry$notes[index], paste0("Promoted by ", reviewer, " at ", format(Sys.time(), tz = "UTC", usetz = TRUE), ": ", notes))), collapse = " | ")
  write_vendor_registry(registry, corpus_path)
  registry[index, , drop = FALSE]
}

#' Build the declared/fixture/empirical compatibility matrix
#'
#' @param x Corpus path or registry data frame.
#' @param required_vendors Required vendors.
#' @param min_empirical_cases Minimum independent empirical cases per vendor.
#' @return An `eye_vendor_compatibility_matrix` data frame.
#' @export
build_compatibility_matrix <- function(x, required_vendors = c("gazepoint", "tobii", "pupillabs", "eyelink", "smi"), min_empirical_cases = 2L) {
  d <- if (is.character(x) && length(x) == 1L) read_vendor_registry(x) else x
  if (!is.data.frame(d)) .vc_stop("Expected a corpus path or registry data frame.")
  min_empirical_cases <- suppressWarnings(as.integer(min_empirical_cases))
  if (length(min_empirical_cases) != 1L || is.na(min_empirical_cases) || min_empirical_cases < 1L) .vc_stop("`min_empirical_cases` must be a positive integer.")
  required_vendors <- unique(vapply(required_vendors, .vc_vendor, character(1)))
  vendors <- union(required_vendors, unique(d$vendor))
  order <- c(declared = 1L, `fixture-tested` = 2L, `empirically-validated` = 3L)
  rows <- lapply(vendors, function(vendor) {
    z <- d[d$vendor == vendor, , drop = FALSE]
    empirical <- z$support_level == "empirically-validated" & z$independent_source & z$licence_reviewed & z$status == "validated"
    observed_support <- unname(order[z$support_level])
    observed_support <- observed_support[is.finite(observed_support)]
    highest <- if (length(observed_support)) names(order)[which(order == max(observed_support))[1L]] else "none"
    data.frame(
      vendor = vendor,
      declared_cases = sum(z$support_level == "declared", na.rm = TRUE),
      fixture_tested_cases = sum(z$support_level == "fixture-tested", na.rm = TRUE),
      empirical_cases = sum(empirical, na.rm = TRUE),
      devices = paste(sort(unique(z$device_model[!is.na(z$device_model)])), collapse = "; "),
      software_versions = paste(sort(unique(z$software_version[!is.na(z$software_version)])), collapse = "; "),
      highest_support = highest,
      production_claim_allowed = sum(empirical, na.rm = TRUE) >= min_empirical_cases,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  class(out) <- c("eye_vendor_compatibility_matrix", "data.frame")
  out
}

#' @export
print.eye_vendor_compatibility_matrix <- function(x, ...) {
  cat("Vendor compatibility matrix\n")
  print.data.frame(x, row.names = FALSE)
  invisible(x)
}

#' @export
plot.eye_vendor_compatibility_matrix <- function(x, ...) {
  value <- x$empirical_cases; names(value) <- x$vendor
  graphics::barplot(value, las = 2, ylab = "Independent empirical cases", main = "Vendor validation evidence", ...)
  invisible(x)
}

#' Write a vendor case evidence report
#'
#' @param corpus_path Corpus directory.
#' @param case_id Case identifier.
#' @param path Markdown output path.
#' @param validation Optional validation result.
#' @param roundtrip Optional round-trip audit.
#' @return Normalized report path.
#' @export
write_vendor_case_report <- function(corpus_path, case_id, path, validation = NULL, roundtrip = NULL) {
  registry <- read_vendor_registry(corpus_path)
  row <- registry[registry$case_id == case_id, , drop = FALSE]
  if (!nrow(row)) .vc_stop("Unknown case: ", case_id)
  fingerprint_path <- file.path(normalizePath(corpus_path, winslash = "/", mustWork = TRUE), "fingerprints", paste0(case_id, ".csv"))
  fingerprint <- if (file.exists(fingerprint_path)) utils::read.csv(fingerprint_path, stringsAsFactors = FALSE) else data.frame()
  lines <- c(
    paste0("# Vendor validation case: ", case_id), "",
    paste0("Generated: ", format(Sys.time(), tz = "UTC", usetz = TRUE)), "",
    "## Registration", "", .ve_markdown_table(row), "",
    "## Source fingerprint", "", .ve_markdown_table(fingerprint), ""
  )
  if (!is.null(validation)) lines <- c(lines, "## Validation", "", "Validation evidence was supplied as an R object. The object class and status are recorded below.", "", paste0("- Class: ", paste(class(validation), collapse = ", ")), paste0("- Status: ", .vc_or(validation$status, "not exposed")), "")
  if (inherits(roundtrip, "eye_roundtrip_loss_audit")) lines <- c(lines, "## Round-trip loss", "", .ve_markdown_table(roundtrip$summary), "")
  lines <- c(lines, "## Claim boundary", "", paste0("Registered support level: **", row$support_level, "**."), "Fixture tests are not independent empirical validation. Production compatibility claims require the declared minimum number of independent real-export cases.")
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, path, useBytes = TRUE)
  normalizePath(path, winslash = "/", mustWork = TRUE)
}
