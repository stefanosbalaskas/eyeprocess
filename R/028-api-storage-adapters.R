# Stable API contracts, scalable storage, and external-engine adapters --------

.as_stop <- function(message, class = "eyeprocess_contract_error") {
  condition <- structure(list(message = message, call = NULL), class = c(class, "error", "condition"))
  stop(condition)
}

.as_or <- function(x, y) if (is.null(x) || !length(x)) y else x

.as_now <- function() format(Sys.time(), tz = "UTC", usetz = TRUE)

.as_hash <- function(x) sprintf("%010d", .ve_hash_int(paste(as.integer(serialize(x, NULL, version = 3)), collapse = ",")))

#' Return the public eyeprocess API version
#'
#' @return A version object with contract metadata.
#' @export
eyeprocess_api_version <- function() {
  out <- base::numeric_version("0.5.0")
  attr(out, "object_schema") <- "2.0.0"
  attr(out, "storage_schema") <- "2.0.0"
  attr(out, "model_contract") <- "1.0.0"
  out
}

#' Describe a stable object schema
#'
#' @param object Object or schema name.
#' @return A machine-readable schema list.
#' @export
object_schema <- function(object = c("eye_dataset", "eyeprocess_model", "validation_plan", "validation_collection", "vendor_corpus", "eye_storage")) {
  if (!is.character(object) || length(object) != 1L) {
    if (inherits(object, "eye_dataset")) object <- "eye_dataset"
    else if (inherits(object, "eyeprocess_model")) object <- "eyeprocess_model"
    else if (inherits(object, "eye_validation_job_plan")) object <- "validation_plan"
    else if (inherits(object, "eye_validation_collection")) object <- "validation_collection"
    else .as_stop("Could not infer an object schema.")
  }
  object <- match.arg(object)
  schemas <- list(
    eye_dataset = list(
      version = "2.0.0", class = "eye_dataset",
      required_components = c("recordings", "provenance"),
      canonical_tables = canonical_table_names(),
      identifiers = c("participant_id", "recording_id", "trial_id", "item_id", "sample_id", "event_id"),
      invariant = "Native fields and time/coordinate transformations remain traceable through provenance."
    ),
    eyeprocess_model = list(
      version = "1.0.0", class = "eyeprocess_model",
      required_components = c("engine", "specification", "fit", "diagnostics", "provenance"),
      optional_components = c("parameters", "predictions", "data_signature", "evidence_status"),
      invariant = "Availability of a fit is not evidence of scientific validity."
    ),
    validation_plan = list(
      version = "1.0.0", class = "eye_validation_job_plan",
      required_columns = c("job_id", "scenario_id", "replication", "seed", "status"),
      invariant = "Every job identity and seed are deterministic functions of the plan."
    ),
    validation_collection = list(
      version = "1.0.0", class = "eye_validation_collection",
      required_components = c("jobs", "results", "estimates", "diagnostics", "paths"),
      invariant = "Replications are aggregated without silently dropping failures."
    ),
    vendor_corpus = list(
      version = "1.0.0", class = "eye_vendor_corpus",
      support_levels = c("declared", "fixture-tested", "empirically-validated"),
      invariant = "Production claims require independent, version-specific empirical exports."
    ),
    eye_storage = list(
      version = "2.0.0", class = "eye_partitioned_storage",
      required_files = c("_partitions.csv", "_transactions.csv", "_eyeprocess_storage.json or _eyeprocess_storage.dput"),
      invariant = "Writes are atomic and each partition is fingerprinted."
    )
  )
  schemas[[object]]
}

.as_model_classes <- c(
  eye_dynamic_irtree = "dynamic_irtree",
  eye_theory_strategy_irt = "theory_strategy",
  eye_gaze_diffusion_irt = "gaze_diffusion",
  eye_functional_pupil_irt = "functional_pupil",
  eyeprocess_model = "generic"
)

#' Validate a fitted model against the stable model contract
#'
#' @param object Model object.
#' @param strict Whether warnings become errors.
#' @return An `eye_model_contract_validation` object.
#' @export
validate_model_object <- function(object, strict = FALSE) {
  list_like <- is.list(object) || is.environment(object)
  if (!list_like) {
    findings <- data.frame(check = "list_like", passed = FALSE, severity = "error", message = "Model objects must be list-like.", stringsAsFactors = FALSE)
    out <- list(valid = FALSE, findings = findings, model_family = NA_character_, schema_version = "1.0.0", object_fingerprint = .as_hash(object))
    class(out) <- "eye_model_contract_validation"
    if (strict) .as_stop(findings$message)
    return(out)
  }
  class_hit <- intersect(names(.as_model_classes), class(object))
  findings <- list()
  add <- function(check, passed, severity = "error", message = "") {
    findings[[length(findings) + 1L]] <<- data.frame(check = check, passed = isTRUE(passed), severity = severity, message = message, stringsAsFactors = FALSE)
  }
  add("recognized_class", length(class_hit) > 0L, message = if (!length(class_hit)) "Unrecognized model class." else class_hit[1L])
  add("has_specification", !is.null(object$spec) || !is.null(object$specification), message = "Model should retain its full specification.")
  add("has_fit", !is.null(object$model) || !is.null(object$fit), message = "Model fit component is absent.")
  add("has_interpretation", !is.null(object$interpretation) || !is.null(object$spec$interpretation), severity = "warning", message = "Interpretive safeguard is absent.")
  diagnostics <- object$diagnostics
  if (is.null(diagnostics) && !is.null(object$model$diagnostics)) diagnostics <- object$model$diagnostics
  add("has_diagnostics", !is.null(diagnostics), severity = "warning", message = "Convergence or diagnostic evidence is absent.")
  add("serializable", !inherits(try(serialize(object, NULL, version = 3), silent = TRUE), "try-error"), message = "Object cannot be serialized.")
  result <- do.call(rbind, findings)
  failed <- result[!result$passed & (result$severity == "error" | strict), , drop = FALSE]
  out <- list(valid = nrow(failed) == 0L, findings = result, model_family = if (length(class_hit)) unname(.as_model_classes[class_hit[1L]]) else NA_character_, schema_version = "1.0.0", object_fingerprint = .as_hash(object))
  class(out) <- "eye_model_contract_validation"
  if (strict && !out$valid) .as_stop(paste(failed$message, collapse = " "))
  out
}

#' @export
print.eye_model_contract_validation <- function(x, ...) {
  cat("eyeprocess model-contract validation\n")
  cat("Valid:       ", x$valid, "\n", sep = "")
  cat("Family:      ", x$model_family, "\n", sep = "")
  cat("Fingerprint: ", x$object_fingerprint, "\n", sep = "")
  print(x$findings, row.names = FALSE)
  invisible(x)
}

#' Upgrade a legacy eye dataset
#'
#' @param x Dataset.
#' @param target_version Target schema version.
#' @param copy Whether to copy before migration.
#' @return Upgraded dataset with migration log.
#' @export
upgrade_eye_dataset <- function(x, target_version = "2.0.0", copy = TRUE) {
  if (!inherits(x, "eye_dataset")) .as_stop("Expected an `eye_dataset`.")
  if (!identical(as.character(target_version), "2.0.0")) .as_stop("This release can upgrade datasets only to schema version 2.0.0.")
  if (copy) x <- unserialize(serialize(x, NULL, version = 3))
  current <- attr(x, "eyeprocess_schema_version")
  if (is.null(current)) current <- "1.0.0"
  log <- data.frame(from = current, to = target_version, operation = character(1), timestamp_utc = .as_now(), stringsAsFactors = FALSE)
  for (table in intersect(canonical_table_names(), names(x))) {
    if (is.data.frame(x[[table]])) rownames(x[[table]]) <- NULL
  }
  if (is.null(x$provenance)) x$provenance <- data.frame()
  log$operation <- "normalize canonical tables; preserve existing content"
  attr(x, "eyeprocess_schema_version") <- target_version
  attr(x, "eyeprocess_migration_log") <- rbind(attr(x, "eyeprocess_migration_log"), log)
  x
}

#' Upgrade a legacy eyeprocess model
#'
#' @param x Model object.
#' @param target_version Target model-contract version.
#' @return Upgraded model.
#' @export
upgrade_eyeprocess_model <- function(x, target_version = "1.0.0") {
  if (!is.list(x)) .as_stop("Model objects must be list-like.")
  if (!identical(as.character(target_version), "1.0.0")) .as_stop("This release can upgrade models only to contract version 1.0.0.")
  if (is.null(x$specification) && !is.null(x$spec)) x$specification <- x$spec
  if (is.null(x$fit) && !is.null(x$model)) x$fit <- x$model
  if (is.null(x$engine)) x$engine <- .as_or(x$spec$engine, .as_or(x$specification$engine, "unknown"))
  if (is.null(x$diagnostics) && !is.null(x$model$diagnostics)) x$diagnostics <- x$model$diagnostics
  if (is.null(x$provenance)) x$provenance <- list(upgraded_utc = .as_now(), source_class = class(x))
  x$model_contract_version <- target_version
  if (!inherits(x, "eyeprocess_model")) class(x) <- unique(c(class(x), "eyeprocess_model"))
  x
}

#' Declare a deprecation in a structured form
#'
#' @param old Deprecated symbol.
#' @param replacement Replacement symbol.
#' @param since Version where deprecation started.
#' @param remove_after Earliest removal version.
#' @param reason Reason.
#' @return Deprecation record.
#' @export
eyeprocess_deprecation <- function(old, replacement, since, remove_after, reason = "") {
  data.frame(old = old, replacement = replacement, since = since, remove_after = remove_after, reason = reason, stringsAsFactors = FALSE)
}

# Partitioned storage ----------------------------------------------------------

.as_storage_metadata <- function(path) file.path(path, "_eyeprocess_storage.json")
.as_storage_transactions <- function(path) file.path(path, "_transactions.csv")
.as_partition_manifest <- function(path) file.path(path, "_partitions.csv")

.as_storage_format <- function(format) {
  format <- match.arg(format, c("parquet", "csv", "rds"))
  if (format == "parquet" && !requireNamespace("arrow", quietly = TRUE)) .as_stop("Parquet storage requires the `arrow` package.")
  format
}

#' Create a partition specification
#'
#' @param by Partition columns.
#' @param format Storage format.
#' @param compression Compression codec for Parquet.
#' @param max_rows Maximum rows per physical file.
#' @return An `eye_partition_spec`.
#' @export
partition_eye_storage <- function(by = c("participant_id", "session_id", "recording_id"), format = c("parquet", "csv", "rds"), compression = "zstd", max_rows = 1000000L) {
  format <- .as_storage_format(match.arg(format))
  max_rows <- suppressWarnings(as.integer(max_rows))
  if (length(max_rows) != 1L || is.na(max_rows) || max_rows < 1L) .as_stop("`max_rows` must be a positive integer.")
  if (!is.character(by) || anyNA(by)) .as_stop("`by` must be a character vector without missing values.")
  out <- list(by = unique(by[nzchar(by)]), format = format, compression = compression, max_rows = max_rows, schema_version = "2.0.0")
  class(out) <- "eye_partition_spec"
  out
}

#' @export
print.eye_partition_spec <- function(x, ...) {
  cat("eyeprocess partition specification\n")
  cat("By:         ", paste(x$by, collapse = ", "), "\n", sep = "")
  cat("Format:     ", x$format, "\n", sep = "")
  cat("Max rows:   ", x$max_rows, "\n", sep = "")
  invisible(x)
}

.as_write_table <- function(data, path, format, compression = "zstd") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- paste0(path, ".tmp-", Sys.getpid(), "-", sample.int(1e8, 1L))
  on.exit(unlink(temporary, recursive = TRUE, force = TRUE), add = TRUE)
  if (format == "parquet") arrow::write_parquet(data, temporary, compression = compression)
  else if (format == "csv") utils::write.csv(data, temporary, row.names = FALSE, na = "")
  else saveRDS(data, temporary, version = 3, compress = "xz")
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(temporary, path)) .as_stop(sprintf("Atomic storage rename failed for `%s`.", path))
  invisible(path)
}

.as_read_table <- function(path, format) {
  if (format == "parquet") as.data.frame(arrow::read_parquet(path))
  else if (format == "csv") utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  else readRDS(path)
}

.as_partition_path <- function(root, table, keys, values, file_index, extension) {
  directory <- file.path(root, table)
  if (length(keys)) {
    for (i in seq_along(keys)) {
      raw_value <- if (is.na(values[i])) "<NA>" else enc2utf8(as.character(values[i]))
      label <- paste0(.ve_safe_name(raw_value, "NA"), "-", sprintf("%010d", .ve_hash_int(raw_value)))
      directory <- file.path(directory, paste0(.ve_safe_name(keys[i]), "=", label))
    }
  }
  file.path(directory, sprintf("part-%06d.%s", file_index, extension))
}

#' Write an eye dataset as atomic partitioned storage
#'
#' @param x Eye dataset or named list of data frames.
#' @param path Destination directory.
#' @param spec Partition specification.
#' @param overwrite Whether to replace an existing store.
#' @param tables Tables to write.
#' @return An `eye_partitioned_storage` object.
#' @export
write_partitioned_eye_storage <- function(x, path, spec = partition_eye_storage(format = if (requireNamespace("arrow", quietly = TRUE)) "parquet" else "rds"), overwrite = FALSE, tables = NULL) {
  if (!is.list(x)) .as_stop("`x` must be an eye dataset or named list of tables.")
  if (!inherits(spec, "eye_partition_spec")) .as_stop("`spec` must be created by `partition_eye_storage()`.")
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE)) && !overwrite) .as_stop("Storage directory is not empty; use `overwrite = TRUE`.")
  staging <- paste0(path, ".staging-", Sys.getpid(), "-", sample.int(1e8, 1L))
  unlink(staging, recursive = TRUE, force = TRUE); dir.create(staging, recursive = TRUE)
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  if (is.null(tables)) tables <- names(x)[vapply(x, is.data.frame, logical(1))]
  tables <- unique(as.character(tables))
  tables <- tables[nzchar(tables) & tables %in% names(x) & vapply(x[tables], is.data.frame, logical(1))]
  if (!length(tables)) .as_stop("No data-frame tables were selected for storage.")
  extension <- c(parquet = "parquet", csv = "csv", rds = "rds")[[spec$format]]
  manifest <- list(); index <- 0L
  for (table in tables) {
    data <- x[[table]]
    if (!is.data.frame(data)) next
    keys <- intersect(spec$by, names(data))
    groups <- if (length(keys) && nrow(data)) {
      encoded <- lapply(data[keys], function(value) ifelse(is.na(value), "<NA>", enc2utf8(as.character(value))))
      do.call(paste, c(encoded, sep = "\r"))
    } else rep("all", nrow(data))
    split_index <- split(seq_len(nrow(data)), groups, drop = TRUE)
    if (!length(split_index) && !nrow(data)) split_index <- list(integer())
    for (group in split_index) {
      chunks <- if (length(group)) split(group, ceiling(seq_along(group) / spec$max_rows)) else list(integer())
      for (chunk in chunks) {
        index <- index + 1L
        values <- if (length(keys) && length(chunk)) as.character(data[chunk[1L], keys, drop = TRUE]) else rep("NA", length(keys))
        target <- .as_partition_path(staging, table, keys, values, index, extension)
        piece <- data[chunk, , drop = FALSE]
        .as_write_table(piece, target, spec$format, spec$compression)
        manifest[[index]] <- data.frame(
          table = table, relative_path = substring(normalizePath(target, winslash = "/"), nchar(normalizePath(staging, winslash = "/")) + 2L),
          rows = nrow(piece), bytes = file.info(target)$size, fingerprint = unname(tools::md5sum(target)),
          partition_columns = paste(keys, collapse = ","), partition_values = paste(values, collapse = ","),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  partition_manifest <- if (length(manifest)) do.call(rbind, manifest) else data.frame(
    table = character(), relative_path = character(), rows = integer(), bytes = numeric(), fingerprint = character(),
    partition_columns = character(), partition_values = character(), stringsAsFactors = FALSE
  )
  utils::write.csv(partition_manifest, .as_partition_manifest(staging), row.names = FALSE)
  transactions <- data.frame(transaction_id = paste0("tx-", .as_hash(list(path, Sys.time(), partition_manifest))), action = "write", timestamp_utc = .as_now(), tables = paste(tables, collapse = ","), partitions = nrow(partition_manifest), rows = sum(partition_manifest$rows, na.rm = TRUE), status = "committed", stringsAsFactors = FALSE)
  utils::write.csv(transactions, .as_storage_transactions(staging), row.names = FALSE)
  metadata <- list(
    class = "eye_partitioned_storage", schema_version = spec$schema_version, api_version = as.character(eyeprocess_api_version()),
    format = spec$format, compression = spec$compression, partition_by = spec$by, created_utc = .as_now(),
    source_class = class(x), tables = tables, transaction_id = transactions$transaction_id
  )
  if (requireNamespace("jsonlite", quietly = TRUE)) jsonlite::write_json(metadata, .as_storage_metadata(staging), auto_unbox = TRUE, pretty = TRUE)
  else dput(metadata, file.path(staging, "_eyeprocess_storage.dput"))
  backup <- NULL
  if (dir.exists(path)) {
    backup <- paste0(path, ".backup-", Sys.getpid(), "-", sample.int(1e8, 1L))
    if (!file.rename(path, backup)) .as_stop("Could not move the existing storage aside before commit.")
  }
  committed <- file.rename(staging, path)
  if (!committed) {
    if (!is.null(backup) && dir.exists(backup)) file.rename(backup, path)
    .as_stop("Could not atomically commit partitioned storage.")
  }
  if (!is.null(backup) && dir.exists(backup)) unlink(backup, recursive = TRUE, force = TRUE)
  open_partitioned_eye_storage(path)
}

#' Open partitioned eye storage
#'
#' @param path Storage directory.
#' @return An `eye_partitioned_storage` object.
#' @export
open_partitioned_eye_storage <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  metadata_file <- .as_storage_metadata(path)
  if (!file.exists(metadata_file) && !file.exists(file.path(path, "_eyeprocess_storage.dput"))) .as_stop("Not an eyeprocess partitioned store.")
  if (!file.exists(.as_partition_manifest(path)) || !file.exists(.as_storage_transactions(path))) .as_stop("Storage manifests are incomplete.")
  metadata <- if (file.exists(metadata_file)) {
    if (!requireNamespace("jsonlite", quietly = TRUE)) .as_stop("Opening JSON storage metadata requires the `jsonlite` package.")
    jsonlite::read_json(metadata_file, simplifyVector = TRUE)
  } else dget(file.path(path, "_eyeprocess_storage.dput"))
  partitions <- utils::read.csv(.as_partition_manifest(path), stringsAsFactors = FALSE)
  transactions <- utils::read.csv(.as_storage_transactions(path), stringsAsFactors = FALSE)
  out <- list(path = path, metadata = metadata, partitions = partitions, transactions = transactions)
  class(out) <- "eye_partitioned_storage"
  out
}

#' @export
print.eye_partitioned_storage <- function(x, ...) {
  cat("Partitioned eyeprocess storage\n")
  cat("Path:       ", x$path, "\n", sep = "")
  cat("Format:     ", x$metadata$format, "\n", sep = "")
  cat("Partitions: ", nrow(x$partitions), "\n", sep = "")
  cat("Rows:       ", sum(x$partitions$rows), "\n", sep = "")
  invisible(x)
}

#' Query partitioned eye storage lazily where possible
#'
#' @param storage Storage object or path.
#' @param table Canonical table.
#' @param filters Named list of equality filters.
#' @param columns Optional selected columns.
#' @param collect Whether to collect an Arrow query.
#' @return Data frame or Arrow query.
#' @export
query_eye_storage <- function(storage, table, filters = list(), columns = NULL, collect = TRUE) {
  if (is.character(storage)) storage <- open_partitioned_eye_storage(storage)
  if (!inherits(storage, "eye_partitioned_storage")) .as_stop("Expected partitioned storage.")
  partition <- storage$partitions[storage$partitions$table == table, , drop = FALSE]
  if (!nrow(partition)) .as_stop(sprintf("Table `%s` is absent.", table))
  files <- file.path(storage$path, partition$relative_path)
  format <- storage$metadata$format
  if (format == "parquet" && requireNamespace("arrow", quietly = TRUE)) {
    dataset <- arrow::open_dataset(files, format = "parquet")
    # Arrow expression construction and projection methods vary across releases.
    # Return the lazy Dataset only when no post-collection operation was requested;
    # otherwise collect once and apply stable base-R filtering/projection below.
    if (!collect && !length(filters) && !length(columns)) return(dataset)
    result <- as.data.frame(dataset)
  } else result <- do.call(rbind, lapply(files, .as_read_table, format = format))
  for (name in names(filters)) {
    if (!name %in% names(result)) .as_stop(sprintf("Filter column `%s` is absent.", name))
    result <- result[result[[name]] %in% filters[[name]], , drop = FALSE]
  }
  if (length(columns)) {
    missing_columns <- setdiff(columns, names(result))
    if (length(missing_columns)) .as_stop(sprintf("Selected columns are absent: %s.", paste(missing_columns, collapse = ", ")))
    result <- result[columns]
  }
  rownames(result) <- NULL
  result
}

#' Validate storage metadata and partition fingerprints
#'
#' @param storage Storage object or path.
#' @param verify_hashes Whether to recompute all fingerprints.
#' @return An `eye_storage_validation` object.
#' @export
validate_eye_storage_metadata <- function(storage, verify_hashes = TRUE) {
  if (is.character(storage)) storage <- open_partitioned_eye_storage(storage)
  p <- storage$partitions
  files <- file.path(storage$path, p$relative_path)
  exists <- file.exists(files)
  bytes <- ifelse(exists, file.info(files)$size, NA_real_)
  hash <- rep(NA_character_, length(files))
  if (verify_hashes && any(exists)) hash[exists] <- unname(tools::md5sum(files[exists]))
  findings <- data.frame(
    relative_path = p$relative_path, exists = exists,
    bytes_match = exists & bytes == p$bytes,
    fingerprint_match = if (verify_hashes) exists & hash == p$fingerprint else NA,
    stringsAsFactors = FALSE
  )
  out <- list(valid = all(findings$exists & findings$bytes_match & (is.na(findings$fingerprint_match) | findings$fingerprint_match)), findings = findings, metadata = storage$metadata)
  class(out) <- "eye_storage_validation"
  out
}

#' Detect missing, truncated, or modified partitions
#'
#' @param storage Storage object or path.
#' @return Findings for corrupt partitions.
#' @export
detect_corrupt_partitions <- function(storage) {
  validation <- validate_eye_storage_metadata(storage, verify_hashes = TRUE)
  bad_fingerprint <- !is.na(validation$findings$fingerprint_match) & !validation$findings$fingerprint_match
  out <- validation$findings[!validation$findings$exists | !validation$findings$bytes_match | bad_fingerprint, , drop = FALSE]
  class(out) <- c("eye_corrupt_partitions", "data.frame")
  out
}

#' Return the transaction manifest
#'
#' @param storage Storage object or path.
#' @return Transaction data frame.
#' @export
storage_transaction_manifest <- function(storage) {
  if (is.character(storage)) storage <- open_partitioned_eye_storage(storage)
  storage$transactions
}

#' Migrate a storage schema through an atomic rewrite
#'
#' @param storage Storage object or path.
#' @param target_path Destination.
#' @param target_version Target schema version.
#' @param format Target format.
#' @param overwrite Whether to replace target.
#' @return Migrated storage.
#' @export
migrate_eye_storage_schema <- function(storage, target_path, target_version = "2.0.0", format = NULL, overwrite = FALSE) {
  if (is.character(storage)) storage <- open_partitioned_eye_storage(storage)
  if (!identical(as.character(target_version), "2.0.0")) .as_stop("This release can migrate storage only to schema version 2.0.0.")
  if (is.null(format)) format <- storage$metadata$format
  tables <- unique(storage$partitions$table)
  data <- setNames(lapply(tables, function(table) query_eye_storage(storage, table)), tables)
  spec <- partition_eye_storage(by = storage$metadata$partition_by, format = format, compression = storage$metadata$compression)
  spec$schema_version <- target_version
  result <- write_partitioned_eye_storage(data, target_path, spec, overwrite = overwrite)
  result$transactions <- rbind(result$transactions, data.frame(transaction_id = paste0("tx-migrate-", .as_hash(list(storage$path, target_path))), action = "migrate", timestamp_utc = .as_now(), tables = paste(tables, collapse = ","), partitions = nrow(result$partitions), rows = sum(result$partitions$rows), status = "committed", stringsAsFactors = FALSE))
  utils::write.csv(result$transactions, .as_storage_transactions(result$path), row.names = FALSE)
  result
}

#' Benchmark storage formats and query operations
#'
#' @param x Named list of tables or eye dataset.
#' @param formats Formats to benchmark.
#' @param partition_by Partition columns.
#' @param repetitions Repetitions.
#' @param directory Parent temporary directory.
#' @return An `eye_storage_benchmark` data frame.
#' @export
benchmark_eye_storage <- function(x, formats = c("rds", "csv", "parquet"), partition_by = c("participant_id", "recording_id"), repetitions = 3L, directory = tempdir()) {
  repetitions <- suppressWarnings(as.integer(repetitions))
  if (length(repetitions) != 1L || is.na(repetitions) || repetitions < 1L) .as_stop("`repetitions` must be a positive integer.")
  formats <- unique(as.character(formats))
  invalid_formats <- setdiff(formats, c("rds", "csv", "parquet"))
  if (length(invalid_formats)) .as_stop(sprintf("Unknown storage formats: %s.", paste(invalid_formats, collapse = ", ")))
  formats <- formats[formats != "parquet" | requireNamespace("arrow", quietly = TRUE)]
  if (!length(formats)) .as_stop("No requested storage format is available.")
  rows <- list(); index <- 0L
  for (format in formats) for (replication in seq_len(repetitions)) {
    path <- file.path(directory, paste0("eye-storage-benchmark-", format, "-", replication, "-", sample.int(1e8, 1L)))
    write_time <- system.time(storage <- write_partitioned_eye_storage(x, path, partition_eye_storage(by = partition_by, format = format), overwrite = TRUE))["elapsed"]
    read_time <- system.time(invisible(lapply(unique(storage$partitions$table), function(table) query_eye_storage(storage, table))))["elapsed"]
    index <- index + 1L
    rows[[index]] <- data.frame(format = format, replication = replication, write_seconds = unname(write_time), read_seconds = unname(read_time), bytes = sum(storage$partitions$bytes), partitions = nrow(storage$partitions), rows = sum(storage$partitions$rows), stringsAsFactors = FALSE)
    unlink(path, recursive = TRUE, force = TRUE)
  }
  out <- do.call(rbind, rows)
  class(out) <- c("eye_storage_benchmark", "data.frame")
  out
}

#' @export
plot.eye_storage_benchmark <- function(x, metric = c("write_seconds", "read_seconds", "bytes"), ...) {
  metric <- match.arg(metric)
  if (requireNamespace("ggplot2", quietly = TRUE)) return(ggplot2::ggplot(x, ggplot2::aes_string(x = "format", y = metric)) + ggplot2::geom_boxplot() + ggplot2::theme_minimal() + ggplot2::labs(x = NULL, y = metric))
  graphics::boxplot(x[[metric]] ~ x$format, xlab = "Format", ylab = metric, ...)
  invisible(x)
}

# External model adapters ------------------------------------------------------

.as_engine_registry <- data.frame(
  engine = c("mirt", "TAM", "brms", "LNIRT", "GDINA", "OpenMx", "diffIRT", "TraMineR", "seqHMM", "eyetrackingR", "PupillometryR"),
  package = c("mirt", "TAM", "brms", "LNIRT", "GDINA", "OpenMx", "diffIRT", "TraMineR", "seqHMM", "eyetrackingR", "PupillometryR"),
  domain = c("IRT", "IRT", "Bayesian multilevel", "joint accuracy-RT", "diagnostic classification", "SEM", "diffusion IRT", "sequence analysis", "hidden Markov", "eye-tracking", "pupillometry"),
  stringsAsFactors = FALSE
)

#' List external engine adapters
#'
#' @return Adapter registry with availability.
#' @export
external_model_engines <- function() {
  out <- .as_engine_registry
  out$available <- vapply(out$package, requireNamespace, logical(1), quietly = TRUE)
  out
}

#' Report an adapter's availability and contract
#'
#' @param engine Engine name.
#' @return Adapter status.
#' @export
engine_adapter_status <- function(engine) {
  engine <- as.character(engine)
  if (length(engine) != 1L || is.na(engine) || !nzchar(trimws(engine))) .as_stop("`engine` must be one non-empty name.")
  row <- .as_engine_registry[tolower(.as_engine_registry$engine) == tolower(engine), , drop = FALSE]
  if (!nrow(row)) .as_stop(sprintf("Unknown engine `%s`.", engine))
  row$available <- requireNamespace(row$package, quietly = TRUE)
  row$contract <- "returns fitted, not_available, or failed without silently selecting a model"
  row
}

.as_not_available <- function(engine, purpose, message = NULL) {
  out <- list(status = "not_available", engine = engine, purpose = purpose, message = .as_or(message, sprintf("Optional package `%s` is not installed.", engine)), timestamp_utc = .as_now())
  class(out) <- c("eye_engine_not_available", "eye_engine_adapter_result")
  out
}

.as_adapter_result <- function(engine, purpose, fit, call, data_signature, diagnostics = NULL) {
  out <- list(status = "fitted", engine = engine, purpose = purpose, fit = fit, call = call, data_signature = data_signature, diagnostics = diagnostics, timestamp_utc = .as_now())
  class(out) <- c("eye_engine_adapter_result", paste0("eye_", tolower(engine), "_adapter"))
  out
}

#' @export
print.eye_engine_adapter_result <- function(x, ...) {
  cat("eyeprocess external-engine adapter\n")
  cat("Engine:  ", x$engine, "\n", sep = "")
  cat("Status:  ", x$status, "\n", sep = "")
  cat("Purpose: ", x$purpose, "\n", sep = "")
  if (!is.null(x$message)) cat("Message: ", x$message, "\n", sep = "")
  invisible(x)
}

#' Fit an external model engine through a stable adapter
#'
#' @param engine Engine name.
#' @param data Engine-ready data.
#' @param specification Engine-specific specification.
#' @param purpose Declared scientific purpose.
#' @param ... Engine arguments.
#' @return An adapter result.
#' @export
fit_external_engine <- function(engine, data, specification = NULL, purpose, ...) {
  purpose <- as.character(purpose)
  if (length(purpose) != 1L || is.na(purpose) || !nzchar(trimws(purpose))) .as_stop("A non-empty declared scientific `purpose` is required.")
  status <- engine_adapter_status(engine)
  engine <- status$engine[1L]
  package <- status$package[1L]
  if (!status$available[1L]) return(.as_not_available(engine, purpose))
  call <- match.call()
  signature <- .as_hash(list(dim = dim(data), names = names(data), specification = specification))
  fit <- tryCatch({
    switch(tolower(engine),
      mirt = mirt::mirt(data = data, model = .as_or(specification, 1L), ...),
      tam = TAM::tam.mml(resp = data, ...),
      brms = brms::brm(formula = specification, data = data, ...),
      lnirt = do.call(LNIRT::LNIRT, c(list(Data = data), list(...))),
      gdina = GDINA::GDINA(dat = data, Q = specification, ...),
      openmx = OpenMx::mxRun(specification, ...),
      diffirt = do.call(get("diffIRT", envir = asNamespace("diffIRT")), c(list(data), list(...))),
      traminer = TraMineR::seqdef(data, ...),
      seqhmm = do.call(seqHMM::build_hmm, c(list(observations = data), list(...))),
      eyetrackingr = eyetrackingR::make_eyetrackingr_data(data, ...),
      pupillometryr = do.call(get("make_pupillometryr_data", envir = asNamespace("PupillometryR")), c(list(data), list(...))),
      .as_stop(sprintf("Adapter implementation is unavailable for `%s`.", engine))
    )
  }, error = function(e) e)
  if (inherits(fit, "error")) {
    out <- list(status = "failed", engine = engine, purpose = purpose, error = conditionMessage(fit), call = call, data_signature = signature, timestamp_utc = .as_now())
    class(out) <- c("eye_engine_adapter_failure", "eye_engine_adapter_result")
    return(out)
  }
  .as_adapter_result(engine, purpose, fit, call, signature)
}

#' Validate an external-engine adapter contract
#'
#' @param result Adapter result.
#' @param require_fit Require fitted status.
#' @return Contract findings.
#' @export
validate_engine_adapter <- function(result, require_fit = FALSE) {
  if (!inherits(result, "eye_engine_adapter_result")) .as_stop("Expected an engine-adapter result.")
  scalar_text <- function(value) length(value) == 1L && !is.na(value) && nzchar(as.character(value))
  findings <- data.frame(
    check = c("status", "engine", "purpose", "timestamp", "fit_when_required"),
    passed = c(length(result$status) == 1L && result$status %in% c("fitted", "not_available", "failed"), scalar_text(result$engine), scalar_text(result$purpose), scalar_text(result$timestamp_utc), !require_fit || identical(result$status, "fitted")),
    stringsAsFactors = FALSE
  )
  list(valid = all(findings$passed), findings = findings, engine = result$engine, status = result$status)
}

#' Compare multiple external-engine adapter results
#'
#' @param ... Adapter results or a list.
#' @return Comparison data frame.
#' @export
compare_engine_adapters <- function(...) {
  results <- list(...)
  if (length(results) == 1L && is.list(results[[1L]]) && !inherits(results[[1L]], "eye_engine_adapter_result")) results <- results[[1L]]
  if (!length(results)) .as_stop("At least one adapter result is required.")
  rows <- lapply(results, function(x) {
    if (!inherits(x, "eye_engine_adapter_result")) .as_stop("All results must be engine-adapter objects.")
    data.frame(engine = x$engine, status = x$status, purpose = x$purpose, data_signature = .as_or(x$data_signature, NA_character_), error = .as_or(x$error, .as_or(x$message, "")), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

# Convenience wrappers preserve declared purpose and avoid autonomous selection.
#' @export
fit_mirt_adapter <- function(data, model = 1L, purpose, ...) fit_external_engine("mirt", data, model, purpose, ...)
#' @export
fit_tam_adapter <- function(data, purpose, ...) fit_external_engine("TAM", data, NULL, purpose, ...)
#' @export
fit_brms_adapter <- function(formula, data, purpose, ...) fit_external_engine("brms", data, formula, purpose, ...)
#' @export
fit_lnirt_adapter <- function(data, purpose, ...) fit_external_engine("LNIRT", data, NULL, purpose, ...)
#' @export
fit_traminer_adapter <- function(data, purpose, ...) fit_external_engine("TraMineR", data, NULL, purpose, ...)
#' @export
fit_seqhmm_adapter <- function(data, purpose, ...) fit_external_engine("seqHMM", data, NULL, purpose, ...)

#' Fit a GDINA adapter with backward-compatible eye-dataset support
#'
#' @param data An `eye_dataset` or response data accepted by `GDINA::GDINA()`.
#' @param Q Q-matrix with one row per item.
#' @param model GDINA model specification used for eye-dataset inputs.
#' @param purpose Declared scientific purpose for the external-engine contract.
#' @param ... Additional arguments passed to the selected adapter.
#' @return An `eyeprocess_model` for eye-dataset inputs or an
#'   `eye_engine_adapter_result` for raw response data.
#' @export
fit_gdina_adapter <- function(data, Q, model = "GDINA", purpose = "cognitive diagnosis", ...) {
  if (inherits(data, "eye_dataset")) {
    .assert_eye_dataset(data)
    responses <- response_matrix(data)
    Q <- as.matrix(Q)
    if (nrow(Q) != ncol(responses)) .eye_stop("The Q-matrix must contain one row per response-matrix item.")
    .require_namespace("GDINA", "for cognitive-diagnosis models")
    fit <- GDINA::GDINA(dat = responses, Q = Q, model = model, ...)
    return(.new_eyeprocess_model(fit, "GDINA", "cognitive_diagnosis", as.data.frame(responses), match.call(), list(q_matrix = Q, model = model), experimental = TRUE))
  }
  fit_external_engine("GDINA", data, Q, purpose, model = model, ...)
}
#' @export
fit_openmx_adapter <- function(model, purpose, ...) fit_external_engine("OpenMx", data = NULL, specification = model, purpose = purpose, ...)
#' @export
fit_diffirt_engine_adapter <- function(data, purpose, ...) fit_external_engine("diffIRT", data, NULL, purpose, ...)
#' @export
fit_eyetrackingr_adapter <- function(data, purpose, ...) fit_external_engine("eyetrackingR", data, NULL, purpose, ...)
#' @export
fit_pupillometryr_adapter <- function(data, purpose, ...) fit_external_engine("PupillometryR", data, NULL, purpose, ...)
