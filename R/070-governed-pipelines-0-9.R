# eyeprocess 0.9.0.9000 ------------------------------------------------------
# Governed end-to-end pipelines. Substantive decisions remain explicit.

#' Define explicit analysis decisions for an eyeprocess workflow
#'
#' @param blink_correction Named blink-correction rule or `NULL`.
#' @param pupil_baseline Numeric baseline window or `NULL`.
#' @param fixation_algorithm Named fixation algorithm or `NULL`.
#' @param aoi_rule Named AOI assignment rule or `NULL`.
#' @param exclusions Explicit exclusion specification or `NULL`.
#' @param model Explicit model specification or `NULL`.
#' @param sensitivity Sensitivity specification or `NULL`.
#' @param ... Additional named decisions.
#' @return An `eye_analysis_spec` object.
#' @export
eye_analysis_spec <- function(
    blink_correction = NULL,
    pupil_baseline = NULL,
    fixation_algorithm = NULL,
    aoi_rule = NULL,
    exclusions = NULL,
    model = NULL,
    sensitivity = NULL,
    ...) {
  extra <- list(...)
  if (length(extra) && (is.null(names(extra)) || any(!nzchar(names(extra)))))
    stop("Additional analysis decisions supplied through ... must be named.", call. = FALSE)
  x <- c(list(
    blink_correction = blink_correction,
    pupil_baseline = pupil_baseline,
    fixation_algorithm = fixation_algorithm,
    aoi_rule = aoi_rule,
    exclusions = exclusions,
    model = model,
    sensitivity = sensitivity
  ), extra)
  structure(list(
    decisions = x,
    created_at = as.character(Sys.time()),
    hash = .ep09_hash_object(x),
    status = "explicit_analysis_spec",
    caveat = "The specification records user-selected decisions; eyeprocess does not infer that these choices are substantively optimal."
  ), class = "eye_analysis_spec")
}

#' Define a governed pipeline step
#'
#' @param name Unique step name.
#' @param fun Function executed by the step.
#' @param requires Names of upstream steps.
#' @param optional Whether an error may be retained without stopping the pipeline.
#' @param description Human-readable description.
#' @param decision Optional decision label linking the step to an analysis specification.
#' @return An `eye_pipeline_step` object.
#' @export
eye_pipeline_step <- function(name, fun, requires = character(), optional = FALSE,
                              description = NULL, decision = NULL) {
  name <- as.character(name)[1L]
  if (is.na(name) || !nzchar(name) || make.names(name) != name) stop("name must be a non-empty syntactic R name.", call. = FALSE)
  if (!is.function(fun)) stop("fun must be a function.", call. = FALSE)
  requires <- unique(as.character(requires))
  if (anyNA(requires) || any(!nzchar(requires))) stop("requires must contain non-empty step names.", call. = FALSE)
  if (name %in% requires) stop("A step cannot require itself.", call. = FALSE)
  structure(list(
    name = name,
    fun = fun,
    requires = requires,
    optional = isTRUE(optional),
    description = if (is.null(description)) "" else as.character(description)[1L],
    decision = if (is.null(decision)) NA_character_ else as.character(decision)[1L],
    function_hash = .ep09_hash_object(body(fun))
  ), class = "eye_pipeline_step")
}

#' Construct a governed analysis pipeline
#'
#' @param ... `eye_pipeline_step` objects.
#' @param spec Optional `eye_analysis_spec`.
#' @param name Pipeline label.
#' @param strict If `TRUE`, undeclared dependencies are errors.
#' @return An object of class "eye_analysis_pipeline", stored as a named list, with components "name", "steps", "spec", "strict", "created_at", "status". It contains a governed analysis pipeline and associated metadata or diagnostics needed to interpret the result.
#' @export
eye_analysis_pipeline <- function(..., spec = eye_analysis_spec(), name = "eye_analysis", strict = TRUE) {
  steps <- list(...)
  if (length(steps) == 1L && is.list(steps[[1L]]) && !inherits(steps[[1L]], "eye_pipeline_step")) steps <- steps[[1L]]
  if (!length(steps)) stop("At least one eye_pipeline_step is required.", call. = FALSE)
  if (!all(vapply(steps, inherits, logical(1), what = "eye_pipeline_step"))) stop("All pipeline entries must be eye_pipeline_step objects.", call. = FALSE)
  nms <- vapply(steps, `[[`, character(1), "name")
  if (anyDuplicated(nms)) stop("Pipeline step names must be unique.", call. = FALSE)
  names(steps) <- nms
  x <- structure(list(
    name = as.character(name)[1L],
    steps = steps,
    spec = spec,
    strict = isTRUE(strict),
    created_at = as.character(Sys.time()),
    status = "governed_pipeline"
  ), class = "eye_analysis_pipeline")
  validate_eye_pipeline(x)
  x$hash <- .ep09_hash_object(eye_pipeline_manifest(x))
  x
}

.ep09_toposort <- function(steps) {
  nms <- names(steps)
  deps <- lapply(steps, `[[`, "requires")
  unknown <- setdiff(unique(unlist(deps, use.names = FALSE)), nms)
  if (length(unknown)) stop("Unknown pipeline dependency/dependencies: ", paste(unknown, collapse = ", "), call. = FALSE)
  incoming <- vapply(nms, function(nm) length(deps[[nm]]), integer(1))
  ready <- nms[incoming == 0L]
  order <- character()
  while (length(ready)) {
    cur <- ready[[1L]]; ready <- ready[-1L]; order <- c(order, cur)
    children <- nms[vapply(deps, function(z) cur %in% z, logical(1))]
    for (ch in children) {
      incoming[[ch]] <- incoming[[ch]] - 1L
      if (incoming[[ch]] == 0L && !ch %in% c(order, ready)) ready <- c(ready, ch)
    }
  }
  if (length(order) != length(nms)) stop("Pipeline dependency graph contains a cycle.", call. = FALSE)
  order
}

#' Validate a governed eyeprocess pipeline
#' @param x Pipeline.
#' @return A logical value or vector indicating a governed eyeprocess pipeline.
#' @export
validate_eye_pipeline <- function(x) {
  if (!inherits(x, "eye_analysis_pipeline")) stop("x must be an eye_analysis_pipeline.", call. = FALSE)
  if (!length(x$steps)) stop("Pipeline has no steps.", call. = FALSE)
  .ep09_toposort(x$steps)
  if (!inherits(x$spec, "eye_analysis_spec")) stop("Pipeline spec must be an eye_analysis_spec.", call. = FALSE)
  if (isTRUE(x$strict)) {
    step_names <- names(x$steps)
    special <- c("context", "spec", ".context", ".spec", "...")
    undeclared <- lapply(x$steps, function(step) {
      fml <- setdiff(names(formals(step$fun)), special)
      candidates <- intersect(fml, step_names)
      setdiff(candidates, step$requires)
    })
    bad <- names(undeclared)[lengths(undeclared) > 0L]
    if (length(bad)) {
      detail <- paste(vapply(bad, function(nm) paste0(nm, " -> ", paste(undeclared[[nm]], collapse = ", ")), character(1)), collapse = "; ")
      stop("Strict pipeline contains formal step dependencies not declared in `requires`: ", detail, ".", call. = FALSE)
    }
  }
  invisible(TRUE)
}

#' Return pipeline vertices and dependency edges
#' @param x Pipeline.
#' @return A named list with components "vertices", "edges", containing return pipeline vertices and dependency edges and associated metadata or diagnostics.
#' @export
eye_pipeline_graph <- function(x) {
  validate_eye_pipeline(x)
  vertices <- data.frame(
    step = names(x$steps),
    optional = vapply(x$steps, `[[`, logical(1), "optional"),
    decision = vapply(x$steps, function(z) ifelse(is.na(z$decision), "", z$decision), character(1)),
    order = match(names(x$steps), .ep09_toposort(x$steps)),
    stringsAsFactors = FALSE
  )
  edges <- .ep09_rbind_fill(lapply(x$steps, function(z) {
    if (!length(z$requires)) return(NULL)
    data.frame(from = z$requires, to = z$name, stringsAsFactors = FALSE)
  }))
  list(vertices = vertices[order(vertices$order), , drop = FALSE], edges = edges)
}

#' Machine-readable pipeline manifest
#' @param x Pipeline.
#' @return A data frame containing machine-readable pipeline manifest. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
eye_pipeline_manifest <- function(x) {
  validate_eye_pipeline(x)
  g <- eye_pipeline_graph(x)
  data.frame(
    pipeline = x$name,
    step = names(x$steps),
    requires = vapply(x$steps, function(z) paste(z$requires, collapse = ";"), character(1)),
    optional = vapply(x$steps, `[[`, logical(1), "optional"),
    decision = vapply(x$steps, function(z) ifelse(is.na(z$decision), "", z$decision), character(1)),
    description = vapply(x$steps, `[[`, character(1), "description"),
    function_hash = vapply(x$steps, `[[`, character(1), "function_hash"),
    execution_order = match(names(x$steps), g$vertices$step),
    stringsAsFactors = FALSE
  )
}

.ep09_call_pipeline_step <- function(step, outputs, context, spec) {
  deps <- step$requires
  args <- if (length(deps)) outputs[deps] else list()
  fml <- names(formals(step$fun))
  has_dots <- "..." %in% fml
  # Support both explicit user-facing names (spec/context) and dot-prefixed
  # names used by lower-level pipeline closures. Only declared aliases are
  # passed unless the function accepts ..., preventing accidental argument
  # injection into ordinary analysis functions.
  special <- list(spec = spec, context = context, .spec = spec, .context = context)
  if (has_dots) return(do.call(step$fun, c(args, special)))
  candidates <- c(args, special)
  use <- candidates[intersect(names(candidates), fml)]
  do.call(step$fun, use)
}

#' Run a governed eyeprocess pipeline
#'
#' @param x Pipeline.
#' @param context Initial named context available as `.context`.
#' @param stop_on_error Stop on a non-optional step error.
#' @param previous Optional prior `eye_pipeline_run` used for resumption.
#' @return An object of class "eye_pipeline_run", stored as a named list, with components "pipeline", "pipeline_hash", "outputs", "records", "errors", "warnings", "context_hash", "completed", "created_at", "status". It contains a governed eyeprocess pipeline and associated metadata or diagnostics needed to interpret the result.
#' @export
run_eye_pipeline <- function(x, context = list(), stop_on_error = TRUE, previous = NULL) {
  validate_eye_pipeline(x)
  if (!is.list(context)) stop("context must be a list.", call. = FALSE)
  order <- .ep09_toposort(x$steps)
  outputs <- list(); records <- list(); errors <- list(); warnings <- list()
  context_hash <- .ep09_hash_object(context)
  if (!is.null(previous)) {
    if (!inherits(previous, "eye_pipeline_run")) stop("previous must be an eye_pipeline_run.", call. = FALSE)
    if (!identical(previous$pipeline_hash, x$hash)) stop("previous run was generated from a different pipeline manifest.", call. = FALSE)
    if (!identical(previous$context_hash, context_hash)) stop("previous run used a different context; rerun from the beginning rather than reusing context-dependent outputs.", call. = FALSE)
    outputs <- previous$outputs
    records <- if (nrow(previous$records)) split(previous$records, previous$records$step) else list()
    errors <- previous$errors
    warnings <- previous$warnings
  }
  for (nm in order) {
    if (nm %in% names(outputs)) next
    step <- x$steps[[nm]]
    upstream_missing <- setdiff(step$requires, names(outputs))
    if (length(upstream_missing)) {
      msg <- paste("Required upstream outputs unavailable:", paste(upstream_missing, collapse = ", "))
      errors[[nm]] <- msg
      if (!step$optional && isTRUE(stop_on_error)) stop("Step '", nm, "' cannot run. ", msg, call. = FALSE)
      next
    }
    # A previously failed step may be retried during resumption. Clear stale
    # diagnostics before recording the new attempt.
    errors[[nm]] <- NULL
    warnings[[nm]] <- NULL
    started <- Sys.time()
    cap <- .ep09_capture(.ep09_call_pipeline_step(step, outputs, context, x$spec))
    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (length(cap$warnings)) warnings[[nm]] <- cap$warnings
    ok <- is.na(cap$error)
    records[[nm]] <- data.frame(
      step = nm,
      status = if (ok) "success" else if (step$optional) "optional_error" else "error",
      optional = step$optional,
      elapsed_sec = elapsed,
      error = if (ok) NA_character_ else cap$error,
      output_hash = if (ok) .ep09_hash_object(cap$value) else NA_character_,
      stringsAsFactors = FALSE
    )
    if (ok) {
      outputs[[nm]] <- cap$value
    } else {
      errors[[nm]] <- cap$error
      if (!step$optional && isTRUE(stop_on_error)) stop("Pipeline step '", nm, "' failed: ", cap$error, call. = FALSE)
    }
  }
  rec <- .ep09_rbind_fill(records)
  completed <- all(order %in% names(outputs) | vapply(x$steps[order], `[[`, logical(1), "optional"))
  structure(list(
    pipeline = x,
    pipeline_hash = x$hash,
    outputs = outputs,
    records = rec,
    errors = errors,
    warnings = warnings,
    context_hash = context_hash,
    completed = completed,
    created_at = as.character(Sys.time()),
    status = if (completed) "pipeline_complete" else "pipeline_incomplete"
  ), class = "eye_pipeline_run")
}

#' Resume a governed pipeline from a prior run
#' @param x Pipeline.
#' @param previous Prior pipeline run.
#' @param context Context used for new steps.
#' @param stop_on_error Stop on non-optional error.
#' @return An object of class "eye_pipeline_run", stored as a named list, with components "pipeline", "pipeline_hash", "outputs", "records", "errors", "warnings", "context_hash", "completed", "created_at", "status". It contains resume a governed pipeline from a prior run and associated metadata or diagnostics needed to interpret the result.
#' @export
resume_eye_pipeline <- function(x, previous, context = list(), stop_on_error = TRUE) {
  run_eye_pipeline(x, context = context, stop_on_error = stop_on_error, previous = previous)
}

#' Audit a pipeline definition or completed run
#' @param x Pipeline or pipeline run.
#' @return An object of class "eye_pipeline_audit", stored as a named list, with components "table", "undeclared_decisions", "valid", "pipeline_hash". It contains a pipeline definition or completed run and associated metadata or diagnostics needed to interpret the result.
#' @export
audit_eye_pipeline <- function(x) {
  pipeline <- if (inherits(x, "eye_pipeline_run")) x$pipeline else x
  validate_eye_pipeline(pipeline)
  man <- eye_pipeline_manifest(pipeline)
  decision_names <- names(pipeline$spec$decisions)
  man$decision_declared <- man$decision == "" | man$decision %in% decision_names
  man$has_description <- nzchar(man$description)
  if (inherits(x, "eye_pipeline_run")) {
    status <- pipeline_step_status(x)
    man <- merge(man, status, by = "step", all.x = TRUE, sort = FALSE)
  }
  structure(list(
    table = man,
    undeclared_decisions = unique(man$decision[nzchar(man$decision) & !man$decision_declared]),
    valid = all(man$decision_declared),
    pipeline_hash = pipeline$hash
  ), class = "eye_pipeline_audit")
}

#' Pipeline step status table
#' @param x Pipeline run.
#' @return A data frame containing pipeline step status table. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
pipeline_step_status <- function(x) {
  if (!inherits(x, "eye_pipeline_run")) stop("x must be an eye_pipeline_run.", call. = FALSE)
  all_steps <- names(x$pipeline$steps)
  rec <- x$records
  out <- data.frame(step = all_steps, stringsAsFactors = FALSE)
  if (nrow(rec)) out <- merge(out, rec, by = "step", all.x = TRUE, sort = FALSE)
  out$status[is.na(out$status)] <- "not_run"
  out
}

#' Extract a pipeline result by step name
#' @param x Pipeline run.
#' @param step Step name.
#' @return An R object containing a pipeline result by step name. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
pipeline_result <- function(x, step) {
  if (!inherits(x, "eye_pipeline_run")) stop("x must be an eye_pipeline_run.", call. = FALSE)
  step <- as.character(step)[1L]
  if (!step %in% names(x$outputs)) stop("No successful output is available for step '", step, "'.", call. = FALSE)
  x$outputs[[step]]
}

#' Return failed pipeline steps
#' @param x Pipeline run.
#' @return A data frame containing return failed pipeline steps. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
pipeline_failures <- function(x) {
  s <- pipeline_step_status(x)
  s[s$status %in% c("error", "optional_error"), , drop = FALSE]
}

#' Write a conservative pipeline report
#' @param x Pipeline or run.
#' @param path Output text/markdown path.
#' @return A character string or vector giving the path or identifier for a conservative pipeline report.
#' @export
write_eye_pipeline_report <- function(x, path) {
  pipeline <- if (inherits(x, "eye_pipeline_run")) x$pipeline else x
  audit <- audit_eye_pipeline(x)
  lines <- c(
    paste0("# eyeprocess pipeline report: ", pipeline$name), "",
    paste0("Pipeline hash: `", pipeline$hash, "`"),
    paste0("Analysis-spec hash: `", pipeline$spec$hash, "`"), "",
    "## Governance", "",
    paste0("- Steps: ", nrow(audit$table)),
    paste0("- All step-linked decisions declared: ", audit$valid),
    "- The report records execution and provenance; it does not certify substantive model adequacy."
  )
  if (inherits(x, "eye_pipeline_run")) {
    s <- pipeline_step_status(x)
    lines <- c(lines, "", "## Execution", "",
               paste0("- Completed: ", x$completed),
               paste0("- Successful steps: ", sum(s$status == "success")),
               paste0("- Error/optional-error steps: ", sum(s$status %in% c("error", "optional_error"))))
  }
  writeLines(lines, path, useBytes = TRUE)
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}

#' Export a pipeline manifest and optional run status
#' @param x Pipeline or run.
#' @param path CSV path.
#' @return An R object containing a pipeline manifest and optional run status. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
export_eye_pipeline <- function(x, path) {
  pipeline <- if (inherits(x, "eye_pipeline_run")) x$pipeline else x
  tab <- eye_pipeline_manifest(pipeline)
  if (inherits(x, "eye_pipeline_run")) tab <- merge(tab, pipeline_step_status(x), by = "step", all.x = TRUE, sort = FALSE)
  utils::write.csv(tab, path, row.names = FALSE, na = "")
  invisible(path)
}

#' Render pipeline dependencies as Graphviz DOT
#' @param x Pipeline.
#' @return A character value or vector containing render pipeline dependencies as Graphviz DOT.
#' @export
eye_pipeline_dot <- function(x) {
  g <- eye_pipeline_graph(x)
  node_lines <- paste0('  "', g$vertices$step, '";')
  edge_lines <- if (nrow(g$edges)) paste0('  "', g$edges$from, '" -> "', g$edges$to, '";') else character()
  paste(c("digraph eyeprocess_pipeline {", node_lines, edge_lines, "}"), collapse = "\n")
}

#' Render pipeline dependencies as Mermaid flowchart text
#' @param x Pipeline.
#' @return A character value or vector containing render pipeline dependencies as Mermaid flowchart text.
#' @export
eye_pipeline_mermaid <- function(x) {
  g <- eye_pipeline_graph(x)
  edge_lines <- if (nrow(g$edges)) paste0("  ", g$edges$from, " --> ", g$edges$to) else paste0("  ", g$vertices$step)
  paste(c("flowchart TD", edge_lines), collapse = "\n")
}

#' Create a targets-compatible dependency manifest
#'
#' This function does not execute or silently translate arbitrary closures into
#' a targets pipeline. It provides the dependency contract required to build an
#' explicit `_targets.R` file.
#' @param x Pipeline.
#' @return A data frame containing a targets-compatible dependency manifest. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
eye_targets_manifest <- function(x) {
  man <- eye_pipeline_manifest(x)
  data.frame(target = man$step, dependencies = man$requires,
             function_hash = man$function_hash, stringsAsFactors = FALSE)
}

#' Write an explicit `_targets.R` template from a governed pipeline
#' @param x Pipeline.
#' @param path Output path.
#' @return An R object containing an explicit `_targets.R` template from a governed pipeline. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
write_eye_targets_template <- function(x, path = "_targets.R") {
  man <- eye_targets_manifest(x)
  lines <- c(
    "# Generated by eyeprocess::write_eye_targets_template()",
    "# Review every command before execution; substantive decisions are not inferred.",
    "library(targets)", "library(eyeprocess)", "", "list("
  )
  target_lines <- vapply(seq_len(nrow(man)), function(i) {
    deps <- strsplit(man$dependencies[[i]], ";", fixed = TRUE)[[1L]]
    deps <- deps[nzchar(deps)]
    dep_note <- if (length(deps)) paste(deps, collapse = ", ") else "no upstream targets"
    paste0("  # ", man$target[[i]], " requires: ", dep_note, "\n",
           "  tar_target(", man$target[[i]], ", stop(\"Replace with explicit eyeprocess step call\"))")
  }, character(1))
  lines <- c(lines, paste(target_lines, collapse = ",\n"), ")")
  writeLines(lines, path, useBytes = TRUE)
  invisible(path)
}

#' @export
print.eye_analysis_spec <- function(x, ...) {
  cat("eyeprocess explicit analysis specification\n")
  cat("  decisions:", length(x$decisions), "\n")
  cat("  hash     :", x$hash, "\n")
  invisible(x)
}

#' @export
print.eye_analysis_pipeline <- function(x, ...) {
  cat("eyeprocess governed analysis pipeline\n")
  cat("  name :", x$name, "\n")
  cat("  steps:", length(x$steps), "\n")
  cat("  hash :", x$hash, "\n")
  invisible(x)
}

#' @export
print.eye_pipeline_run <- function(x, ...) {
  cat("eyeprocess pipeline run\n")
  cat("  pipeline :", x$pipeline$name, "\n")
  cat("  completed:", x$completed, "\n")
  cat("  outputs  :", length(x$outputs), "\n")
  invisible(x)
}
