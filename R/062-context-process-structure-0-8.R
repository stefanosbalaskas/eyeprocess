# eyeprocess 0.8.0.9000 ------------------------------------------------------
# Visual-context/testlet measurement, multiblock exploratory structure,
# process-profile discovery, external validity, and pre-pilot item seeding.

#' Build an item-to-visual-context registry
#'
#' @param item_metadata Item-level metadata.
#' @param item Item identifier column.
#' @param context Optional explicit context column.
#' @param context_candidates Candidate metadata columns searched in order.
#' @param min_items_per_context Minimum items required for a shared context.
#' @return An object of class "eye_visual_context_registry", stored as a named list, with components "mapping", "source_item_column", "source_context_column", "min_items_per_context", "caveat". It contains an item-to-visual-context registry and associated metadata or diagnostics needed to interpret the result.
#' @export
visual_context_registry <- function(
    item_metadata, item = "item_id", context = NULL,
    context_candidates = c("visual_anchor_id", "stimulus_id", "stimulus_page", "page_id",
                           "layout_id", "screen_id", "diagram_id"),
    min_items_per_context = 3L) {
  item_metadata <- .ep08_as_df(item_metadata, "item_metadata")
  .ep08_req_cols(item_metadata, item, "item_metadata")
  min_items_per_context <- as.integer(min_items_per_context)
  if (min_items_per_context < 2L) stop("min_items_per_context must be at least 2.", call. = FALSE)
  if (anyDuplicated(as.character(item_metadata[[item]])))
    stop("item_metadata must contain one row per item for visual-context registration.", call. = FALSE)
  if (is.null(context)) {
    found <- context_candidates[context_candidates %in% names(item_metadata)]
    if (!length(found)) stop("No visual-context column found; supply `context` explicitly.", call. = FALSE)
    context <- found[1L]
  }
  .ep08_req_cols(item_metadata, context, "item_metadata")
  tab <- data.frame(item_id = as.character(item_metadata[[item]]),
                    visual_context_id = as.character(item_metadata[[context]]),
                    stringsAsFactors = FALSE)
  tab$visual_context_id[is.na(tab$visual_context_id) | !nzchar(tab$visual_context_id)] <-
    paste0("unique_context__", tab$item_id[is.na(tab$visual_context_id) | !nzchar(tab$visual_context_id)])
  counts <- as.data.frame(table(tab$visual_context_id), stringsAsFactors = FALSE)
  names(counts) <- c("visual_context_id", "n_items")
  tab <- merge(tab, counts, by = "visual_context_id", all.x = TRUE, sort = FALSE)
  tab$shared_context <- tab$n_items >= as.integer(min_items_per_context)
  structure(list(
    mapping = tab[, c("item_id", "visual_context_id", "n_items", "shared_context")],
    source_item_column = item, source_context_column = context,
    min_items_per_context = as.integer(min_items_per_context),
    caveat = "Visual-context factors represent shared presentation context unless substantive theory justifies another interpretation."
  ), class = "eye_visual_context_registry")
}

#' Print a visual context registry object
#' @return Invisibly returns the input object after printing its summary; the object's class and contents are unchanged.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param ... Additional arguments passed to the underlying method or helper.
print.eye_visual_context_registry <- function(x, ...) {
  cat("<eye_visual_context_registry>\n")
  cat(" items:", nrow(x$mapping), "\n")
  cat(" contexts:", length(unique(x$mapping$visual_context_id)), "\n")
  cat(" shared contexts:", length(unique(x$mapping$visual_context_id[x$mapping$shared_context])), "\n")
  invisible(x)
}

.ep08_context_positions <- function(registry, item_names, selected_context = NULL) {
  map <- registry$mapping
  map$item_position <- match(map$item_id, item_names)
  map <- map[is.finite(map$item_position), , drop = FALSE]
  shared <- unique(map$visual_context_id[map$shared_context])
  if (!length(shared)) stop("No shared visual context has enough items.", call. = FALSE)
  if (is.null(selected_context)) selected_context <- shared[1L]
  if (!selected_context %in% shared) stop("selected context is not a valid shared context.", call. = FALSE)
  sort(unique(map$item_position[map$visual_context_id == selected_context]))
}

#' Fit an explicit visual-context/testlet IRT model
#'
#' @param response_matrix Person x item response matrix.
#' @param registry `visual_context_registry()` object.
#' @param context Optional context identifier to model; defaults to the first shared context.
#' @param itemtype mirt item type.
#' @param model_dimension Name for the primary latent dimension.
#' @param context_dimension Name for the context/testlet dimension.
#' @param SE Request standard errors from mirt.
#' @return An object of class "eye_visual_context_irt", stored as a named list, with components "base_model", "context_model", "comparison", "registry", "context", "positions", "itemtype", "model_string", "status", "caveat". It contains an explicit visual-context/testlet IRT model and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_visual_context_irt <- function(
    response_matrix, registry, context = NULL, itemtype = "2PL",
    model_dimension = "Ability", context_dimension = "VisualContextFactor",
    SE = FALSE) {
  if (!requireNamespace("mirt", quietly = TRUE))
    stop("Package `mirt` is required for fit_visual_context_irt().", call. = FALSE)
  if (!inherits(registry, "eye_visual_context_registry"))
    stop("registry must be created by visual_context_registry().", call. = FALSE)
  X <- as.data.frame(response_matrix)
  if (ncol(X) < 4L) stop("At least four items are required.", call. = FALSE)
  item_names <- colnames(X)
  if (is.null(item_names) || any(!nzchar(item_names))) {
    item_names <- paste0("Item", seq_len(ncol(X))); colnames(X) <- item_names
  }
  pos <- .ep08_context_positions(registry, item_names, context)
  if (length(pos) < 3L || length(pos) >= ncol(X))
    stop("Context factor must include at least three but not all items.", call. = FALSE)
  selected_context <- unique(registry$mapping$visual_context_id[registry$mapping$item_id %in% item_names[pos]])[1L]
  model_string <- paste0(
    model_dimension, " = 1-", ncol(X), "\n",
    context_dimension, " = ", paste(pos, collapse = ",")
  )
  spec <- mirt::mirt.model(model_string)
  base <- mirt::mirt(X, model = 1, itemtype = itemtype, SE = SE, verbose = FALSE)
  contextual <- mirt::mirt(X, model = spec, itemtype = itemtype, SE = SE, verbose = FALSE)
  cmp <- tryCatch(stats::anova(base, contextual), error = function(e) e)
  structure(list(
    base_model = base, context_model = contextual, comparison = cmp,
    registry = registry, context = selected_context, positions = pos,
    itemtype = itemtype, model_string = model_string,
    status = "reference_visual_context_testlet_model",
    caveat = paste(
      "The context factor represents shared screen/page/layout dependence.",
      "Do not interpret it as a substantive second ability without independent theory and validation."
    )
  ), class = "eye_visual_context_irt")
}

#' Compare base and visual-context IRT models
#' @return A data frame containing base and visual-context IRT models. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
#' @param x Object to process, inspect, compare, or plot.
compare_visual_context_irt <- function(x) {
  if (!inherits(x, "eye_visual_context_irt")) stop("x must be eye_visual_context_irt.", call. = FALSE)
  if (inherits(x$comparison, "error")) return(data.frame(status = conditionMessage(x$comparison)))
  as.data.frame(x$comparison)
}

#' Extract visual-context factor effects/loadings
#' @return An R object containing visual-context factor effects/loadings. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param x Object to process, inspect, compare, or plot.
#' @param IRTpars Passed to the underlying IRT coefficient extractor to request IRT parameterization when supported.
context_factor_effects <- function(x, IRTpars = FALSE) {
  if (!inherits(x, "eye_visual_context_irt")) stop("x must be eye_visual_context_irt.", call. = FALSE)
  if (!requireNamespace("mirt", quietly = TRUE)) stop("Package `mirt` is required to extract model coefficients.", call. = FALSE)
  co <- mirt::coef(x$context_model, simplify = TRUE, IRTpars = IRTpars)
  co$items
}

#' Audit visual-context dependence
#' @return A data frame containing visual-context dependence. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
#' @param x Object to process, inspect, compare, or plot.
audit_visual_context_dependence <- function(x) {
  if (!inherits(x, "eye_visual_context_irt")) stop("x must be eye_visual_context_irt.", call. = FALSE)
  map <- x$registry$mapping
  data.frame(
    context = x$context,
    n_context_items = length(x$positions),
    total_items = length(unique(map$item_id)),
    context_fraction = length(x$positions) / length(unique(map$item_id)),
    comparison_available = !inherits(x$comparison, "error"),
    interpretation = "Context factor models known shared presentation dependence; it is not automatically a substantive trait."
  )
}

#' Define conceptual process-feature blocks
#'
#' @param data Data frame.
#' @param blocks Named list of column names for conceptual blocks.
#' @param id Optional identifier column.
#' @param drop_constant Remove non-varying columns.
#' @return An object of class "eye_process_feature_blocks", stored as a named list, with components "data", "blocks", "id", "block_sizes", "status". It contains define conceptual process-feature blocks and associated metadata or diagnostics needed to interpret the result.
#' @export
process_feature_blocks <- function(data, blocks, id = NULL, drop_constant = TRUE) {
  data <- .ep08_as_df(data)
  if (!is.list(blocks) || is.null(names(blocks)) || any(!nzchar(names(blocks))))
    stop("blocks must be a named list of column names.", call. = FALSE)
  flat_vars <- unlist(blocks, use.names = FALSE)
  if (anyDuplicated(flat_vars)) stop("Each feature must belong to only one block for this multiblock map.", call. = FALSE)
  all_vars <- unique(flat_vars)
  required <- c(if (!is.null(id)) id else character(), all_vars)
  .ep08_req_cols(data, required)
  clean <- lapply(blocks, function(vars) {
    vars <- intersect(vars, names(data))
    if (isTRUE(drop_constant)) vars <- vars[vapply(data[vars], function(z) {
      sz <- .ep08_sd(z); is.finite(sz) && sz > 0
    }, logical(1))]
    vars
  })
  clean <- clean[lengths(clean) > 0L]
  if (length(clean) < 2L) stop("At least two non-empty process-feature blocks are required.", call. = FALSE)
  structure(list(data = data, blocks = clean, id = id,
                 block_sizes = lengths(clean),
                 status = "conceptual_process_feature_blocks"),
            class = "eye_process_feature_blocks")
}

#' Fit a multiblock psychometric/gaze/pupil/quality structure map
#'
#' Uses FactoMineR MFA when available/requested. A block-standardized PCA fallback
#' is available as a transparent exploratory reference and is explicitly labeled.
#'
#' @param x A `process_feature_blocks()` object or data frame.
#' @param blocks Required if `x` is a data frame.
#' @param id Optional identifier.
#' @param engine `auto`, `FactoMineR`, or `pca_block_scaled`.
#' @param ncp Number of components retained where supported.
#' @return An object of class "eye_multiblock_process_map", stored as a named list, with components "model", "person_coordinates", "variable_coordinates", "block_coordinates", "blocks", "engine", "status", "caveat". It contains a multiblock psychometric/gaze/pupil/quality structure map and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_multiblock_process_map <- function(x, blocks = NULL, id = NULL,
                                       engine = c("auto", "FactoMineR", "pca_block_scaled"), ncp = 5L) {
  engine <- match.arg(engine)
  if (!inherits(x, "eye_process_feature_blocks")) x <- process_feature_blocks(x, blocks = blocks, id = id)
  data <- x$data; blocks <- x$blocks; id <- x$id
  ncp <- as.integer(ncp)
  if (ncp < 1L) stop("ncp must be at least 1.", call. = FALSE)
  if (nrow(data) < 3L) stop("At least three rows are required for multiblock mapping.", call. = FALSE)
  vars <- unlist(blocks, use.names = FALSE)
  df <- data[, vars, drop = FALSE]
  for (v in vars) df[[v]] <- .ep08_num(df[[v]])
  # Conservative mean imputation for exploratory mapping only.
  for (v in vars) {
    mu <- .ep08_mean(df[[v]]); if (!is.finite(mu)) mu <- 0
    df[[v]][!is.finite(df[[v]])] <- mu
  }
  chosen <- engine
  if (engine == "auto") chosen <- if (requireNamespace("FactoMineR", quietly = TRUE)) "FactoMineR" else "pca_block_scaled"
  ids <- if (!is.null(id)) as.character(data[[id]]) else as.character(seq_len(nrow(data)))
  if (chosen == "FactoMineR") {
    if (!requireNamespace("FactoMineR", quietly = TRUE)) stop("Package `FactoMineR` is required.", call. = FALSE)
    ncp_use <- min(ncp, ncol(df), max(1L, nrow(df) - 1L))
    fit <- FactoMineR::MFA(df, group = lengths(blocks), type = rep("s", length(blocks)),
                           name.group = names(blocks), ncp = ncp_use, graph = FALSE)
    person <- as.data.frame(fit$ind$coord); person$id <- ids
    variables <- as.data.frame(fit$quanti.var$coord)
    variables$variable <- rownames(variables)
    block_coord <- if (!is.null(fit$group$coord)) {
      z <- as.data.frame(fit$group$coord); z$block <- rownames(z); z
    } else data.frame()
    status <- "exploratory_MFA"
  } else {
    scaled_blocks <- lapply(names(blocks), function(b) {
      vv <- blocks[[b]]
      Z <- scale(df[vv])
      Z <- as.matrix(Z) / sqrt(ncol(Z))
      colnames(Z) <- vv; Z
    })
    Z <- do.call(cbind, scaled_blocks)
    fit <- stats::prcomp(Z, center = FALSE, scale. = FALSE, rank. = min(ncp, ncol(Z), nrow(Z) - 1L))
    person <- as.data.frame(fit$x); person$id <- ids
    variables <- as.data.frame(fit$rotation); variables$variable <- rownames(variables)
    block_coord <- do.call(rbind, lapply(names(blocks), function(b) {
      vv <- intersect(blocks[[b]], rownames(fit$rotation))
      if (!length(vv)) return(NULL)
      vals <- colMeans(abs(fit$rotation[vv, , drop = FALSE]))
      data.frame(block = b, t(vals), check.names = FALSE)
    }))
    status <- "exploratory_block_scaled_PCA_fallback_not_MFA"
  }
  structure(list(
    model = fit, person_coordinates = person, variable_coordinates = variables,
    block_coordinates = block_coord, blocks = blocks, engine = chosen,
    status = status,
    caveat = "Multiblock mapping is exploratory structure description and does not replace IRT calibration, DIF analysis, or external validation."
  ), class = "eye_multiblock_process_map")
}

#' Extract multiblock block contributions/coordinates
#' @return An R object containing multiblock block contributions/coordinates. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param x Object to process, inspect, compare, or plot.
multiblock_contributions <- function(x) {
  if (!inherits(x, "eye_multiblock_process_map")) stop("x must be eye_multiblock_process_map.", call. = FALSE)
  x$block_coordinates
}

#' Extract multiblock person coordinates
#' @return An R object containing multiblock person coordinates. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param x Object to process, inspect, compare, or plot.
multiblock_person_coordinates <- function(x) {
  if (!inherits(x, "eye_multiblock_process_map")) stop("x must be eye_multiblock_process_map.", call. = FALSE)
  x$person_coordinates
}

#' Extract multiblock variable coordinates
#' @return An R object containing multiblock variable coordinates. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param x Object to process, inspect, compare, or plot.
multiblock_variable_coordinates <- function(x) {
  if (!inherits(x, "eye_multiblock_process_map")) stop("x must be eye_multiblock_process_map.", call. = FALSE)
  x$variable_coordinates
}

.ep08_soft_cluster_prob <- function(Z, centers) {
  d2 <- sapply(seq_len(nrow(centers)), function(k) rowSums((Z - matrix(centers[k, ], nrow(Z), ncol(Z), byrow = TRUE))^2))
  if (is.null(dim(d2))) d2 <- matrix(d2, ncol = 1L)
  s <- exp(-0.5 * sweep(d2, 1L, apply(d2, 1L, min), "-"))
  s / rowSums(s)
}

#' Fit exploratory process profiles
#'
#' @param data Person-level process data.
#' @param variables Continuous process variables.
#' @param k Number of profiles.
#' @param id Optional person identifier.
#' @param engine `auto`, `tidyLPA`, or `kmeans_reference`.
#' @param seed Random seed.
#' @return An object of class "eye_process_profile_mixture", stored as a named list, with components "model", "assignment", "summary", "variables", "k", "engine", "scaled_data", "status", "caveat". It contains exploratory process profiles and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_process_profile_mixture <- function(
    data, variables, k = 3L, id = "person_id",
    engine = c("auto", "tidyLPA", "kmeans_reference"), seed = 777) {
  engine <- match.arg(engine)
  data <- .ep08_as_df(data)
  if (!length(variables)) stop("Supply at least one process-profile variable.", call. = FALSE)
  .ep08_req_cols(data, variables)
  k <- as.integer(k)
  if (k < 2L) stop("k must be at least 2.", call. = FALSE)
  X <- data[, variables, drop = FALSE]
  for (v in variables) X[[v]] <- .ep08_num(X[[v]])
  usable <- variables[vapply(X[variables], function(z) { sz <- .ep08_sd(z); is.finite(sz) && sz > 0 }, logical(1))]
  if (length(usable) < 2L) stop("At least two varying numeric process variables are required.", call. = FALSE)
  variables <- usable; X <- X[, variables, drop = FALSE]
  ok <- stats::complete.cases(X)
  X <- X[ok, , drop = FALSE]
  if (nrow(X) < max(20L, 4L * k)) stop("Too few complete cases for requested profile count.", call. = FALSE)
  Z <- scale(X)
  ids <- if (!is.null(id) && id %in% names(data)) as.character(data[[id]][ok]) else as.character(which(ok))
  chosen <- engine
  if (engine == "auto") chosen <- if (requireNamespace("tidyLPA", quietly = TRUE)) "tidyLPA" else "kmeans_reference"
  set.seed(seed)
  if (chosen == "tidyLPA") {
    if (!requireNamespace("tidyLPA", quietly = TRUE)) stop("Package `tidyLPA` is required.", call. = FALSE)
    fit <- tidyLPA::estimate_profiles(as.data.frame(Z), n_profiles = k, models = 1)
    dat <- tryCatch(tidyLPA::get_data(fit), error = function(e) NULL)
    if (is.null(dat) || !"Class" %in% names(dat))
      stop("tidyLPA fit did not expose profile assignments through get_data().", call. = FALSE)
    cls <- as.integer(dat$Class)
    probs <- as.matrix(dat[grep("^CPROB", names(dat), value = TRUE)])
    if (!ncol(probs)) {
      centers <- do.call(rbind, lapply(seq_len(k), function(j) colMeans(Z[cls == j, , drop = FALSE])))
      probs <- .ep08_soft_cluster_prob(Z, centers)
    }
    status <- "exploratory_latent_profile_analysis"
  } else {
    fit <- stats::kmeans(Z, centers = k, nstart = 50)
    cls <- fit$cluster
    probs <- .ep08_soft_cluster_prob(Z, fit$centers)
    status <- "descriptive_kmeans_reference_not_finite_mixture"
  }
  assignment <- data.frame(id = ids, profile = paste0("profile_", cls), stringsAsFactors = FALSE)
  for (j in seq_len(ncol(probs))) assignment[[paste0("profile_probability_", j)]] <- probs[, j]
  dat_complete <- data[ok, , drop = FALSE]
  dat_complete$.profile <- assignment$profile
  summary_tab <- stats::aggregate(dat_complete[variables], by = list(profile = dat_complete$.profile), FUN = .ep08_mean)
  structure(list(
    model = fit, assignment = assignment, summary = summary_tab,
    variables = variables, k = k, engine = chosen, scaled_data = Z,
    status = status,
    caveat = "Process profiles are exploratory descriptive groupings; they are not clinical, cheating, engagement, or cognitive-strategy labels without external validation."
  ), class = "eye_process_profile_mixture")
}

#' Extract process-profile probabilities
#' @return An R object containing process-profile probabilities. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param x Object to process, inspect, compare, or plot.
process_profile_probabilities <- function(x) {
  if (!inherits(x, "eye_process_profile_mixture")) stop("x must be eye_process_profile_mixture.", call. = FALSE)
  x$assignment
}

#' Summarize process profiles
#' @return An R object containing process profiles. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param x Object to process, inspect, compare, or plot.
process_profile_summary <- function(x) {
  if (!inherits(x, "eye_process_profile_mixture")) stop("x must be eye_process_profile_mixture.", call. = FALSE)
  x$summary
}

#' Compare candidate process-profile solutions
#' @param data Person-level process data.
#' @param variables Variables used for profiling.
#' @param k_values Candidate numbers of profiles.
#' @param seed Seed.
#' @return A tabular R object containing candidate process-profile solutions; rows represent analysis units and columns contain the returned quantities.
#' @export
compare_process_profile_solutions <- function(data, variables, k_values = 2:6, seed = 777) {
  data <- .ep08_as_df(data); .ep08_req_cols(data, variables)
  X <- data[, variables, drop = FALSE]
  for (v in variables) X[[v]] <- .ep08_num(X[[v]])
  usable <- variables[vapply(X[variables], function(z) { sz <- .ep08_sd(z); is.finite(sz) && sz > 0 }, logical(1))]
  if (length(usable) < 2L) stop("At least two varying numeric variables are required.", call. = FALSE)
  X <- X[, usable, drop = FALSE]
  X <- scale(X[stats::complete.cases(X), , drop = FALSE])
  k_values <- unique(as.integer(k_values))
  k_values <- k_values[is.finite(k_values) & k_values >= 2L & k_values < nrow(X)]
  if (!length(k_values)) stop("No valid k_values for the available complete cases.", call. = FALSE)
  set.seed(seed)
  rows <- lapply(k_values, function(k) {
    fit <- stats::kmeans(X, centers = as.integer(k), nstart = 30)
    data.frame(k = as.integer(k), total_withinss = fit$tot.withinss,
               between_over_total = fit$betweenss / fit$totss)
  })
  do.call(rbind, rows)
}

#' Audit external/structural validity of process traits
#'
#' @param data Person-level data containing a criterion and process predictors.
#' @param criterion External criterion column.
#' @param predictors Process predictors.
#' @param baseline_predictors Optional baseline predictors for incremental validity.
#' @return An object of class "eye_process_external_validity", stored as a named list, with components "full_model", "baseline_model", "comparison", "associations", "criterion", "predictors", "baseline_predictors", "data", "incremental_r2", "status", "caveat". It contains external/structural validity of process traits and associated metadata or diagnostics needed to interpret the result.
#' @export
audit_process_external_validity <- function(data, criterion, predictors,
                                            baseline_predictors = NULL) {
  data <- .ep08_as_df(data)
  if (!length(predictors)) stop("Supply at least one process predictor.", call. = FALSE)
  req <- unique(c(criterion, predictors, baseline_predictors))
  .ep08_req_cols(data, req)
  d <- data[, req, drop = FALSE]
  for (v in req) d[[v]] <- .ep08_num(d[[v]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) < 20L) stop("At least 20 complete cases are required.", call. = FALSE)
  full <- stats::lm(stats::reformulate(unique(c(baseline_predictors, predictors)), response = criterion), data = d)
  base <- if (length(baseline_predictors)) stats::lm(stats::reformulate(baseline_predictors, response = criterion), data = d) else
    stats::lm(stats::as.formula(paste(criterion, "~ 1")), data = d)
  an <- tryCatch(stats::anova(base, full), error = function(e) NULL)
  cor_tab <- data.frame(
    predictor = predictors,
    correlation = vapply(predictors, function(p) suppressWarnings(stats::cor(d[[criterion]], d[[p]], use = "complete.obs")), numeric(1)),
    stringsAsFactors = FALSE
  )
  structure(list(
    full_model = full, baseline_model = base, comparison = an,
    associations = cor_tab, criterion = criterion, predictors = predictors,
    baseline_predictors = baseline_predictors, data = d,
    incremental_r2 = summary(full)$r.squared - summary(base)$r.squared,
    status = "external_structural_validation",
    caveat = "Association with an external criterion supports validity evidence but does not establish causal mechanisms."
  ), class = "eye_process_external_validity")
}

#' Extract process-criterion associations
#' @return An R object containing process-criterion associations. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
#' @param x Object to process, inspect, compare, or plot.
process_criterion_associations <- function(x) {
  if (!inherits(x, "eye_process_external_validity")) stop("x must be eye_process_external_validity.", call. = FALSE)
  x$associations
}

#' Extract incremental process validity
#' @return A data frame containing incremental process validity. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
#' @param x Object to process, inspect, compare, or plot.
incremental_process_validity <- function(x) {
  if (!inherits(x, "eye_process_external_validity")) stop("x must be eye_process_external_validity.", call. = FALSE)
  data.frame(baseline_r2 = summary(x$baseline_model)$r.squared,
             full_r2 = summary(x$full_model)$r.squared,
             incremental_r2 = x$incremental_r2)
}

#' Compare process external-validity models
#' @return A data frame containing process external-validity models. Rows represent the analysis units and columns contain the identifiers, estimates, or diagnostics defined by the function.
#' @export
#' @param x Object to process, inspect, compare, or plot.
compare_process_criterion_models <- function(x) {
  if (!inherits(x, "eye_process_external_validity")) stop("x must be eye_process_external_validity.", call. = FALSE)
  if (is.null(x$comparison)) return(data.frame())
  as.data.frame(x$comparison)
}

#' Fit an experimental pre-pilot item-parameter seeding model
#'
#' Estimates screening predictions for item difficulty/discrimination from item
#' design/process features. Predictions are not calibrated operational parameters.
#'
#' @param item_data Calibrated item-level training data.
#' @param difficulty,discrimination Target columns.
#' @param predictors Design/process predictors.
#' @param engine `auto`, `ranger`, or `lm`.
#' @param seed Seed.
#' @return An object of class "eye_item_parameter_seed", stored as a named list, with components "difficulty_model", "discrimination_model", "difficulty", "discrimination", "predictors", "engine", "training_data", "status", "caveat". It contains an experimental pre-pilot item-parameter seeding model and associated metadata or diagnostics needed to interpret the result.
#' @export
fit_item_parameter_seed_model <- function(
    item_data, difficulty = "irt_difficulty", discrimination = "irt_discrimination",
    predictors, engine = c("auto", "ranger", "lm"), seed = 2221) {
  engine <- match.arg(engine)
  if (!length(predictors)) stop("Supply at least one item design/process predictor.", call. = FALSE)
  item_data <- .ep08_as_df(item_data, "item_data")
  .ep08_req_cols(item_data, c(difficulty, discrimination, predictors), "item_data")
  d <- item_data[, c(difficulty, discrimination, predictors), drop = FALSE]
  for (v in names(d)) d[[v]] <- .ep08_num(d[[v]])
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (any(vapply(d[predictors], function(z) { sz <- .ep08_sd(z); !is.finite(sz) || sz == 0 }, logical(1))))
    stop("All item-seeding predictors must vary in the complete training data.", call. = FALSE)
  if (nrow(d) < max(8L, length(predictors) + 3L)) stop("Too few complete calibrated items for parameter seeding.", call. = FALSE)
  chosen <- engine
  if (engine == "auto") chosen <- if (requireNamespace("ranger", quietly = TRUE) && nrow(d) >= 15L) "ranger" else "lm"
  fd <- stats::reformulate(predictors, response = difficulty)
  fa <- stats::reformulate(predictors, response = discrimination)
  set.seed(seed)
  if (chosen == "ranger") {
    if (!requireNamespace("ranger", quietly = TRUE)) stop("Package `ranger` is required.", call. = FALSE)
    md <- ranger::ranger(fd, data = d, num.trees = 500, seed = seed)
    ma <- ranger::ranger(fa, data = d, num.trees = 500, seed = seed + 1L)
  } else {
    md <- stats::lm(fd, data = d); ma <- stats::lm(fa, data = d)
  }
  structure(list(
    difficulty_model = md, discrimination_model = ma,
    difficulty = difficulty, discrimination = discrimination, predictors = predictors,
    engine = chosen, training_data = d,
    status = "experimental_pre_pilot_screening",
    caveat = paste(
      "Predicted item parameters are screening priors/cold-start estimates only.",
      "Operational use requires expert review, pilot data, bias/accessibility review, and formal IRT calibration."
    )
  ), class = "eye_item_parameter_seed")
}

#' Predict pre-pilot item-parameter priors
#' @param object Seed model.
#' @param newdata Candidate item feature data.
#' @return An R object containing pre-pilot item-parameter priors. The concrete class and structure follow the selected method, engine, or input object and are preserved as documented by that workflow.
#' @export
predict_item_parameter_priors <- function(object, newdata) {
  if (!inherits(object, "eye_item_parameter_seed")) stop("object must be eye_item_parameter_seed.", call. = FALSE)
  newdata <- .ep08_as_df(newdata, "newdata"); .ep08_req_cols(newdata, object$predictors, "newdata")
  d <- newdata
  for (v in object$predictors) d[[v]] <- .ep08_num(d[[v]])
  if (object$engine == "ranger") {
    pd <- stats::predict(object$difficulty_model, data = d)$predictions
    pa <- stats::predict(object$discrimination_model, data = d)$predictions
  } else {
    pd <- stats::predict(object$difficulty_model, newdata = d)
    pa <- stats::predict(object$discrimination_model, newdata = d)
  }
  out <- newdata
  out$predicted_pre_pilot_difficulty <- as.numeric(pd)
  out$predicted_pre_pilot_discrimination <- pmax(as.numeric(pa), 0.05)
  out$operational_status <- "not_operational_requires_review_pilot_calibration"
  out
}

#' Audit a candidate item bank against a seed model
#' @param object Seed model.
#' @param candidate_data Candidate item feature data.
#' @param difficulty_range Plausible screening range for predicted difficulty.
#' @param discrimination_min Minimum screening discrimination.
#' @return An object of class "eye_candidate_item_bank_audit", stored as a named list, with components "table", "seed_model", "status", "caveat". It contains a candidate item bank against a seed model and associated metadata or diagnostics needed to interpret the result.
#' @export
audit_candidate_item_bank <- function(object, candidate_data,
                                      difficulty_range = c(-3, 3), discrimination_min = 0.3) {
  if (length(difficulty_range) != 2L || any(!is.finite(difficulty_range)) || difficulty_range[1L] >= difficulty_range[2L])
    stop("difficulty_range must contain two increasing finite values.", call. = FALSE)
  if (!is.finite(discrimination_min) || discrimination_min <= 0)
    stop("discrimination_min must be positive.", call. = FALSE)
  p <- predict_item_parameter_priors(object, candidate_data)
  p$difficulty_review_flag <- p$predicted_pre_pilot_difficulty < difficulty_range[1L] |
    p$predicted_pre_pilot_difficulty > difficulty_range[2L]
  p$discrimination_review_flag <- p$predicted_pre_pilot_discrimination < discrimination_min
  p$review_required <- p$difficulty_review_flag | p$discrimination_review_flag
  structure(list(table = p, seed_model = object,
                 status = "experimental_candidate_item_screening",
                 caveat = object$caveat), class = "eye_candidate_item_bank_audit")
}
