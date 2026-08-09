# Multi-objective item-bank optimization --------------------------------------

#' Define item-bank objectives
#'
#' @param information,process_burden,fairness,exposure Column names or numeric vectors.
#' @param content_constraints Optional constraint declaration.
#' @param weights Named objective weights.
#' @param directions Named directions, `"max"` or `"min"`.
#' @return An `eye_item_objective_spec` object.
#' @export
#' @noRd
item_objective_spec <- function(
    information,
    process_burden,
    fairness,
    exposure,
    content_constraints = NULL,
    weights = c(information = 1, process_burden = 1, fairness = 1, exposure = 1),
    directions = c(information = "max", process_burden = "min", fairness = "min", exposure = "min")) {
  objective <- list(information = information, process_burden = process_burden, fairness = fairness, exposure = exposure)
  out <- list(objectives = objective, constraints = content_constraints, weights = weights, directions = directions)
  class(out) <- "eye_item_objective_spec"
  out
}

.mi_objective_table <- function(x, spec) {
  .mi_assert_data(x)
  table <- data.frame(item_id = if ("item_id" %in% names(x)) as.character(x$item_id) else paste0("item_", seq_len(nrow(x))), stringsAsFactors = FALSE)
  for (name in names(spec$objectives)) {
    value <- spec$objectives[[name]]
    table[[name]] <- if (length(value) == 1L && is.character(value) && value %in% names(x)) .mi_numeric(x[[value]]) else .mi_numeric(value)
    if (length(table[[name]]) != nrow(x)) .mi_stop(sprintf("Objective `%s` must have one value per item.", name))
  }
  table
}

.mi_dominates <- function(a, b, directions) {
  signs <- ifelse(directions == "max", 1, -1)
  a <- a * signs; b <- b * signs
  all(a >= b) && any(a > b)
}

#' Identify the non-dominated item Pareto front
#'
#' @param x Item table.
#' @param objectives Objective specification.
#' @return An `eye_item_pareto` object.
#' @export
#' @noRd
item_pareto_front <- function(x, objectives) {
  if (!inherits(objectives, "eye_item_objective_spec")) .mi_stop("`objectives` must be an `eye_item_objective_spec`.")
  table <- .mi_objective_table(x, objectives)
  objective_names <- names(objectives$objectives)
  directions <- objectives$directions[objective_names]
  non_dominated <- rep(TRUE, nrow(table))
  for (i in seq_len(nrow(table))) {
    for (j in seq_len(nrow(table))) {
      if (i != j && .mi_dominates(as.numeric(table[j, objective_names]), as.numeric(table[i, objective_names]), directions)) {
        non_dominated[[i]] <- FALSE; break
      }
    }
  }
  standardized <- as.data.frame(lapply(table[objective_names], .mi_z))
  score <- numeric(nrow(table))
  for (name in objective_names) {
    direction <- if (objectives$directions[[name]] == "max") 1 else -1
    weight <- objectives$weights[[name]] %||% 1
    score <- score + direction * weight * standardized[[name]]
  }
  table$pareto_front <- non_dominated
  table$weighted_score <- score
  .mi_new("eye_item_pareto", data = x, objective_spec = objectives, table = table,
          summary = table[order(!table$pareto_front, -table$weighted_score), ], status = "Item Pareto front identified.")
}

.mi_constraint_filter <- function(table, constraints) {
  if (is.null(constraints)) return(rep(TRUE, nrow(table)))
  if (is.function(constraints)) return(as.logical(constraints(table)))
  if (is.list(constraints)) {
    keep <- rep(TRUE, nrow(table))
    for (name in names(constraints)) {
      if (!name %in% names(table)) next
      rule <- constraints[[name]]
      if (length(rule) == 2L && is.numeric(rule)) keep <- keep & table[[name]] >= min(rule) & table[[name]] <= max(rule)
    }
    return(keep)
  }
  rep(TRUE, nrow(table))
}

#' Optimize a multi-objective item bank
#'
#' @param x Item table or Pareto object.
#' @param n_items Number of selected items.
#' @param objectives Objective specification.
#' @param constraints Optional function/list constraints.
#' @param method Integer-style greedy or evolutionary search.
#' @param iterations Evolutionary iterations.
#' @param seed Seed.
#' @return An `eye_item_bank_optimization` object.
#' @export
#' @noRd
optimize_item_bank <- function(x, n_items, objectives, constraints = NULL, method = c("integer", "evolutionary"), iterations = 500, seed = 20260807) {
  method <- match.arg(method)
  pareto <- if (inherits(x, "eye_item_pareto")) x else item_pareto_front(x, objectives)
  table <- pareto$table
  n_items <- as.integer(n_items)
  if (n_items < 1L || n_items > nrow(table)) .mi_stop("`n_items` is outside the available item count.")
  allowed <- .mi_constraint_filter(table, constraints %||% objectives$constraints)
  candidates <- which(allowed)
  if (length(candidates) < n_items) .mi_stop("Constraints leave fewer items than requested.")
  if (method == "integer") {
    selected <- candidates[order(table$pareto_front[candidates], table$weighted_score[candidates], decreasing = TRUE)][seq_len(n_items)]
  } else {
    set.seed(seed)
    best <- sample(candidates, n_items); best_score <- sum(table$weighted_score[best])
    for (i in seq_len(as.integer(iterations))) {
      proposal <- best
      replace_index <- sample(seq_along(proposal), 1L)
      available <- setdiff(candidates, proposal)
      if (!length(available)) break
      proposal[[replace_index]] <- sample(available, 1L)
      score <- sum(table$weighted_score[proposal])
      if (score > best_score) { best <- proposal; best_score <- score }
    }
    selected <- best
  }
  table$selected <- seq_len(nrow(table)) %in% selected
  selected_table <- table[table$selected, , drop = FALSE]
  .mi_new(
    "eye_item_bank_optimization",
    pareto = pareto,
    table = table,
    selected = selected_table,
    n_items = n_items,
    method = method,
    objective_total = sum(selected_table$weighted_score),
    summary = selected_table,
    status = "Multi-objective item bank selected under declared constraints."
  )
}

#' Audit item-bank decision stability
#'
#' @param x Optimization or Pareto object.
#' @param draws Perturbation draws.
#' @param noise_sd Standardized objective perturbation.
#' @param seed Seed.
#' @return An `eye_bank_decision_stability` object.
#' @export
#' @noRd
audit_bank_decision_stability <- function(x, draws = 1000, noise_sd = 0.1, seed = 20260807) {
  optimization <- if (inherits(x, "eye_item_bank_optimization")) x else .mi_stop("`x` must be an item-bank optimization result.")
  table <- optimization$table
  n_items <- optimization$n_items
  set.seed(seed)
  count <- setNames(integer(nrow(table)), table$item_id)
  for (i in seq_len(as.integer(draws))) {
    score <- table$weighted_score + stats::rnorm(nrow(table), 0, noise_sd)
    selected <- order(score, decreasing = TRUE)[seq_len(n_items)]
    count[selected] <- count[selected] + 1L
  }
  summary <- data.frame(item_id = names(count), selection_probability = as.numeric(count) / draws, selected_originally = table$selected, stringsAsFactors = FALSE)
  .mi_new("eye_bank_decision_stability", optimization = optimization, summary = summary,
          status = "Item-bank selection stability audited under objective perturbation.")
}

#' @export
plot.eye_item_pareto <- function(x, type = c("pareto", "tradeoffs", "diagnostics"), x_objective = "information", y_objective = "process_burden", ...) {
  type <- match.arg(type)
  table <- x$table
  x_objective <- if (x_objective %in% names(table)) x_objective else names(x$objective_spec$objectives)[[1L]]
  y_objective <- if (y_objective %in% names(table)) y_objective else names(x$objective_spec$objectives)[[2L]]
  graphics::plot(table[[x_objective]], table[[y_objective]], pch = ifelse(table$pareto_front, 19, 1), xlab = x_objective, ylab = y_objective, main = "Item Pareto front")
  graphics::text(table[[x_objective]], table[[y_objective]], labels = table$item_id, pos = 3, cex = 0.7)
  invisible(x)
}
#' @export
plot.eye_item_bank_optimization <- function(x, type = c("coverage", "selected_profile", "tradeoffs", "diagnostics"), ...) {
  type <- match.arg(type)
  table <- x$table
  if (type == "coverage") {
    objective <- names(x$pareto$objective_spec$objectives)[[1L]]
    graphics::plot(seq_len(nrow(table)), table[[objective]], pch = ifelse(table$selected, 19, 1), xlab = "Item", ylab = objective, main = "Selected-bank information coverage")
  } else {
    objectives <- names(x$pareto$objective_spec$objectives)
    values <- rbind(all_items = colMeans(table[, objectives, drop = FALSE]), selected = colMeans(x$selected[, objectives, drop = FALSE]))
    graphics::barplot(t(values), beside = TRUE, las = 2, ylab = "Objective mean", main = "Selected item-bank profile")
    graphics::legend("topright", legend = rownames(values), fill = seq_len(nrow(values)), bty = "n")
  }
  invisible(x)
}
#' @export
plot.eye_bank_decision_stability <- function(x, type = c("decision_stability", "diagnostics"), ...) {
  order <- order(x$summary$selection_probability, decreasing = TRUE)
  graphics::barplot(x$summary$selection_probability[order], names.arg = x$summary$item_id[order], las = 2, ylab = "Selection probability", main = "Item-bank decision stability")
  invisible(x)
}
#' @export
plot_item_pareto <- function(x, ...) plot(x, type = "pareto", ...)
#' @export
plot_objective_tradeoffs <- function(x, ...) plot(x, type = "tradeoffs", ...)
#' @export
plot_bank_information_coverage <- function(x, ...) plot(x, type = "coverage", ...)
#' @export
plot_decision_stability <- function(x, ...) plot(x, type = "decision_stability", ...)
#' @export
plot_selected_bank_profile <- function(x, ...) plot(x, type = "selected_profile", ...)
