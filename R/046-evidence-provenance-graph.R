# Evidence and decision provenance graphs ------------------------------------

.mi_nodes_from_stage <- function(x, stage) {
  if (is.null(x)) return(data.frame())
  if (is.data.frame(x) && all(c("node_id", "label") %in% names(x))) {
    out <- x
    if (!"stage" %in% names(out)) out$stage <- stage
    return(out)
  }
  labels <- if (is.list(x)) names(x) else as.character(x)
  if (is.null(labels) || any(!nzchar(labels))) labels <- paste0(stage, "_", seq_along(x))
  data.frame(node_id = paste0(stage, "::", make.unique(labels)), label = labels, stage = stage, stringsAsFactors = FALSE)
}

#' Build an evidence and decision provenance graph
#'
#' @param raw_data,transformations,metrics,models,diagnostics,decisions Named objects, character labels, or node tables.
#' @param edges Optional explicit edge table with `from` and `to`.
#' @return An `eye_evidence_graph` object.
#' @export
#' @noRd
build_evidence_graph <- function(raw_data, transformations = NULL, metrics = NULL, models = NULL, diagnostics = NULL, decisions = NULL, edges = NULL) {
  stages <- list(raw_data = raw_data, transformations = transformations, metrics = metrics, models = models, diagnostics = diagnostics, decisions = decisions)
  nodes <- do.call(rbind, lapply(names(stages), function(stage) .mi_nodes_from_stage(stages[[stage]], stage)))
  if (!nrow(nodes)) .mi_stop("At least one evidence node is required.")
  if (is.null(edges)) {
    edge_rows <- list(); k <- 1L
    active_stages <- names(stages)[vapply(stages, function(value) !is.null(value) && length(value), logical(1))]
    if (length(active_stages) > 1L) {
      for (i in seq_len(length(active_stages) - 1L)) {
        from <- nodes$node_id[nodes$stage == active_stages[[i]]]
        to <- nodes$node_id[nodes$stage == active_stages[[i + 1L]]]
        for (one_to in to) {
          parents <- if (length(from) <= 3L) from else from[seq_len(3L)]
          edge_rows[[k]] <- data.frame(from = parents, to = one_to, relation = "supports", stringsAsFactors = FALSE); k <- k + 1L
        }
      }
    }
    edges <- if (length(edge_rows)) do.call(rbind, edge_rows) else data.frame(from = character(), to = character(), relation = character())
  } else {
    edges <- as.data.frame(edges)
    .mi_assert_columns(edges, c("from", "to"))
    if (!"relation" %in% names(edges)) edges$relation <- "supports"
  }
  .mi_new("eye_evidence_graph", nodes = nodes, edges = edges, stages = stages,
          summary = data.frame(nodes = nrow(nodes), edges = nrow(edges), decisions = sum(nodes$stage == "decisions")), status = "Evidence provenance graph built.")
}

.mi_ancestors <- function(edges, node) {
  found <- node; frontier <- node
  repeat {
    parents <- unique(edges$from[edges$to %in% frontier])
    parents <- setdiff(parents, found)
    if (!length(parents)) break
    found <- c(found, parents); frontier <- parents
  }
  found
}

#' Trace evidence supporting an item decision
#'
#' @param graph Evidence graph.
#' @param item_id Decision node identifier or label fragment.
#' @return An `eye_decision_trace` object.
#' @export
#' @noRd
trace_item_decision <- function(graph, item_id) {
  if (!inherits(graph, "eye_evidence_graph")) .mi_stop("`graph` must be an `eye_evidence_graph` object.")
  candidates <- graph$nodes$node_id[graph$nodes$node_id == item_id | grepl(item_id, graph$nodes$label, fixed = TRUE)]
  candidates <- candidates[graph$nodes$stage[match(candidates, graph$nodes$node_id)] == "decisions"] %||% candidates
  if (!length(candidates)) .mi_stop("No matching decision node was found.")
  target <- candidates[[1L]]
  nodes <- .mi_ancestors(graph$edges, target)
  sub_nodes <- graph$nodes[graph$nodes$node_id %in% nodes, , drop = FALSE]
  sub_edges <- graph$edges[graph$edges$from %in% nodes & graph$edges$to %in% nodes, , drop = FALSE]
  .mi_new("eye_decision_trace", graph = graph, target = target, nodes = sub_nodes, edges = sub_edges,
          summary = sub_nodes, status = "Decision evidence path traced."
  )
}

#' Compare two decision-provenance graphs
#'
#' @param graph_a,graph_b Evidence graphs.
#' @return An `eye_provenance_comparison` object.
#' @export
#' @noRd
compare_decision_provenance <- function(graph_a, graph_b) {
  if (!inherits(graph_a, "eye_evidence_graph") || !inherits(graph_b, "eye_evidence_graph")) .mi_stop("Both inputs must be evidence graphs.")
  nodes_a <- graph_a$nodes$node_id; nodes_b <- graph_b$nodes$node_id
  edge_key <- function(edges) paste(edges$from, edges$to, edges$relation, sep = " -> ")
  edges_a <- edge_key(graph_a$edges); edges_b <- edge_key(graph_b$edges)
  summary <- data.frame(
    feature = c("nodes_added", "nodes_removed", "edges_added", "edges_removed"),
    count = c(length(setdiff(nodes_b, nodes_a)), length(setdiff(nodes_a, nodes_b)), length(setdiff(edges_b, edges_a)), length(setdiff(edges_a, edges_b))),
    stringsAsFactors = FALSE
  )
  .mi_new("eye_provenance_comparison", graph_a = graph_a, graph_b = graph_b, summary = summary,
          nodes_added = setdiff(nodes_b, nodes_a), nodes_removed = setdiff(nodes_a, nodes_b), edges_added = setdiff(edges_b, edges_a), edges_removed = setdiff(edges_a, edges_b), status = "Decision provenance graphs compared.")
}

.mi_has_cycle <- function(nodes, edges) {
  visited <- setNames(rep(0L, length(nodes)), nodes)
  visit <- function(node) {
    if (visited[[node]] == 1L) return(TRUE)
    if (visited[[node]] == 2L) return(FALSE)
    visited[[node]] <<- 1L
    children <- edges$to[edges$from == node]
    for (child in children) if (child %in% nodes && visit(child)) return(TRUE)
    visited[[node]] <<- 2L
    FALSE
  }
  any(vapply(nodes, visit, logical(1)))
}

#' Audit evidence-graph dependencies
#'
#' @param graph Evidence graph.
#' @return An `eye_evidence_dependency_audit` object.
#' @export
#' @noRd
audit_evidence_dependencies <- function(graph) {
  if (!inherits(graph, "eye_evidence_graph")) .mi_stop("`graph` must be an `eye_evidence_graph` object.")
  nodes <- graph$nodes$node_id
  missing_from <- setdiff(graph$edges$from, nodes); missing_to <- setdiff(graph$edges$to, nodes)
  incoming <- table(factor(graph$edges$to, levels = nodes)); outgoing <- table(factor(graph$edges$from, levels = nodes))
  orphan <- nodes[incoming == 0 & outgoing == 0]
  cycle <- if (nrow(graph$edges)) .mi_has_cycle(nodes, graph$edges) else FALSE
  summary <- data.frame(missing_source_nodes = length(missing_from), missing_target_nodes = length(missing_to), orphan_nodes = length(orphan), has_cycle = cycle, passed = !length(missing_from) && !length(missing_to) && !cycle)
  .mi_new("eye_evidence_dependency_audit", graph = graph, missing_from = missing_from, missing_to = missing_to, orphan = orphan, summary = summary, status = "Evidence dependency audit completed.")
}

.mi_plot_graph <- function(nodes, edges, title) {
  stages <- unique(nodes$stage)
  x_position <- match(nodes$stage, stages)
  y_position <- ave(seq_len(nrow(nodes)), nodes$stage, FUN = function(index) seq(0.1, 0.9, length.out = length(index)))
  graphics::plot(range(x_position) + c(-0.5, 0.5), c(0, 1), type = "n", xaxt = "n", yaxt = "n", xlab = "Evidence stage", ylab = "", main = title)
  graphics::axis(1, at = seq_along(stages), labels = stages, las = 2)
  positions <- data.frame(node_id = nodes$node_id, x = x_position, y = y_position)
  if (nrow(edges)) {
    for (i in seq_len(nrow(edges))) {
      from <- positions[positions$node_id == edges$from[[i]], ]; to <- positions[positions$node_id == edges$to[[i]], ]
      if (nrow(from) && nrow(to)) graphics::arrows(from$x, from$y, to$x, to$y, length = 0.05)
    }
  }
  graphics::points(x_position, y_position, pch = 21, bg = "white", cex = 2)
  graphics::text(x_position, y_position, labels = nodes$label, cex = 0.7)
}

#' @export
plot.eye_evidence_graph <- function(x, type = c("graph", "metric_dependencies", "model_impact", "diagnostics"), ...) {
  type <- match.arg(type)
  .mi_plot_graph(x$nodes, x$edges, if (type == "metric_dependencies") "Metric dependency graph" else if (type == "model_impact") "Model-to-decision impact graph" else "Evidence and decision provenance")
  invisible(x)
}
#' @export
plot.eye_decision_trace <- function(x, type = c("decision_path", "graph"), ...) {
  .mi_plot_graph(x$nodes, x$edges, paste("Decision evidence path:", x$target)); invisible(x)
}
#' @export
plot.eye_provenance_comparison <- function(x, type = c("comparison", "diagnostics"), ...) {
  graphics::barplot(x$summary$count, names.arg = x$summary$feature, las = 2, ylab = "Count", main = "Decision provenance changes"); invisible(x)
}
#' @export
plot_evidence_graph <- function(x, ...) plot(x, type = "graph", ...)
#' @export
plot_item_decision_path <- function(x, ...) plot(x, type = "decision_path", ...)
#' @export
plot_metric_dependency_graph <- function(x, ...) plot(x, type = "metric_dependencies", ...)
#' @export
plot_model_decision_impact <- function(x, ...) plot(x, type = "model_impact", ...)
