# Copy and edit. These functions are sourced in an isolated validation worker.
simulator <- function(n_person, n_item, process_effect, seed, ...) {
  set.seed(seed)
  data <- rnorm(n_person * n_item, mean = process_effect)
  structure(data, truth = c(process_effect = process_effect))
}

fitter <- function(simulation, ...) {
  list(estimate = mean(simulation), std_error = sd(simulation) / sqrt(length(simulation)), converged = TRUE)
}

extractor <- function(fit) {
  data.frame(parameter = "process_effect", estimate = fit$estimate, std_error = fit$std_error)
}

truth_extractor <- function(simulation) attr(simulation, "truth")

diagnostics_extractor <- function(fit) data.frame(converged = fit$converged)
