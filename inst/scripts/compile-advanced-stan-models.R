#!/usr/bin/env Rscript
if (!requireNamespace("cmdstanr", quietly = TRUE)) stop("Install cmdstanr and CmdStan before compiling advanced models.")
models <- c(
  "dynamic_irtree_observed.stan",
  "dynamic_irtree_hidden.stan",
  "functional_pupil_irt.stan",
  "theory_strategy_mixture.stan",
  "gaze_diffusion_irt.stan"
)
for (name in models) {
  path <- system.file("stan", name, package = "eyeprocess")
  if (!nzchar(path)) stop("Missing Stan program: ", name)
  message("Compiling ", name)
  cmdstanr::cmdstan_model(path, quiet = FALSE)
}
