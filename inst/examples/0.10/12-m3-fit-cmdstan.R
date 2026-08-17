# Computational example. The full pre-commit validator runs an actual
# multi-chain installed-package fit; keep ordinary example execution light.
if (FALSE) {
  sim <- simulate_multimodal_m3(n_person = 60, n_item = 10, seed = 20260815)
  fit <- fit_multimodal_m3(
    sim,
    chains = 4,
    parallel_chains = 4,
    iter_warmup = 600,
    iter_sampling = 400,
    seed = 20260815,
    init = 0
  )
  print(fit)
  print(summary(fit))
  print(validate_multimodal_m3(fit))
}
