if (requireNamespace("cmdstanr", quietly = TRUE)) {
  sim <- simulate_multimodal_m4(n_person = 40L, n_item = 10L, seed = 20260820L)
  spec <- multimodal_m4_spec(n_states = 2L)
  # Deliberate backend fit; uncomment for interactive use.
  # fit <- fit_multimodal_m4(sim, spec = spec, chains = 4L, iter_warmup = 600L, iter_sampling = 400L)
}
