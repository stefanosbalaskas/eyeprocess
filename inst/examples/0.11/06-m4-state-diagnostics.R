sim <- simulate_multimodal_m4(n_person = 30L, n_item = 8L, seed = 20260820L)
states <- multimodal_m4_state_diagnostics(sim)
stopifnot(inherits(states, "eye_multimodal_m4_states"), abs(sum(states$occupancy$mean_probability) - 1) < 1e-8)
print(states)
