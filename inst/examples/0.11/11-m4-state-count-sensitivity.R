sim <- simulate_multimodal_m4(n_person = 30L, n_item = 8L, seed = 20260820L)
sens <- multimodal_m4_sensitivity(sim, n_states = 1:3)
stopifnot(inherits(sens, "eye_multimodal_m4_sensitivity"), !isTRUE(sens$executed))
print(sens)
