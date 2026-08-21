sim <- simulate_multimodal_m4(n_person = 30L, n_item = 8L, seed = 20260820L)
stopifnot(inherits(sim, "eye_multimodal_m4_simulation"), nrow(sim$data) == 240L)
print(sim)
