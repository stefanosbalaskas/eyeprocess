sim <- simulate_multimodal_m4(n_person = 30L, n_item = 8L, seed = 20260820L)
controls <- multimodal_m4_negative_controls(sim, seed = 20260821L)
stopifnot(inherits(controls, "eye_multimodal_m4_negative_controls"), !isTRUE(controls$executed))
print(controls)
