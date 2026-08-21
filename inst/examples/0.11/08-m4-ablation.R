sim <- simulate_multimodal_m4(n_person = 30L, n_item = 8L, seed = 20260820L)
abl <- multimodal_m4_ablation(sim)
stopifnot(inherits(abl, "eye_multimodal_m4_ablation"), !isTRUE(abl$executed))
print(abl)
