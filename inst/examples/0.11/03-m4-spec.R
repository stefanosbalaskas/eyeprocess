spec <- multimodal_m4_spec(n_states = 2L, trait_conditioning = c("theta", "tau"))
null_spec <- multimodal_m4_spec(n_states = 1L, trait_conditioning = character())
stopifnot(inherits(spec, "eye_multimodal_m4_spec"), isTRUE(null_spec$state_null))
print(spec)
