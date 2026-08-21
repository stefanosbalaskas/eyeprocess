if (exists("fit") && inherits(fit, "eye_multimodal_m4_fit")) {
  state_fit <- fitted(fit, type = "state")
  stopifnot(all(grepl("state_|MAP_state|entropy|person|item|sequence|trial", names(state_fit))))
}
