for (mechanism in c("mcar", "quality", "gaze", "ability", "device")) {
  sim <- simulate_multimodal_m3(n_person = 60, n_item = 10, pupil_missingness = mechanism, seed = 100 + match(mechanism, c("mcar","quality","gaze","ability","device")))
  audit <- audit_multimodal_m3_identifiability(sim)
  cat("\n", mechanism, ": pupil missing=", sprintf("%.3f", audit$pupil$missing_fraction), "\n", sep = "")
}
