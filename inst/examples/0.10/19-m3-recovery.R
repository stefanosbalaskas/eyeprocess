# A tiny run is a computational smoke test only. Use a predeclared larger grid for scientific evidence.
if (FALSE) {
  rec <- multimodal_m3_recovery(reps = 2, pupil_signal = c("informative", "null", "confounded"), pupil_missingness = c("mcar", "device"), n_person = 80, n_item = 10)
  print(rec)
  plot(rec, type = "pupil")
  plot(rec, type = "coverage")
}
