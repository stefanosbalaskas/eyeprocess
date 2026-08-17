# Computationally intensive: fits all eight response-anchored channel combinations.
if (FALSE) {
  sim <- simulate_multimodal_m3(n_person = 80, n_item = 10, seed = 20260815)
  ab <- multimodal_m3_ablation(sim, chains = 4, parallel_chains = 4, iter_warmup = 750, iter_sampling = 500, refresh = 0)
  info <- multimodal_m3_process_information(ab)
  print(info)
  plot(info, type = "incremental_pupil")
  plot(info, type = "redundancy")
  plot(info, type = "sensor_value")
}
