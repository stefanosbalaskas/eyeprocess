if (exists("ablation_fit") && inherits(ablation_fit, "eye_multimodal_m4_ablation") && isTRUE(ablation_fit$executed)) {
  info <- multimodal_m4_process_information(ablation_fit)
  stopifnot(inherits(info, "eye_multimodal_m4_information"))
  print(info)
}
