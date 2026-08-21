if (exists("fit") && inherits(fit, "eye_multimodal_m4_fit")) {
  ppc <- multimodal_m4_ppc(fit)
  stopifnot(inherits(ppc, "eye_multimodal_m4_ppc"))
  print(ppc)
}
