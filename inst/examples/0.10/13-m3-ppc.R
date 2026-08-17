# Requires a fitted M3 object named `fit`.
if (exists("fit") && inherits(fit, "eye_multimodal_m3_fit")) {
  ppc <- multimodal_m3_ppc(fit)
  print(ppc)
  plot(ppc, type = "item_channel")
  plot(ppc, type = "pupil_global")
}
