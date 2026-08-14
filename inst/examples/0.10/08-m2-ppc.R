library(eyeprocess)

# Run after obtaining an M2 fit:
#
# fit <- fit_multimodal_m2(...)
# ppc <- multimodal_m2_ppc(fit)
# print(ppc)
# plot(ppc)

message(
  "M2 PPC uses item-level W (response), L (RT), and M (gaze) ",
  "posterior predictive discrepancies."
)
