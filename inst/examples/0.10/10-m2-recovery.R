library(eyeprocess)

# Publication-grade recovery is intentionally not started automatically.
#
# rec <- multimodal_m2_recovery(
#   n_rep = 25,
#   n_person = 150,
#   n_item = 15,
#   base_seed = 20261001
# )
#
# print(rec)
# plot(rec, type = "truth_vs_estimate")
# plot(rec, type = "coverage")

message(
  "M2 recovery reports bias, RMSE, posterior SD, and 95% interval coverage."
)
