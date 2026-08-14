library(eyeprocess)

# Computational example:
#
# sim <- simulate_multimodal_m2(n_person = 150, n_item = 15, seed = 220)
# abl <- multimodal_m2_ablation(sim, seed = 221)
# info <- multimodal_m2_process_information(abl)
# print(info)
# plot(info, type = "response_elpd")
# plot(info, type = "theta_variance")

message(
  "M0/M1/M2 information is compared on the common scored-response target; ",
  "channel information is not assumed additive."
)
