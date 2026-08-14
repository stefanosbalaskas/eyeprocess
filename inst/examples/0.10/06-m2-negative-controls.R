library(eyeprocess)

sim <- simulate_multimodal_m2(
  n_person = 80,
  n_item = 10,
  seed = 20260815
)

controls <- multimodal_m2_negative_controls(
  sim,
  seed = 20260816
)

print(controls)
print(controls$provenance)
