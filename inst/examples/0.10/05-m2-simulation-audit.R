library(eyeprocess)

sim <- simulate_multimodal_m2(
  n_person = 80,
  n_item = 10,
  seed = 20260814
)

print(sim)

audit <- audit_multimodal_m2_identifiability(
  sim$data
)

print(audit)
