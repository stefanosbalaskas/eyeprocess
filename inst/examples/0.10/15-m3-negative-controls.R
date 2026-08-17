sim <- simulate_multimodal_m3(n_person = 50, n_item = 8, seed = 20260815)
neg <- multimodal_m3_negative_controls(sim, seed = 20260816)
print(neg)
plot(neg, type = "pupil_alignment")
