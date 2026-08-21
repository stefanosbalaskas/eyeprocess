rec <- multimodal_m4_recovery()
stopifnot(inherits(rec, "eye_multimodal_m4_recovery"), !isTRUE(rec$executed), nrow(rec$design) == 5L)
print(rec)
