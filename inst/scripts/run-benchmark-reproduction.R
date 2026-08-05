#!/usr/bin/env Rscript
library(eyeprocess)
study <- eyeprocess_benchmark_study()
validation <- validate_benchmark_study(study)
print(validation)
stopifnot(validation$valid)
result <- run_benchmark_reproduction(study)
print(result)
stopifnot(result$passed)
