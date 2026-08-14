library(eyeprocess)
set.seed(42)
response_only <- matrix(rnorm(4000, sd=1.0), ncol=4)
response_plus_process <- matrix(rnorm(4000, sd=0.75), ncol=4)
colnames(response_only) <- colnames(response_plus_process) <-
  paste0("person_", 1:4)
info <- process_information(
  response_only,
  response_plus_process,
  metric="entropy_reduction"
)
info
plot(info)
