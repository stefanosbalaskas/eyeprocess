# Run the all-evidence script; negative controls and known-leakage audit are generated.
main <- system.file("validation", "0.9-m2", "scripts", "run-all-validation-evidence.R", package = "eyeprocess")
if (!nzchar(main)) stop("Installed eyeprocess Milestone #2 validation runner not found.")
sys.source(main, envir = globalenv())
