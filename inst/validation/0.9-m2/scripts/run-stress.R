# Run the all-evidence script; measurement-stress evidence is executed, summarized, frozen, and indexed.
main <- system.file("validation", "0.9-m2", "scripts", "run-all-validation-evidence.R", package = "eyeprocess")
if (!nzchar(main)) stop("Installed eyeprocess Milestone #2 validation runner not found.")
validation_env <- new.env(parent = asNamespace("eyeprocess"))
sys.source(main, envir = validation_env)
