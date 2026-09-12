# Run the all-evidence script; SBC ranks and summaries are always generated.
main <- system.file("validation", "0.9-m2", "scripts", "run-all-validation-evidence.R", package = "eyeprocess")
if (!nzchar(main)) stop("Installed eyeprocess Milestone #2 validation runner not found.")
validation_env <- new.env(parent = asNamespace("eyeprocess"))
sys.source(main, envir = validation_env)
