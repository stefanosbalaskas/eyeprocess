# eyeprocess 0.2.0.9001 handoff

Install in a fresh R session and run `validate_eyeprocess.R`. The target is all unit tests passing, R CMD check with 0 errors/0 warnings/0 notes, clean pkgdown configuration, and successful runtime smoke tests. Then rerun `validate_gazepoint_real_exports.R` and `validate_real_exports.R`; both should retain PASS status.
