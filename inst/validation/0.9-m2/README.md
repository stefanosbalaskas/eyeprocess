# eyeprocess 0.9 Milestone #2 validation execution

This directory ships reproducible **software-validation** scripts and design metadata. Executed evidence must be written outside the installed package/source tree. None of these simulations establishes construct validity of gaze, pupil, response-time, or process measures.

The main entry point is `scripts/run-all-validation-evidence.R`. Use profile `local` for a compact deterministic run and `full` for the larger predeclared evidence programme. Exact IRT recovery is executed only when `mirt` is available; otherwise it is explicitly gated and no substitute estimator is used.
