# eyeprocess 0.2.0.9002 portable-fixture hotfix

The complete package gate for 0.2.0.9001 passed all unit tests and
substantive checks, but `R CMD check` reported one WARNING because two
bundled regression fixtures contained spaces in their filenames:

- `inst/extdata/gazepoint_v72/User 3_all_gaze.csv`
- `inst/extdata/gazepoint_v72/User 3_fixations.csv`

Version 0.2.0.9002 renames them to:

- `inst/extdata/gazepoint_v72/user3_all_gaze.csv`
- `inst/extdata/gazepoint_v72/user3_fixations.csv`

All test references were updated. Filename-based identity inference
still returns canonical participant identity `User 3`. A regression test
now requires every bundled `inst/extdata` path to be free of whitespace.

No import, schema, modelling, plotting, biometric, or
empirical-validation behavior changed.
