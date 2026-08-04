## Test environments

* Windows 11 x64, R 4.6.1

## R CMD check results

Version 0.2.0.9001 passed all unit tests, vignettes, examples, code checks, pkgdown configuration, installation, runtime smoke tests, and real Gazepoint empirical validation. `R CMD check` reported one WARNING solely because two packaged regression fixtures contained spaces in their filenames.

Version 0.2.0.9002 renames those fixtures to portable, space-free filenames and adds regression coverage for portable packaged paths. No analytical or import behavior changed. A fresh complete Windows runtime gate is pending for this patch.

The private real-export corpus comprises six paired Gazepoint Analysis 7.2.0 gaze/fixation recordings and four multi-section Data Summary reports. Its dedicated validation and complete corpus workflow passed with zero validation errors and zero validation warnings.
