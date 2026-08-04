# eyeprocess 0.0.0.9003 validation hotfix

This release addresses the two findings remaining after the successful
0.0.0.9002 unit-test and R CMD check run.

## Fixed

1. **False installer version mismatch**
   `read.dcf()` returned the source version with a `Version` name attribute.
   `identical()` therefore returned `FALSE` against the unnamed installed
   version even though both printed as `0.0.0.9002`. Both values are now
   converted to trimmed, unnamed character scalars before comparison.

2. **pkgdown URL validation**
   `DESCRIPTION` now declares both the canonical package website and the
   GitHub repository:

   - https://stefanosbalaskas.github.io/eyeprocess
   - https://github.com/stefanosbalaskas/eyeprocess

## Preserved validation status

The user's Windows run established that all unit tests, examples, vignettes,
code checks, dependency checks, documentation checks, and R CMD check stages
passed before pkgdown validation. Version 0.0.0.9003 changes only package
metadata, installer comparison normalization, and corresponding regression
tests. A fresh Windows run remains the authoritative final validation gate.
