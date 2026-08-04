## Development status

This is a development source tree and has not yet been submitted to CRAN.

The last fully validated baseline is eyeprocess 0.0.0.9004. On Windows 11 with
R 4.6.1 it installed, passed all unit tests, built and rebuilt all vignettes,
completed R CMD check with 0 errors, 0 warnings, and 0 notes, passed
`pkgdown::check_pkgdown()`, and passed runtime smoke tests.

Version 0.1.0.9003 adds empirical validation of real vendor export structures,
canonical round-trip evidence, compatibility corpora, conservative
anonymization, and shareable validation reports. Static source validation is
clean; a fresh runtime check of this feature release is pending.

## Scope

The package provides a vendor-neutral relational representation of eye-tracking,
pupil, event, response, and biometric process data, with first-class Gazepoint
support and optional psychometric modelling engines.

## Validation required before release

- Run the full test suite on Windows, macOS, and Linux.
- Run `R CMD check --as-cran` with current R and R-devel.
- Validate every dedicated adapter against multiple real exports and software
  versions; synthetic fixtures are not sufficient for compatibility claims.
- Review every generated validation bundle for study-specific disclosures even
  when automated anonymization is enabled.
- Complete parameter-recovery and missing-data simulations for experimental
  models.
- Confirm package-name, repository, site, and maintainer metadata before public
  release.
