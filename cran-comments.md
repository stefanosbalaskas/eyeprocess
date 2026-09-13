## Test environments

* GitHub Actions: macOS latest, R release
* GitHub Actions: Windows latest, R release
* GitHub Actions: Ubuntu latest, R devel
* GitHub Actions: Ubuntu latest, R release
* GitHub Actions: Ubuntu latest, R oldrel-1

## R CMD check results

All five GitHub Actions R CMD check jobs completed successfully for the cleaned
0.11.1 resubmission candidate (workflow run 34723422665, certified package
commit 9ef09efb7626fd6ed8a390cb12e7233191624076).

## Resubmission

This is a resubmission of eyeprocess 0.11.1 addressing the CRAN review feedback.

* DESCRIPTION now expands item response theory (IRT), Brain Imaging Data
  Structure (BIDS), and area of interest (AOI) on first use.
* Exported-function and method return documentation was corrected at source
  level with `@return` documentation and regenerated into Rd files. The final
  static CRAN remediation audit confirms that usage-bearing Rd files are not
  missing `\value` sections. Return descriptions state class, structure, and
  meaning where applicable, with representative and high-risk return contracts
  reviewed against their implementations.
* Direct global-environment and manual RNG-state handling was removed or
  isolated. RNG scoping uses `withr::local_seed()`, and validation/report
  execution uses isolated environments rather than `.GlobalEnv`.
* The shipped installer was removed, and shipped package functions, examples,
  and vignettes do not install packages.

The final static CRAN remediation audit also completed successfully on the same
certified package commit (workflow run 34723422704).
