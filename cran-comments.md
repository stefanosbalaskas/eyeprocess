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

The first post-remediation merge was also certified on exact `master` commit
f7136626ec9a2f89351b3920160b16bbc3b588e5: workflow run 34746847489 passed
all five R CMD check jobs, and pkgdown run 34746847488 completed successfully.

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

## CRAN incoming pretest follow-up

The 13 September 2026 CRAN incoming pretest reported a top-level-files NOTE for
research/manuscript QA artifacts that were present in the repository but were
not intended to ship in the package tarball. The package build exclusions have
now been corrected in `.Rbuildignore` for every file named by the pretest:
`core_demo_audit.rds`, `eyeprocess_clean_render_check.txt`,
`eyeprocess_final_qa_report.txt`, `eyeprocess_function_demo_matrix.csv`,
`eyeprocess_literature_evidence.csv`, `eyeprocess_references.bib`,
`eyeprocess_search_record.csv`, `eyeprocess_software_manuscript.Rmd`,
`eyeprocess_software_manuscript.docx`, `eyeprocess_software_manuscript.html`,
`eyeprocess_v0.11.1_feature_inventory.csv`, and
`render_eyeprocess_manuscript.R`.

The incoming feasibility check also reports several domain-specific terms as
possibly misspelled. These are intentional terminology or proper names used in
eye-tracking and physiological-signal research: Gazepoint, pupillometry,
biometric/Biometrics, scanpath, timebase, resumable, transportability, and AOI
(the latter is expanded as area of interest on first use).

`cmdstanr` is an optional suggested dependency and is not hosted in the CRAN
main repository. `Additional_repositories` therefore points to the Stan
R-universe repository; the CRAN incoming pretest itself confirmed availability
of `cmdstanr` from that declared repository.
