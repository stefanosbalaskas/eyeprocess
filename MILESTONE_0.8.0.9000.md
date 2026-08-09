# eyeprocess 0.8.0.9000 milestone

## Status

**Closed, frozen, merged, and deployed.**

## Source lineage

- Validated feature commit: `b762f95e4aacb419cb875b793880632246c853fd`
- Pull request: \#9 — Add process measurement and deployment governance
- Master merge commit: `4c54bc9a814035bb1ab8b554974749259d76afc0`
- pkgdown/gh-pages deployment commit:
  `303f9159ac15edfb4d6415525f72bab77f54998f`

## Scope

- Biometric/process pre-flight governance and exclusion manifests.
- Deployment drift across device, site, vendor, and stimulus version.
- Temporal process windows and AOI trajectory modelling.
- Advanced pupil frequency/activity, event deconvolution, confound
  adjustment, fatigue audits, and signal filtering.
- Visual-context/testlet IRT.
- Multiblock process representations.
- Process-profile mixtures and external-validity workflows.
- Streaming scoring and validation bundles.
- Item-parameter seeding and candidate-bank auditing.
- Presentation/accessibility sensitivity workflows.
- Process-decision proxy features.
- Advanced Rasch, mixture, imputation, Bayesian, and gaze-aware 3PL
  diagnostics.
- Explicit frontier-estimator gates.
- 11 new pkgdown articles and corresponding reference documentation.

## Validation checkpoint

- Focused 0.8 test files: 8/8 PASS.
- Complete eyeprocess test suite: PASS.
- [`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html):
  PASS.
- `R CMD check`: 0 errors, 0 warnings, 0 notes.
- Installation with vignettes: PASS.
- Installed version: `0.8.0.9000`.
- Representative installed API audit: PASS.
- Pre-flight specification contract: PASS.
- Frontier gating contract: PASS.
- `git diff --check`: PASS.
- GitHub PR \#9: 8/8 checks completed successfully.
- Public 0.8 article/reference surface: PASS.

## Repository freeze

- `master` = `origin/master` =
  `4c54bc9a814035bb1ab8b554974749259d76afc0` at milestone closure.
- `gh-pages` = `origin/gh-pages` =
  `303f9159ac15edfb4d6415525f72bab77f54998f` at deployment closure.
- Working tree was clean.
- Feature branch was removed after merge.

## Scientific governance

Process evidence is not interpreted automatically as a mental-state,
clinical, ability, or misconduct diagnosis.

Frontier methods without an exact validated implementation remain
explicitly gated rather than silently substituting a simpler estimator.

Experimental and user-supplied model specifications retain conservative
maturity labels.
