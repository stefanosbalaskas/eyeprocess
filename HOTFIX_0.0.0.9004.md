# eyeprocess 0.0.0.9004 pkgdown-index hotfix

The 0.0.0.9003 Windows validation established that installation, the
full unit test suite, vignette construction and rebuilding, and
`R CMD check` all passed without errors, warnings, or notes. Validation
then stopped at
[`pkgdown::check_pkgdown()`](https://pkgdown.r-lib.org/reference/check_pkgdown.html)
because the documented topic `eyeprocess-package` was not listed in
`_pkgdown.yml`.

Version 0.0.0.9004 adds a dedicated **Package overview** reference
section:

``` yaml
reference:
- title: Package overview
  contents:
  - eyeprocess-package
```

The package topic remains publicly discoverable rather than being hidden
with `@keywords internal`. No runtime implementation changed.
