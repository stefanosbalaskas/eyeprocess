# Install `eyeprocess` on Windows

Place the source folder at:

```text
path/to/eyeprocess
```

## RStudio Console

```r
source("path/to/eyeprocess/install_eyeprocess.R")
source("path/to/eyeprocess/validate_eyeprocess.R")
```

## PowerShell

```powershell
Set-ExecutionPolicy -Scope Process Bypass
& "path/to/eyeprocess\install_eyeprocess.ps1"
```

## Development commands

```r
setwd("path/to/eyeprocess")
devtools::load_all()
devtools::test()
devtools::check(document = FALSE)
pkgdown::check_pkgdown()
pkgdown::build_site()
```

## Optional model engines

```r
install.packages(c("mirt", "TAM", "LNIRT", "lme4"))
```

`brms` is optional and requires an appropriate Stan toolchain. EyeLink EDF files
require SR Research's local `edf2asc` converter; ASC and Data Viewer exports can
be read without that bridge.

## Initialize empirical export validation

After the package validation succeeds:

```r
library(eyeprocess)

init_validation_corpus(
  "path/to/eyeprocess-validation-corpus"
)
```

Add only de-identified exports, complete the generated manifest, and run the
repository-level `validate_real_exports.R` script. Keep the corpus outside
public version control and manually review every generated bundle before
sharing it.
