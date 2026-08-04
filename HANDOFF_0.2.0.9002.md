# eyeprocess 0.2.0.9002 handoff

## Purpose

Resolve the final `R CMD check` WARNING caused solely by spaces in two packaged Gazepoint fixture filenames.

## Windows validation

Restart R, install the source tree, then run:

```r
source("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess/install_eyeprocess.R")
source("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess/validate_eyeprocess.R")
```

Target:

```text
0 errors | 0 warnings | 0 notes
pkgdown: No problems found
All requested local validation stages completed.
```

Then reconfirm the empirical corpus:

```r
source("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess/validate_gazepoint_real_exports.R")
source("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess/validate_real_exports.R")
```
