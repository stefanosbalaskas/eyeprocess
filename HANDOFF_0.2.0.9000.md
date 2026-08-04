# eyeprocess 0.2.0.9000 handoff

This candidate adds empirical support for the supplied Gazepoint Analysis 7.2.0
export structure. The private raw corpus is distributed separately and must not
be committed to the public package repository.

## Git baseline

Before extracting this candidate over the package source, commit the validated
0.1.0.9003 baseline and create the feature branch:

```bat
cd /d C:\Users\Stefanos-PC\Documents\Rstudio\eyeprocess
git config --global user.email "s.balaskas@ac.upatras.gr"
git config --global user.name "Stefanos Balaskas"
git commit -m "Establish validated eyeprocess empirical-validation baseline"
git switch -c feature/real-gazepoint-validation
```

## Private corpus

The private corpus belongs at:

```text
C:\Users\Stefanos-PC\Documents\Rstudio\eyeprocess-validation-corpus
```

It contains six paired `_all_gaze.csv` and `_fixations.csv` exports and four
Gazepoint Data Summary reports. Do not add this directory to the package Git
repository.

## Validation

After installing 0.2.0.9000, run:

```r
source("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess/validate_eyeprocess.R")
source("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess/validate_gazepoint_real_exports.R")
source("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess/validate_real_exports.R")
```

The candidate is not a validated release until all three stages pass on the
user's Windows environment.
