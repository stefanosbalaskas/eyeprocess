# Windows handoff: eyeprocess 0.3.0.9000

This candidate adds the complete real Gazepoint downstream workflow to
the runtime-validated 0.2.0.9002 baseline.

## 1. Install

Restart R with `Ctrl+Shift+F10`, extract the candidate over the existing
source directory, and install from the R Console:

``` r

package_zip <- file.choose()

unzip(
  package_zip,
  exdir = "path/to",
  overwrite = TRUE
)

source(
  "path/to/eyeprocess/install_eyeprocess.R"
)
```

Confirm that the installed version is `0.3.0.9000`.

## 2. Run the complete package gate

``` r

source(
  "path/to/eyeprocess/validate_eyeprocess.R"
)
```

The target is all unit tests passing, `R CMD check` with zero errors,
warnings, and notes, a clean pkgdown configuration, successful
installation, and runtime smoke tests.

## 3. Run the real six-user Gazepoint workflow

``` r

source(
  "path/to/eyeprocess/run_gazepoint_downstream_workflow.R"
)
```

Input:

``` text
path/to/eyeprocess-validation-corpus/cases/gazepoint-analysis-v7.2.0-demo
```

Output:

``` text
path/to/eyeprocess-downstream-output
```

The validated corpus should yield six recordings, twelve reconstructed
media trials, 7,340 gaze samples, 337 vendor fixations, 14,680 eye
samples, and 58,720 biometric observations. With no behavioural response
file, IRT readiness should be `process_ready_response_pending`; this is
intentional.

## 4. Review the outputs

Review:

``` text
gazepoint-workflow-report.html
gazepoint-workflow-report.md
workflow-validation.csv
canonical-validation-report.md
canonical-dataset/
qc/
tables/
irt/
plots/
workflow-result.rds
workflow-spec.rds
source-fingerprint.csv
session-info.txt
rerun-workflow.R
```

The workflow must not fabricate responses, scores, item parameters,
abilities, or psychological interpretations.
