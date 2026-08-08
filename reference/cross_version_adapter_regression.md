# Compare adapter output across software/format versions

Compare adapter output across software/format versions

## Usage

``` r
cross_version_adapter_regression(
  input,
  baseline_adapter,
  candidate_adapter,
  baseline_version = "baseline",
  candidate_version = "candidate",
  extract_samples = function(x) {
     if (is.data.frame(x))
         x
     else if
(is.list(x) && !is.null(x$samples))
x$samples
     else
    stop("Define `extract_samples` for this adapter output.", call. = FALSE)
 },
  audit_args = list()
)
```

## Arguments

- input:

  Shared raw fixture/input.

- baseline_adapter, candidate_adapter:

  Functions that parse \`input\`.

- baseline_version, candidate_version:

  Version labels.

- extract_samples:

  Function extracting comparable sample tables.

- audit_args:

  Arguments passed to \`field_fidelity_report()\`.
