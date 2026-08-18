# Validate an M3 simulation or fitted four-channel model

Combines structural support, sampler diagnostics, PPC, pupil-confound
availability, device/missingness summaries and explicit interpretive
boundaries. Validation of synthetic or computational behavior is not
empirical construct validation.

## Usage

``` r
validate_multimodal_m3(
  x,
  include_ppc = TRUE,
  rhat_max = 1.05,
  ess_min = 50,
  ebfmi_min = 0.3
)
```

## Arguments

- x:

  M3 simulation or fit.

- include_ppc:

  Include M3 PPC for fits.

- rhat_max, ess_min, ebfmi_min:

  Diagnostic thresholds.

## Value

An \`eye_multimodal_m3_validation\`.
