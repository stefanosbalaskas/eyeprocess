# Audit M4 structural and posterior identifiability

Evaluates sequence information, channel support, state occupancy,
assignment uncertainty, state separation, transition degeneracy, and HMC
diagnostics. The result is deliberately multi-criterion; M4 does not
collapse validity to a single undocumented boolean.

## Usage

``` r
audit_multimodal_m4_identifiability(
  x,
  spec = NULL,
  include_posterior = TRUE,
  rhat_max = 1.05,
  ess_min = 100,
  ebfmi_min = 0.3,
  occupancy_min = 0.03,
  entropy_fraction_review = 0.8
)
```

## Arguments

- x:

  Data, M4 simulation, M4 fit, or internal prepared M4 data.

- spec:

  Optional M4 specification when \`x\` is not a fit.

- include_posterior:

  Whether to evaluate posterior criteria for a fit.

- rhat_max, ess_min, ebfmi_min:

  Sampler thresholds.

- occupancy_min:

  Minimum mean posterior state occupancy for non-null K.

- entropy_fraction_review:

  Review threshold relative to maximum entropy.

## Value

An \`eye_multimodal_m4_identifiability\` with machine-readable checks.
