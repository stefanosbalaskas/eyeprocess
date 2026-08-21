# M2 Three-Way Reference Model: Response, RT, and Gaze

## Scope

The M2 milestone implements a likelihood-faithful three-way
response-process model motivated by Man, Harring, and Zhan (2022), DOI
`10.1177/01466216221089344`. The measurement layer combines a Rasch
response model, a lognormal response-time model, and a negative-binomial
fixation-count model. Person-side ability, speed, and gaze-process
effects are correlated; item difficulty, time intensity, and gaze
intensity are also correlated.

The implementation deliberately separates **likelihood fidelity** from
**prior fidelity**. `prior_profile = "regularized"` uses regularizing
Stan priors. `"paper_centered"` centers priors near values described in
the source literature but is not claimed to reproduce every published
hyperprior exactly.

No process channel is automatically interpreted as attention, cognitive
load, guessing, strategy, or difficulty.

## Canonical specification

``` r

library(eyeprocess)
#> eyeprocess 0.11.0: vendor-neutral eye/process data harmonization with first-class Gazepoint support.

spec <- multimodal_m2_spec()
spec
#> <eye_multimodal_m2_spec>
#>   model: M2 response + RT + gaze
#>   backend: cmdstanr
#>   likelihood: Rasch + lognormal RT + negative-binomial gaze
#>   reference DOI: 10.1177/01466216221089344
#>   prior profile: regularized
#>   missingness: ignorable
#>   lifecycle: experimental
#>   boundary: process channels are observations, not automatic psychological constructs
```

The object is not a new specification ecosystem. It inherits the
existing `eye_multimodal_irt_spec` and `eye_irt_model_spec` architecture
and composes the established `irt_*_channel()` constructors.

## Simulation

``` r

sim <- simulate_multimodal_m2(
  n_person = 80,
  n_item = 10,
  seed = 20260814
)

head(sim$data)
#>   person_id item_id response        rt gaze
#> 1     P0001    I001        1  2.978669  161
#> 2     P0002    I001        1 76.968523  114
#> 3     P0003    I001        0 48.517830   29
#> 4     P0004    I001        1 23.997114   64
#> 5     P0005    I001        0 71.585599   58
#> 6     P0006    I001        1 26.440118   63
sim
#> <eye_multimodal_m2_simulation>
#>   persons: 80
#>   items: 10
#>   rows: 800
#>   dropout: response=0.000, rt=0.000, gaze=0.000
#>   seed: 20260814
#>   generating likelihood: response-Rasch + lognormal-RT + NB-fixation
```

The complete generating data and all latent truth are retained
separately in `sim$complete_data` and `sim$truth`.

## Structural audit

``` r

audit <- audit_multimodal_m2_identifiability(sim$data)
audit
#> <eye_multimodal_m2_identifiability>
#>   model: M2
#>   persons: 80
#>   items: 10
#>   supported: TRUE
#>   missing fractions: response=0.000, rt=0.000, gaze=0.000
#>   boundary: This audit is a conservative structural/data-support screen. It does not establish global identifiability, construct validity, or robustness to MNAR channel missingness.
```

The audit is a conservative pre-fit screen. Passing it does not prove
global identifiability, empirical adequacy, or construct validity.

## CmdStan fit

The estimator is gated. It fails explicitly if CmdStanR/CmdStan are
unavailable and does not substitute another engine.

``` r

fit <- fit_multimodal_m2(
  sim,
  chains = 4,
  parallel_chains = 4,
  iter_warmup = 1000,
  iter_sampling = 1000,
  seed = 9001
)

summary(fit)
plot(fit, type = "person_correlations")
plot(fit, type = "item_correlations")
plot(fit, type = "item_parameters")
```

## Identification

The reference implementation fixes the person latent means to zero,
fixes the response discrimination to one, uses a fixed `-1` speed
loading in the log-time mean, and a fixed `+1` gaze-process loading in
the negative-binomial log mean. These constraints define the channel
scales. The structural covariance parameters remain estimated.

## Missingness

Channel-specific missing observations are omitted from that channel
likelihood under an explicit **ignorable missingness** assumption. This
does not validate MAR/MNAR assumptions. Informative channel dropout
belongs to a separate missingness model and sensitivity analysis.

## Interpretation boundary

The M2 estimator answers a measurement question: whether a joint
probabilistic model can use response, RT, and gaze-count observations
coherently. Psychological labels require external design and validation
evidence.
