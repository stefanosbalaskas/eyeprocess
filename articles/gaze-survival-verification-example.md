# Worked example: evidence verification latency

## Question

This example models **time to first entry into a source/evidence AOI**.
Repeated trials occur under `standard` and `evidence_prompt` conditions.
Complete usable trials that never inspect the evidence AOI remain in the
risk set until the observation window ends and are right censored.

## Generate raw trial/event inputs

``` r

raw <- simulate_gaze_survival_inputs(
  "verification",
  seed = 20260918,
  n_participants = 36,
  trials_per_participant = 3
)
```

The synthetic input is deliberately not pre-censored. The survival state
is constructed from the event definition and the explicit trial
observation window.

## Construct and validate the survival table

``` r

verification <- prepare_gaze_survival_data(
  raw$trials,
  raw$events,
  target_aoi = "source_evidence",
  event_type = "first_aoi_entry",
  condition_col = "condition_id",
  min_valid_fraction = .90,
  time_origin = "trial_start",
  event_detector = "synthetic_truth",
  aoi_specification = "fixed synthetic source/evidence AOI"
)

validate_gaze_survival_data(verification, raise_on_error = FALSE)
```

    ## [1] severity code     n        message 
    ## <0 rows> (or 0-length row.names)

``` r

summarise_gaze_censoring(verification, by = "condition")
```

    ##         condition n_trials n_analyzable n_observed_events n_censored
    ## 1 evidence_prompt       54           54                53          1
    ## 2        standard       54           54                49          5
    ##   n_review_required censoring_fraction
    ## 1                 0         0.01851852
    ## 2                 0         0.09259259

A missing target event becomes ordinary right censoring only when the
observation window is complete and usable. Unresolved or poor-quality
trials remain review cases.

## Kaplan-Meier description

``` r

plot_gaze_survival_curve(verification, group = "condition")
```

![Kaplan-Meier curves for time to first source/evidence AOI
entry.](gaze-survival-verification-example_files/figure-html/km-1.png)

Kaplan-Meier curves for time to first source/evidence AOI entry.

Lower survival at a given time means a larger proportion of trials has
already inspected the evidence AOI.

## Complementary event-incidence view

``` r

plot_gaze_cumulative_incidence(verification, group = "condition")
```

![Single-event 1-KM display for time to first source/evidence AOI
entry.](gaze-survival-verification-example_files/figure-html/incidence-1.png)

Single-event 1-KM display for time to first source/evidence AOI entry.

This is the complement of the Kaplan-Meier survival curve for one target
event. Do not interpret it as a competing-risks cumulative-incidence
function when multiple mutually exclusive events compete.

## Empirical hazard/risk increments

``` r

plot_gaze_hazard(verification, group = "condition")
```

![Empirical event/risk increments for the evidence-verification
example.](gaze-survival-verification-example_files/figure-html/hazard-1.png)

Empirical event/risk increments for the evidence-verification example.

This plot is most useful as a diagnostic view of when event increments
occur relative to the risk set; it should not be oversold as a smooth
underlying hazard function.

## Repeated-participant Cox model

``` r

cox <- fit_gaze_mixed_cox_model(
  verification,
  "condition",
  participant_col = "participant_id",
  structure = "cluster_robust"
)

tidy_gaze_survival_model(cox)
```

    ##                term estimate_log_scale std_error hazard_ratio  conf_low
    ## 1 conditionstandard          -1.087844 0.2157481    0.3369423 0.2207549
    ##   conf_high statistic      p_value effect_measure
    ## 1 0.5142812 -5.042194 4.602255e-07   hazard_ratio

``` r

check_gaze_proportional_hazards(cox)
```

    ##        term rho    chisq    p_value alpha ph_flag
    ## 1 condition  NA 6.369666 0.01160874  0.05    TRUE
    ## 2    GLOBAL  NA 6.369666 0.01160874  0.05    TRUE

The hazard ratio is an instantaneous evidence-inspection-rate contrast
among trials still at risk. It is not a ratio of mean latencies.

## Explicit AFT sensitivity models

``` r

weibull <- fit_gaze_aft_model(
  verification,
  "condition",
  distribution = "weibull"
)
lognormal <- fit_gaze_aft_model(
  verification,
  "condition",
  distribution = "lognormal"
)

compare_gaze_survival_models(cox, weibull, lognormal)
```

    ## Warning: Information criteria are not directly comparable across Cox partial
    ## likelihood, coxme penalized frailty likelihood, and AFT full likelihood, or
    ## across different analysis-row counts. Use diagnostics and estimand-specific
    ## interpretation instead of ranking by AIC/BIC.

    ##     model        family           backend    logLik      AIC      BIC   n
    ## 1 model_1  cox_repeated   survival::coxph -381.0763 764.1526 766.7776 102
    ## 2 model_2   aft_weibull survival::survreg -138.6049 283.2098 291.2562 108
    ## 3 model_3 aft_lognormal survival::survreg -140.0168 286.0335 294.0799 108
    ##         likelihood_basis information_criteria_comparable
    ## 1 cox_partial_likelihood                           FALSE
    ## 2        full_likelihood                           FALSE
    ## 3        full_likelihood                           FALSE

Interpret the AFT effects as time ratios under the named distribution.
Do not select Weibull versus log-normal because one gives a more
favorable p-value.

## Troubleshooting clinic

Stop and resolve the scientific/data contract rather than silently
modifying rows when:

- the observation window is missing or incomplete;
- event time exceeds the censoring limit;
- the target is already present at time zero and the time origin is
  ambiguous;
- gaze quality is below the declared rule;
- event counts are too sparse for stable inference;
- proportional-hazards diagnostics are flagged;
- competing events require a competing-risks estimand.

For `coxme` frailty fits, report a corresponding marginal Cox PH
diagnostic separately rather than pretending `cox.zph()` applies to the
frailty object.

## Reporting example

> Evidence-inspection latency was analysed as a right-censored
> time-to-event outcome. The target event was first entry into the
> source/evidence AOI from trial onset. Complete usable trials without
> an evidence entry were retained as right-censored observations;
> unresolved gaze-quality trials were not recoded as censoring. We
> fitted a participant-clustered Cox model and reported hazard ratios
> with 95% confidence intervals. Proportional-hazards diagnostics and
> pre-specified Weibull/log-normal AFT sensitivity models were examined.

Report participants, trials, events, censored trials and percentage,
review-required rows, event/AOI/time-origin definitions, gaze-quality
rule, repeated-participant structure, effect estimate with 95% CI,
diagnostics, sensitivity analyses, and package/backend versions.

## API links

Preparation and validation:
[`prepare_gaze_survival_data()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`validate_gaze_survival_data()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`summarise_gaze_censoring()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md).

Models and diagnostics:
[`fit_gaze_mixed_cox_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`fit_gaze_aft_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`check_gaze_proportional_hazards()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`compare_gaze_survival_models()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md).

Reporting and visualization:
[`tidy_gaze_survival_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`report_gaze_survival_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`plot_gaze_survival_curve()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md).
