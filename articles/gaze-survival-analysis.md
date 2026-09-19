# Survival Analysis for Gaze Latency

## Why TTFF is a censored-data problem

Time to first fixation, first AOI entry, evidence inspection, revisit,
transition latency, and substantively defined disengagement are
time-to-event outcomes. A valid trial that ends before the target event
occurs still contributes information up to the end of its risk window.
`eyeprocess` therefore retains that trial as a **right-censored
observation** rather than dropping it or assigning a latency of zero.

A second distinction is essential: **target not observed during a
complete usable trial** is not the same as **target status unknown
because gaze became unusable or the observation window is incomplete**.
The latter remains a review row with an unknown event indicator; it is
not silently counted as ordinary censoring.

The canonical table is one row per participant-trial and contains
participant, trial, stimulus, condition, target AOI, time origin,
event/censor time, analysis time, event indicator, event definition,
valid-sample count, valid-data fraction, and trial duration. Additional
governance fields retain censoring reason, join identity, event
detector, AOI specification, quality rule, preprocessing specification,
source, and software version.

## When to use this method

Use survival analysis when the scientific outcome is a genuine event
latency and some valid trials end before the event is observed. Typical
targets include first disclosure fixation, first source/evidence
inspection, first revisit, or first transition into a target AOI.

Do **not** use this workflow when event status cannot be determined,
risk windows are unknown, competing event types require a
competing-risks estimand, or the scientific question concerns the
complete continuous gaze trajectory rather than an event time.
Informative trial termination requires additional sensitivity analysis;
ordinary right censoring is not automatically justified by the fact that
a recording ended.

## Worked example: disclosure inspection

The synthetic generator deliberately returns trial windows and event
rows first. Censoring is therefore constructed visibly rather than
hidden in a prebuilt survival table.

``` r

raw <- simulate_gaze_survival_inputs(
  "disclosure",
  seed = 20260918,
  n_participants = 36,
  trials_per_participant = 3
)
head(raw$trials)
#>   recording_id participant_id trial_id stimulus_id        condition_id
#> 1         R001           P001 P001_T01         S01             control
#> 2         R001           P001 P001_T02         S02  minimal_disclosure
#> 3         R001           P001 P001_T03         S03 detailed_disclosure
#> 4         R002           P002 P002_T01         S01  minimal_disclosure
#> 5         R002           P002 P002_T02         S02 detailed_disclosure
#> 6         R002           P002 P002_T03         S03             control
#>   start_time end_time n_valid_samples valid_data_fraction
#> 1          0        4             221           0.9423942
#> 2          0        4             233           0.9775320
#> 3          0        4             239           0.9462968
#> 4          0        4             222           0.9944168
#> 5          0        4             234           0.9563629
#> 6          0        4             238           0.9947536
#>   observation_end_reason
#> 1    scheduled_trial_end
#> 2    scheduled_trial_end
#> 3    scheduled_trial_end
#> 4    scheduled_trial_end
#> 5    scheduled_trial_end
#> 6    scheduled_trial_end
head(raw$events)
#>   recording_id trial_id start_time     aoi_id episode_type
#> 1         R001 P001_T01  0.3500000       body     fixation
#> 2         R001 P001_T01  2.5891246 disclosure     fixation
#> 3         R001 P001_T02  0.3900000       body     fixation
#> 4         R001 P001_T02  0.8711407 disclosure     fixation
#> 5         R001 P001_T03  0.4300000       body     fixation
#> 6         R001 P001_T03  2.2208362 disclosure     fixation
```

Canonical event tables can be joined by `participant_id + trial_id` when
participant identity is present, or by `recording_id + trial_id` when
event rows carry recording identity instead. Trial-only matching is
accepted only when trial IDs are globally unique. Ambiguous identity is
an error.

``` r

surv_data <- prepare_gaze_survival_data(
  raw$trials,
  raw$events,
  target_aoi = "disclosure",
  event_type = "first_fixation",
  condition_col = "condition_id",
  observation_end_reason_col = "observation_end_reason",
  min_valid_fraction = .90,
  time_origin = "trial_start",
  source_data = "synthetic_disclosure_trial_event_inputs",
  preprocessing_specification = "synthetic_truth_no_filtering",
  event_detector = "synthetic_truth",
  aoi_specification = "fixed synthetic disclosure AOI",
  quality_rules = list(minimum_fixation_ms = 80, valid_fraction_min = .90)
)

summarise_gaze_censoring(surv_data, by = "condition")
#>             condition n_trials n_analyzable n_observed_events n_censored
#> 1             control       36           36                22         14
#> 2 detailed_disclosure       36           36                36          0
#> 3  minimal_disclosure       36           36                34          2
#>   n_review_required censoring_fraction
#> 1                 0         0.38888889
#> 2                 0         0.00000000
#> 3                 0         0.05555556
```

A non-default time origin must be explicit. For example, latency from
stimulus onset should be constructed with
`time_origin = "stimulus_onset"` and
`time_origin_col = "stimulus_onset"`; the censoring window is then
measured from that same origin.

## Worked variant: evidence verification

The second synthetic scenario models **time to first entry into a
source/evidence AOI** under repeated `standard` and `evidence_prompt`
trials. Some complete usable trials never inspect the evidence region,
so those rows remain right-censored.

``` r

verify_raw <- simulate_gaze_survival_inputs(
  "verification",
  seed = 20260918,
  n_participants = 36,
  trials_per_participant = 3
)

verification <- prepare_gaze_survival_data(
  verify_raw$trials,
  verify_raw$events,
  target_aoi = "source_evidence",
  event_type = "first_aoi_entry",
  condition_col = "condition_id",
  min_valid_fraction = .90,
  time_origin = "trial_start",
  event_detector = "synthetic_truth",
  aoi_specification = "fixed synthetic source/evidence AOI"
)

summarise_gaze_censoring(verification, by = "condition")
#>         condition n_trials n_analyzable n_observed_events n_censored
#> 1 evidence_prompt       54           54                53          1
#> 2        standard       54           54                49          5
#>   n_review_required censoring_fraction
#> 1                 0         0.01851852
#> 2                 0         0.09259259
```

This analysis retains the observed risk time of never-inspected trials
rather than reducing the question to a complete-case latency comparison.

## Event semantics

`event_type` supports first fixation, first AOI entry, first evidence
inspection, first revisit, first transition into the target, and
disengagement. Revisit/transition/disengagement are **visit-level**
concepts: consecutive identical AOI labels are collapsed before those
event times are identified, so two consecutive fixations within one
visit do not create a false revisit.

### Censoring decision guide

| Trial state | `event_observed` | `analysis_time` | Model eligible? | Interpretation |
|:---|---:|---:|:---|:---|
| Target event occurs inside a usable window | 1 | Event latency | Yes | Observed event |
| Target never occurs before a complete usable window ends | 0 | Censoring time | Yes | Right censored |
| Window incomplete or event status unknowable | NA | NA | No | Review required |
| Gaze quality fails the declared rule | NA | NA | No | Review/exclusion branch |
| Event occurs after the censoring limit | Invalid | Invalid | No | Data-contract error |

The last three states are never silently recoded as ordinary censoring.

## Kaplan–Meier description

``` r

plot_gaze_survival_curve(surv_data, group = "condition")
```

![Kaplan–Meier curves for time to first disclosure fixation in the
synthetic
example.](gaze-survival-analysis_files/figure-html/km-plot-1.png)

Kaplan–Meier curves for time to first disclosure fixation in the
synthetic example.

The survival curve is the probability that the target event has **not
yet** occurred.
[`plot_gaze_cumulative_incidence()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md)
displays `1 - KM` for this single-event setting. It should not be
described as a competing-risks cumulative incidence function when
multiple event types compete.

## Complementary survival visualizations

``` r

plot_gaze_cumulative_incidence(surv_data, group = "condition")
```

![Single-event 1-KM display for time to first disclosure
fixation.](gaze-survival-analysis_files/figure-html/cumulative-incidence-plot-1.png)

Single-event 1-KM display for time to first disclosure fixation.

The 1-KM display answers the same single-event question from the
opposite direction: the proportion of trials for which the target event
has already occurred by time `t`. It is useful for communication, but it
is **not** a competing-risks cumulative-incidence estimator.

``` r

plot_gaze_hazard(surv_data, group = "condition")
```

![Empirical event/risk increments over time in the synthetic disclosure
example.](gaze-survival-analysis_files/figure-html/empirical-hazard-plot-1.png)

Empirical event/risk increments over time in the synthetic disclosure
example.

The hazard view is a descriptive event/risk increment display at
observed event times. Treat it as a diagnostic visualization rather than
a smoothed continuous-time hazard estimate.

## Cox versus AFT models

Estimator choice is explicit.
[`fit_gaze_mixed_cox_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md)
requires `structure = "cluster_robust"` or `"frailty"`;
[`fit_gaze_aft_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md)
requires `distribution = "weibull"` or `"lognormal"`. Both arguments are
required so the package never chooses an estimator silently.

``` r

cox_clustered <- fit_gaze_mixed_cox_model(
  surv_data,
  "condition",
  participant_col = "participant_id",
  structure = "cluster_robust"
)
weibull <- fit_gaze_aft_model(surv_data, "condition", distribution = "weibull")
lognormal <- fit_gaze_aft_model(surv_data, "condition", distribution = "lognormal")

tidy_gaze_survival_model(cox_clustered)
#>                           term estimate_log_scale std_error hazard_ratio
#> 1 conditiondetailed_disclosure           1.377500 0.2344998     3.964979
#> 2  conditionminimal_disclosure           1.231458 0.2426759     3.426220
#>   conf_low conf_high statistic      p_value effect_measure
#> 1 2.504000  6.278378  5.874207 4.248714e-09   hazard_ratio
#> 2 2.129361  5.512916  5.074495 3.885256e-07   hazard_ratio
tidy_gaze_survival_model(weibull)
#>                           term estimate_log_scale  std_error time_ratio
#> 1                  (Intercept)          1.3856764 0.07915587  3.9975290
#> 2 conditiondetailed_disclosure         -0.5635307 0.10165472  0.5691958
#> 3  conditionminimal_disclosure         -0.4914879 0.10340637  0.6117156
#>    conf_low conf_high statistic      p_value effect_measure
#> 1 3.4230561 4.6684124 17.505669 1.296952e-68     time_ratio
#> 2 0.4663726 0.6946889 -5.543577 2.963544e-08     time_ratio
#> 3 0.4994935 0.7491507 -4.752975 2.004453e-06     time_ratio
```

A Cox coefficient is reported as a **hazard ratio**: a relative
instantaneous event rate among trials still at risk, not a ratio of mean
TTFF. An AFT coefficient is reported as a **time ratio** and describes
multiplicative event time under the specified distribution. Information
criteria are not treated as rank-comparable across ordinary Cox partial
likelihood, `coxme` penalized frailty likelihood, and AFT full
likelihood.

### Cross-language backend contract

R delegates marginal Cox to
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html),
Gaussian participant frailty to
[`coxme::coxme`](https://rdrr.io/pkg/coxme/man/coxme.html), and
Weibull/log-normal AFT to
[`survival::survreg`](https://rdrr.io/pkg/survival/man/survreg.html).
Python delegates Cox to `statsmodels.PHReg` and parametric AFT to
`lifelines`. The two packages align the censoring contract, estimator
choice, estimand interpretation, provenance, and semantic output fields.
Exact coefficients are not required to be numerically identical when
backend parameterizations or optimizers differ; each language is
regression-tested against its own specialist backend.

[`compare_gaze_survival_models()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md)
never declares a preferred estimator. Cox partial-likelihood, mixed-Cox
penalized partial-likelihood, and AFT full-likelihood information
criteria are flagged as non-comparable across those likelihood bases.

``` r

compare_gaze_survival_models(cox_clustered, weibull, lognormal)
#>     model        family           backend    logLik      AIC      BIC   n
#> 1 model_1  cox_repeated   survival::coxph -354.7486 713.4971 718.5407  92
#> 2 model_2   aft_weibull survival::survreg -135.4916 278.9831 289.7117 108
#> 3 model_3 aft_lognormal survival::survreg -135.0641 278.1282 288.8567 108
#>         likelihood_basis information_criteria_comparable
#> 1 cox_partial_likelihood                           FALSE
#> 2        full_likelihood                           FALSE
#> 3        full_likelihood                           FALSE
```

## Repeated participants: clustered versus frailty Cox

`structure = "cluster_robust"` fits a marginal Cox model with
participant-clustered sandwich uncertainty through
[`survival::coxph`](https://rdrr.io/pkg/survival/man/coxph.html).
`structure = "frailty"` is a **different estimator** and delegates to
the specialist
[`coxme::coxme`](https://rdrr.io/pkg/coxme/man/coxme.html) mixed-effects
Cox engine with a Gaussian participant random intercept. The package
never silently substitutes one for the other.

``` r

frailty <- fit_gaze_mixed_cox_model(
  surv_data,
  "condition",
  participant_col = "participant_id",
  structure = "frailty"
)
tidy_gaze_survival_model(frailty)
#>                           term estimate_log_scale std_error hazard_ratio
#> 1 conditiondetailed_disclosure           1.536285 0.2993990     4.647293
#> 2  conditionminimal_disclosure           1.476000 0.2983902     4.375410
#>   conf_low conf_high statistic      p_value effect_measure
#> 1 2.584349  8.356971  5.131229 2.878564e-07   hazard_ratio
#> 2 2.437971  7.852518  4.946545 7.554237e-07   hazard_ratio
```

## Diagnostics

``` r

check_gaze_proportional_hazards(cox_clustered)
#>        term rho    chisq   p_value alpha ph_flag
#> 1 condition  NA 2.636062 0.2676618  0.05   FALSE
#> 2    GLOBAL  NA 2.636062 0.2676618  0.05   FALSE
```

[`check_gaze_proportional_hazards()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md)
uses
[`survival::cox.zph()`](https://rdrr.io/pkg/survival/man/cox.zph.html)
for `coxph` models. The helper does not pretend that this is a
frailty-specific diagnostic for `coxme`; for a frailty analysis, report
a corresponding marginal Cox PH diagnostic separately and make that
distinction explicit.

AFT models require strictly positive analysis times. A target already
fixated at time zero should trigger review of the time origin and
sampling-resolution rule rather than an automatic numeric offset.

## Model-choice quick guide

| Scientific question | Recommended family | Interpretation |
|:---|:---|:---|
| What proportion remains uninspected over time? | Kaplan-Meier | Probability the target event has not yet occurred |
| How does condition alter the instantaneous event rate? | Cox PH | Hazard ratio |
| How does condition multiply event time? | Weibull/log-normal AFT | Time ratio |
| How should repeated trials be handled marginally? | Clustered Cox | Population-average hazard ratio with participant-clustered uncertainty |
| How should latent participant heterogeneity be modelled? | `coxme` frailty Cox | Conditional hazard ratio with participant random effect |

Do not choose between Cox and AFT by whichever produces the smaller
p-value. State the estimand first, inspect diagnostics, and treat
alternative model families as pre-specified sensitivity analyses where
appropriate.

## Sensitivity analysis

Rebuild the survival table across defensible AOI geometries, event
detectors, trial origins, fixation-duration thresholds, and quality
rules. Then fit explicitly named model families across those branches.

``` r

expanded <- surv_data
expanded$aoi_specification <- "synthetic expanded disclosure AOI"

sensitivity <- compare_gaze_survival_specifications(
  list(primary = surv_data, expanded_aoi_demo = expanded),
  "condition",
  model_families = c("cox_cluster_robust", "aft_weibull")
)
head(sensitivity)
#>       specification model_family                         term
#> 1           primary cox_repeated conditiondetailed_disclosure
#> 2           primary cox_repeated  conditionminimal_disclosure
#> 3           primary  aft_weibull                  (Intercept)
#> 4           primary  aft_weibull conditiondetailed_disclosure
#> 5           primary  aft_weibull  conditionminimal_disclosure
#> 6 expanded_aoi_demo cox_repeated conditiondetailed_disclosure
#>   estimate_log_scale  std_error hazard_ratio  conf_low conf_high statistic
#> 1          1.3775004 0.23449980     3.964979 2.5039996 6.2783776  5.874207
#> 2          1.2314577 0.24267589     3.426220 2.1293605 5.5129161  5.074495
#> 3          1.3856764 0.07915587           NA 3.4230561 4.6684124 17.505669
#> 4         -0.5635307 0.10165472           NA 0.4663726 0.6946889 -5.543577
#> 5         -0.4914879 0.10340637           NA 0.4994935 0.7491507 -4.752975
#> 6          1.3775004 0.23449980     3.964979 2.5039996 6.2783776  5.874207
#>        p_value effect_measure n_trials n_observed_events n_censored
#> 1 4.248714e-09   hazard_ratio      108                92         16
#> 2 3.885256e-07   hazard_ratio      108                92         16
#> 3 1.296952e-68     time_ratio      108                92         16
#> 4 2.963544e-08     time_ratio      108                92         16
#> 5 2.004453e-06     time_ratio      108                92         16
#> 6 4.248714e-09   hazard_ratio      108                92         16
#>   censoring_fraction  event_detector                 aoi_specification
#> 1          0.1481481 synthetic_truth    fixed synthetic disclosure AOI
#> 2          0.1481481 synthetic_truth    fixed synthetic disclosure AOI
#> 3          0.1481481 synthetic_truth    fixed synthetic disclosure AOI
#> 4          0.1481481 synthetic_truth    fixed synthetic disclosure AOI
#> 5          0.1481481 synthetic_truth    fixed synthetic disclosure AOI
#> 6          0.1481481 synthetic_truth synthetic expanded disclosure AOI
#>                                                          quality_rules
#> 1 min_valid_fraction=0.9;minimum_fixation_ms=80;valid_fraction_min=0.9
#> 2 min_valid_fraction=0.9;minimum_fixation_ms=80;valid_fraction_min=0.9
#> 3 min_valid_fraction=0.9;minimum_fixation_ms=80;valid_fraction_min=0.9
#> 4 min_valid_fraction=0.9;minimum_fixation_ms=80;valid_fraction_min=0.9
#> 5 min_valid_fraction=0.9;minimum_fixation_ms=80;valid_fraction_min=0.9
#> 6 min_valid_fraction=0.9;minimum_fixation_ms=80;valid_fraction_min=0.9
#>    preprocessing_specification time_origin time_ratio
#> 1 synthetic_truth_no_filtering trial_start         NA
#> 2 synthetic_truth_no_filtering trial_start         NA
#> 3 synthetic_truth_no_filtering trial_start  3.9975290
#> 4 synthetic_truth_no_filtering trial_start  0.5691958
#> 5 synthetic_truth_no_filtering trial_start  0.6117156
#> 6 synthetic_truth_no_filtering trial_start         NA
```

The relabelled branch above demonstrates the output contract only. A
real AOI sensitivity analysis should reconstruct the event table from
the alternative AOI geometry rather than merely change its metadata.

## Failure case: unusable gaze is not censoring

``` r

bad_trials <- raw$trials
bad_trials$valid_data_fraction[1] <- .20
reviewed <- suppressWarnings(prepare_gaze_survival_data(
  bad_trials,
  raw$events,
  target_aoi = "disclosure",
  min_valid_fraction = .90
))
reviewed[1, c("event_observed", "censor_reason", "review_required")]
#>   event_observed         censor_reason review_required
#> 1             NA unusable_gaze_quality            TRUE
```

That row must be resolved or handled in an explicit
sensitivity/exclusion branch before model fitting. The model helpers
refuse unresolved review rows rather than silently dropping them.

## Reporting guidance

At minimum report participants, trials, observed events, censored trials
and percentage, review-required rows, target event, target AOI, time
origin, observation-window definition, event detector, fixation-duration
rule, quality rule, model family, hazard ratio or time ratio with 95%
CI, repeated-participant structure, diagnostics, and sensitivity
analyses.

``` r

report <- report_gaze_survival_model(cox_clustered)
report[c(
  "N_participants", "N_trials", "N_observed_events",
  "N_censored_trials", "N_review_required", "censoring_percentage",
  "event_type", "target_aoi", "time_origin", "model_family",
  "random_or_frailty_structure", "diagnostic_result"
)]
#> $N_participants
#> [1] 36
#> 
#> $N_trials
#> [1] 108
#> 
#> $N_observed_events
#> [1] 92
#> 
#> $N_censored_trials
#> [1] 16
#> 
#> $N_review_required
#> [1] 0
#> 
#> $censoring_percentage
#> [1] 14.81481
#> 
#> $event_type
#> [1] "first_fixation"
#> 
#> $target_aoi
#> [1] "disclosure"
#> 
#> $time_origin
#> [1] "trial_start"
#> 
#> $model_family
#> [1] "cox_repeated"
#> 
#> $random_or_frailty_structure
#> [1] "cluster_robust:participant_id"
#> 
#> $diagnostic_result
#> [1] "no PH diagnostic flag at alpha=.05"
```

A concise manuscript formulation is:

> Gaze latency was analysed as a right-censored time-to-event outcome.
> Trials in which the target AOI was not inspected before the valid
> observation window ended were retained as censored observations;
> unresolved gaze-quality trials were not reclassified as censoring. We
> fitted a Cox proportional-hazards model with participant-clustered
> uncertainty and reported hazard ratios with 95% confidence intervals.
> Proportional-hazards diagnostics and pre-specified
> AOI/event-definition sensitivity analyses were examined, with a
> Weibull AFT model reported as an alternative time-ratio estimand.

Adapt the wording to the actual detector, AOI, time origin, quality
threshold, and repeated-effects structure.

### Copyable manuscript template

> Gaze latency was analysed as a right-censored time-to-event outcome.
> The target event was \[EVENT\] in \[TARGET AOI\], measured from \[TIME
> ORIGIN\]. Trials remained under observation until \[OBSERVATION-WINDOW
> RULE\]. Trials without the event at the end of a complete usable
> window were retained as right-censored observations; trials with
> unresolved event status or gaze quality below \[QUALITY RULE\] were
> not recoded as censoring. Events were defined using
> \[DETECTOR/FIXATION RULE\]. We fitted \[MODEL FAMILY\] with
> \[REPEATED-PARTICIPANT STRUCTURE\] and reported \[HAZARD RATIOS/TIME
> RATIOS\] with 95% confidence intervals. Diagnostics indicated
> \[RESULT\], and the substantive conclusion was
> \[ROBUST/QUALIFIED/SENSITIVE\] across the pre-specified AOI,
> event-definition, quality-threshold, and estimator sensitivity
> analyses.

For the results paragraph, report participants, trials, events, censored
trials and percentage, review-required rows, the effect estimate with
confidence interval, diagnostic result, and whether sensitivity branches
changed the conclusion.

## API map

Preparation and validation:
[`prepare_gaze_survival_data()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`validate_gaze_survival_data()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`summarise_gaze_censoring()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md).

Estimation and models:
[`estimate_gaze_survival()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`fit_gaze_cox_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`fit_gaze_mixed_cox_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`fit_gaze_aft_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`predict_gaze_survival()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`estimate_gaze_latency_quantiles()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md).

Diagnostics and sensitivity:
[`check_gaze_proportional_hazards()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`compare_gaze_survival_models()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`compare_gaze_survival_specifications()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md).

Plots and reporting:
[`plot_gaze_survival_curve()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`plot_gaze_cumulative_incidence()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`plot_gaze_hazard()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`plot_gaze_cox_diagnostics()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`tidy_gaze_survival_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md),
[`report_gaze_survival_model()`](https://stefanosbalaskas.github.io/eyeprocess/reference/gaze-survival.md).
