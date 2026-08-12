# Measurement-quality stress tests and negative controls

Milestone \#2 turns measurement-quality perturbations into predeclared
software-validation scenarios.

``` r

sp <- eyeprocess_stress_evidence_plan()
head(expand_eyeprocess_stress_evidence_plan(sp))
#>   scenario_id    corruption severity     seed
#> 1   STRESS001  missing_gaze     0.00 20365541
#> 2   STRESS002  missing_gaze     0.05 20470270
#> 3   STRESS003  missing_gaze     0.15 20574999
#> 4   STRESS004  missing_gaze     0.30 20679728
#> 5   STRESS005 pupil_dropout     0.00 20784457
#> 6   STRESS006 pupil_dropout     0.05 20889186
eyeprocess_reliability_evidence_plan()
#> $metrics
#> [1] "split_half"         "icc"                "temporal_stability"
#> [4] "bland_altman"      
#> 
#> $bootstrap
#> [1] 200
#> 
#> $seed
#> [1] 20260811
#> 
#> $guardrail
#> [1] "Reliability is repeatability evidence, not construct validity."
#> 
#> attr(,"class")
#> [1] "eye_reliability_evidence_plan"
eyeprocess_negative_control_evidence_plan()
#> $controls
#> [1] "permutation"    "temporal_shift" "placebo_window" "known_leakage" 
#> 
#> $replications
#> [1] 100
#> 
#> $seed
#> [1] 20260811
#> 
#> $guardrail
#> [1] "Negative controls diagnose analysis behavior; they do not label analyst conduct or participant state."
#> 
#> attr(,"class")
#> [1] "eye_negative_control_evidence_plan"
```

Stress dimensions include gaze missingness, pupil dropout, calibration
offsets, sampling jitter, AOI-label noise, device shifts, and trial
imbalance. Reliability evidence includes split-half, ICC, temporal
stability, and Bland-Altman-style agreement. Negative controls include
permutations, temporal shifts, placebo windows, and intentionally known
leakage.

Thresholds and perturbations are study-specific reporting conventions.
Reliability is not validity, and negative-control/leakage findings are
not misconduct labels.
