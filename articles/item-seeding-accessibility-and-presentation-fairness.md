# Item seeding, accessibility review, and presentation fairness

## Experimental item-parameter seeding

``` r

seed <- fit_item_parameter_seed_model(
  calibrated_items,
  predictors = c("visual_density", "text_complexity", "word_count", "screen_luminance")
)

candidate_predictions <- predict_item_parameter_priors(seed, candidate_items)
audit_candidate_item_bank(seed, candidate_items)
plot(seed, candidate_data = candidate_items)
```

These predictions are screening priors/cold-start estimates only. They
do not replace content review, accessibility/bias review, pilot testing,
or IRT calibration.

## Presentation/accessibility sensitivity

``` r

a <- audit_presentation_accessibility(person_process_data)
sim <- simulate_presentation_variants(a)
plot(a)
```

This audit must not be used to infer dyslexia, ADHD, neurodivergence,
visual impairment, or another diagnosis. It identifies presentation
patterns worth evaluating with calibrated alternative versions.

``` r

compare_presentation_fairness(
  experiment_data,
  variant = "presentation_version",
  outcome = "accuracy"
)
```
