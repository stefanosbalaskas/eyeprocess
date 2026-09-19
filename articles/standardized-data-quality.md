# Standardized eye-tracking data quality

## Why a dedicated quality report?

Eye-tracking quality is not one score. **Accuracy** describes
target-referenced error, **precision** describes reproducibility while
gaze is stable, **sampling diagnostics** describe the temporal
measurement process, and **data loss** describes unavailable or invalid
observations. These quantities constrain different analyses and should
remain separately inspectable.

The standardized quality API is vendor-neutral. It does not use vendor
acceptance labels, does not silently convert coordinates, and does not
automatically exclude observations.

### Recommended workflow

1.  Validate columns, coordinate units, and timestamps.
2.  Segment validation data into periods in which the intended target is
    stable.
3.  Compute target-referenced accuracy.
4.  Compute RMS-S2S and SD precision separately; add BCEA when a
    spatial-area summary is useful.
5.  Compare nominal and effective sampling behavior and inspect interval
    jitter.
6.  Quantify valid, invalid, and missing samples and inspect missing
    runs/reasons.
7.  Apply only study-defined review rules.
8.  Repeat key analyses under defensible quality decisions as a
    sensitivity analysis.
9.  Report the observed quality metrics and the decision rule used.

## Accuracy versus precision

A constant offset can produce **poor accuracy but excellent precision**.
Conversely, centered but noisy gaze can produce acceptable mean accuracy
with poor precision. Therefore, one cannot substitute for the other.

``` r

library(eyeprocess)
#> eyeprocess 0.11.1.9000: vendor-neutral eye/process data harmonization with first-class Gazepoint support.
x <- simulate_gaze_quality_calibration(samples_per_target = 8)
q <- create_gaze_quality_report(
  x,
  by = c("profile", "target_id"),
  valid = "valid",
  missing_reason = "missing_reason",
  nominal_sampling_hz = 60
)
q[c("profile", "target_id", "accuracy_mean", "precision_rms_s2s",
    "precision_sd", "bcea", "effective_sampling_hz", "data_loss_fraction")]
#>                         profile target_id accuracy_mean precision_rms_s2s
#> 1                   missingness         1     0.2038983         0.3723245
#> 2                   missingness         2     0.1208572         0.2173418
#> 3                   missingness         3     0.2050401         0.4507930
#> 4                   missingness         4     0.3342909         0.4936667
#> 5                   missingness         5     0.2645711         0.3832862
#> 6                   missingness         6     0.3697123         0.5290561
#> 7                   missingness         7     0.1768610         0.2917686
#> 8                   missingness         8     0.3683028         0.3250203
#> 9                   missingness         9     0.1758097         0.2819499
#> 10           irregular_sampling         1     0.2580351         0.3509699
#> 11           irregular_sampling         2     0.2764499         0.3697507
#> 12           irregular_sampling         3     0.3234771         0.3892796
#> 13           irregular_sampling         4     0.1942139         0.2670008
#> 14           irregular_sampling         5     0.2970780         0.4162781
#> 15           irregular_sampling         6     0.2841654         0.2214373
#> 16           irregular_sampling         7     0.2323949         0.2930337
#> 17           irregular_sampling         8     0.3426513         0.4723916
#> 18           irregular_sampling         9     0.2660328         0.4831944
#> 19 good_accuracy_good_precision         1     0.1952280         0.2425720
#> 20 good_accuracy_good_precision         2     0.1479487         0.2257995
#> 21 good_accuracy_good_precision         3     0.1549211         0.2275499
#> 22 good_accuracy_good_precision         4     0.1136454         0.1582688
#> 23 good_accuracy_good_precision         5     0.1665890         0.2194203
#> 24 good_accuracy_good_precision         6     0.1217871         0.1917545
#> 25 good_accuracy_good_precision         7     0.1441908         0.2331926
#> 26 good_accuracy_good_precision         8     0.1806938         0.3045719
#> 27 good_accuracy_good_precision         9     0.1313981         0.1765858
#> 28 good_accuracy_poor_precision         1     1.1201963         1.3224223
#> 29 good_accuracy_poor_precision         2     1.1080083         1.9871790
#> 30 good_accuracy_poor_precision         3     0.6117188         0.9779925
#> 31 good_accuracy_poor_precision         4     0.6718079         1.2912672
#> 32 good_accuracy_poor_precision         5     0.6932250         0.9190246
#> 33 good_accuracy_poor_precision         6     1.1802648         1.7992260
#> 34 good_accuracy_poor_precision         7     1.2124467         1.6630388
#> 35 good_accuracy_poor_precision         8     1.1264000         1.4844337
#> 36 good_accuracy_poor_precision         9     0.9440231         1.2769225
#> 37 poor_accuracy_good_precision         1     1.0953563         0.2164899
#> 38 poor_accuracy_good_precision         2     1.1388079         0.1932125
#> 39 poor_accuracy_good_precision         3     1.1313614         0.2190133
#> 40 poor_accuracy_good_precision         4     1.0726315         0.1674472
#> 41 poor_accuracy_good_precision         5     1.0875775         0.3078673
#> 42 poor_accuracy_good_precision         6     1.1537931         0.1754027
#> 43 poor_accuracy_good_precision         7     1.1100043         0.2228539
#> 44 poor_accuracy_good_precision         8     1.0832458         0.1737250
#> 45 poor_accuracy_good_precision         9     1.1780941         0.1787003
#> 46 poor_accuracy_poor_precision         1     1.7354670         1.2945837
#> 47 poor_accuracy_poor_precision         2     1.4182149         1.3307771
#> 48 poor_accuracy_poor_precision         3     1.3492783         1.3690243
#> 49 poor_accuracy_poor_precision         4     1.5326206         2.1096081
#> 50 poor_accuracy_poor_precision         5     1.2953815         1.0690436
#> 51 poor_accuracy_poor_precision         6     1.4429288         1.6205708
#> 52 poor_accuracy_poor_precision         7     1.2866361         1.2442577
#> 53 poor_accuracy_poor_precision         8     1.3572839         1.4733503
#> 54 poor_accuracy_poor_precision         9     1.1712686         1.2107496
#>    precision_sd       bcea effective_sampling_hz data_loss_fraction
#> 1    0.24007220 0.11895842              37.50000              0.375
#> 2    0.13718156 0.04207937              37.50000              0.375
#> 3    0.22873830 0.09431452              37.50000              0.375
#> 4    0.27448318 0.26404193              37.50000              0.375
#> 5    0.28507244 0.24482907              37.50000              0.375
#> 6    0.35871455 0.41923935              37.50000              0.375
#> 7    0.16368896 0.06000620              37.50000              0.375
#> 8    0.32602925 0.13273776              37.50000              0.375
#> 9    0.17107232 0.09481066              37.50000              0.375
#> 10   0.23821768 0.16524586              51.10893              0.000
#> 11   0.27256218 0.25381979              44.34860              0.000
#> 12   0.28873179 0.20498700              43.66817              0.000
#> 13   0.18701454 0.12359156              47.76262              0.000
#> 14   0.30171003 0.30854774              53.32363              0.000
#> 15   0.17439494 0.10521116              57.77406              0.000
#> 16   0.21556759 0.15955069              52.21556              0.000
#> 17   0.30311860 0.31871272              57.14576              0.000
#> 18   0.29108111 0.29885782              61.56065              0.000
#> 19   0.19805892 0.10224875              60.00000              0.000
#> 20   0.14519105 0.06891347              60.00000              0.000
#> 21   0.13467536 0.06052523              60.00000              0.000
#> 22   0.12402210 0.05383371              60.00000              0.000
#> 23   0.18747781 0.11559200              60.00000              0.000
#> 24   0.12503893 0.05518699              60.00000              0.000
#> 25   0.16113573 0.07072278              60.00000              0.000
#> 26   0.18832131 0.08386924              60.00000              0.000
#> 27   0.13312813 0.03442053              60.00000              0.000
#> 28   1.13125472 2.93014404              60.00000              0.000
#> 29   1.14326228 3.77496071              60.00000              0.000
#> 30   0.65134223 1.49417094              60.00000              0.000
#> 31   0.75017343 1.73060240              60.00000              0.000
#> 32   0.71901708 1.72813998              60.00000              0.000
#> 33   1.17811578 4.28103956              60.00000              0.000
#> 34   1.21000840 4.86074870              60.00000              0.000
#> 35   1.06525277 3.20080300              60.00000              0.000
#> 36   0.87795431 2.50665380              60.00000              0.000
#> 37   0.12175857 0.04614761              60.00000              0.000
#> 38   0.11829320 0.04449319              60.00000              0.000
#> 39   0.14504346 0.06703913              60.00000              0.000
#> 40   0.09819616 0.02787452              60.00000              0.000
#> 41   0.18471065 0.08115383              60.00000              0.000
#> 42   0.15903378 0.06460880              60.00000              0.000
#> 43   0.13903223 0.06457246              60.00000              0.000
#> 44   0.18115964 0.07800608              60.00000              0.000
#> 45   0.12159306 0.03842020              60.00000              0.000
#> 46   0.86097587 2.61996515              60.00000              0.000
#> 47   0.94618372 2.83820210              60.00000              0.000
#> 48   0.82333481 2.08068184              60.00000              0.000
#> 49   1.27465756 4.51433984              60.00000              0.000
#> 50   0.62863285 1.22286488              60.00000              0.000
#> 51   1.01085940 3.41119277              60.00000              0.000
#> 52   1.12653382 4.16179529              60.00000              0.000
#> 53   1.11070317 4.02791117              60.00000              0.000
#> 54   0.82019378 2.23849996              60.00000              0.000
```

## Precision metrics answer different questions

For adjacent valid samples $`i`$ and $`i+1`$, two-dimensional RMS-S2S is

``` math
\mathrm{RMS\text{-}S2S}=\sqrt{\frac{1}{K}\sum_{i=1}^{K}
[(x_{i+1}-x_i)^2+(y_{i+1}-y_i)^2]}.
```

The implementation never joins across a missing sample. When
`max_gap_ms` is supplied, pairs separated by a larger timestamp gap are
also excluded from the successive-sample calculation and that choice is
preserved in provenance.

SD precision summarizes spatial spread around a stable target using the
population-SD convention (denominator `n`) used by the methodological
guidance; it is not mathematically interchangeable with RMS-S2S. BCEA
uses the same population SDs and provides an area summary:

``` math
\mathrm{BCEA}=2\pi k\,\sigma_x\sigma_y\sqrt{1-\rho^2},\qquad
k=-\log(1-p),
```

where `p` is the explicitly reported probability level (default `0.68`).

## Effective versus nominal sampling

[`estimate_effective_sampling_rate()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)
reports observed timestamps, the effective sample count, timestamp span,
an estimated recording duration (span plus one median positive
interval), effective Hz, and median interval. When nominal frequency is
supplied it separates `long_interval_count` (number of intervals
exceeding the declared nominal-gap multiple) from
`dropped_interval_count` (estimated missing nominal samples represented
by those long gaps). In a canonical gaze-quality report the effective
count is the number of valid gaze samples with finite timestamps, so
loss directly reduces effective Hz. Duplicate and non-monotonic
timestamps are never silently repaired.

``` r

summarise_sampling_quality(
  x[x$profile == "irregular_sampling", ],
  time = "timestamp_ms",
  nominal_sampling_hz = 60
)
#>     n_observed_timestamps n_intervals median_interval_ms mean_interval_ms
#> all                    72          71           17.38641         19.40926
#>     min_interval_ms max_interval_ms duplicate_timestamp_count
#> all        4.166667        59.33804                         0
#>     non_monotonic_timestamp_count sampling_jitter_ms sampling_jitter_mad_ms
#> all                             0           10.43408               3.529006
#>     observed_sample_count effective_sample_count timestamp_span_s
#> all                    72                     72         1.378058
#>     trial_duration_s effective_sampling_hz long_interval_count
#> all         1.395444              51.59648                   8
#>     dropped_interval_count nominal_sampling_hz effective_count_rule
#> all                     14                  60    finite timestamps
```

## Data loss and missingness

[`compute_gaze_data_loss()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)
keeps coordinate missingness and explicit validity information separate.
If a reason column exists, lost observations can additionally be
decomposed into reasons such as blink, tracker invalidity, off-screen,
or unknown.

``` r

compute_gaze_data_loss(
  x[x$profile == "missingness", ],
  valid = "valid",
  missing_reason = "missing_reason"
)
#>     n_samples valid_sample_fraction invalid_sample_fraction
#> all        72                 0.625                       0
#>     missing_sample_fraction data_loss_fraction missing_run_count
#> all                   0.375              0.375                 9
#>     longest_missing_run_samples longest_missing_run_ms
#> all                           3                     50
#>     missing_reason_blink_fraction missing_reason_tracker_invalidity_fraction
#> all                     0.6666667                                  0.3333333
```

## Study-specific review rules, not universal exclusions

Thresholds are optional and descriptive by default. They create
`quality_flags` and `review_required`; they **never remove rows**.

``` r

flagged <- create_gaze_quality_report(
  x,
  by = c("profile", "target_id"),
  valid = "valid",
  thresholds = list(
    accuracy_mean = list(max = 1.0),
    valid_sample_fraction = list(min = 0.80)
  )
)
attr(flagged, "gaze_quality_provenance")$automatic_exclusion
#> [1] FALSE
```

## Quality decisions as sensitivity analysis

A review flag is not an exclusion. Keep the complete
`gaze_quality_report`, then make the downstream inclusion decision
explicitly and report it separately.

``` r

reviewed <- create_gaze_quality_report(
  x,
  by = c("profile", "target_id"),
  valid = "valid",
  thresholds = list(
    accuracy_mean = list(max = 1.0),
    valid_sample_fraction = list(min = 0.80)
  )
)

analysis_keep <- reviewed[!reviewed$review_required, ]
analysis_flagged <- reviewed[reviewed$review_required, ]
```

For a substantive model, join the quality metadata back to the
trial/participant analysis key and repeat the planned analysis under
defensible alternatives: all otherwise eligible units, a pre-specified
exclusion rule, stratification by quality flag, or another explicitly
justified sensitivity specification. Do not choose a post-hoc cutoff
because it improves the substantive result.

When nominal frequency is supplied, also keep `long_interval_count`
separate from `dropped_interval_count`: the first counts long observed
intervals; the second estimates missing nominal samples represented by
those gaps.

## When not to use these metrics

Do not estimate target-referenced accuracy without known targets. Do not
compute fixation precision across target transitions or intentional eye
movements. Do not interpret BCEA as accuracy. Do not infer tracker
quality from nominal hardware frequency alone. Do not treat missing
coordinates as zeros, and do not interpret a quality flag as
participant-level impairment or disengagement.

## Manuscript reporting example

A compact report should identify the coordinate unit, the validation
procedure, the probability level used for BCEA, both nominal and
observed sampling behavior where relevant, the operational definition of
data loss, and any study-specific review/exclusion rule.
[`report_gaze_quality()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)
produces a descriptive starting point, but the final manuscript should
additionally state the acquisition and preprocessing context.

Example wording:

> Gaze-position accuracy was quantified as target-referenced Euclidean
> error. Precision was summarized separately using RMS sample-to-sample
> displacement, spatial SD, and 68% BCEA. The nominal 60-Hz rate was
> supplemented by timestamp-derived effective frequency and inter-sample
> jitter. Data loss was defined from unavailable coordinates or explicit
> invalidity flags. Pre-specified quality thresholds triggered manual
> review rather than automatic exclusion; all exclusions and sensitivity
> analyses were reported separately.

## API map

- Accuracy:
  [`compute_gaze_accuracy()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)
- Precision:
  [`compute_rms_s2s()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`compute_gaze_sd_precision()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`compute_bcea()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`compute_gaze_precision()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)
- Sampling:
  [`estimate_sampling_interval()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`estimate_sampling_jitter()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`estimate_effective_sampling_rate()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)
- Availability:
  [`compute_valid_sample_fraction()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`compute_gaze_data_loss()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)
- Reports:
  [`summarise_spatial_quality()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`summarise_sampling_quality()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`create_gaze_quality_report()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)
- Comparisons:
  [`compare_gaze_quality_sessions()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`compare_gaze_quality_conditions()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)
- Visuals:
  [`plot_gaze_accuracy()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`plot_gaze_precision()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`plot_bcea()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`plot_sampling_intervals()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md),
  [`plot_gaze_quality_dashboard()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)
- Teaching/CI:
  [`simulate_gaze_quality_calibration()`](https://stefanosbalaskas.github.io/eyeprocess/reference/standardized_spatial_quality.md)

## Reporting references

The terminology and reporting boundaries follow the minimal eye-tracking
reporting guideline (Dunn et al., 2024, *Behavior Research Methods*) and
the 2026 data-quality tutorial by Niehorster and colleagues (*Behavior
Research Methods*, <doi:10.3758/s13428-026-03039-4>).
