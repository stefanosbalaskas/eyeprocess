# Identify items for descriptive 3PL/process review

Identify items for descriptive 3PL/process review

## Usage

``` r
audit_3pl_process_signatures(
  x,
  lower_asymptote_quantile = 0.8,
  fast_rt_quantile = 0.2,
  fast_ttff_quantile = 0.2
)
```

## Arguments

- x:

  An \`eye_gaze_anchored_3pl_audit\`.

- lower_asymptote_quantile:

  Quantile used to flag relatively large lower asymptotes.

- fast_rt_quantile:

  Optional lower quantile for RT review.

- fast_ttff_quantile:

  Optional lower quantile for TTFF review.

## Value

Item-level review table. These flags are not behavioral classifications.
