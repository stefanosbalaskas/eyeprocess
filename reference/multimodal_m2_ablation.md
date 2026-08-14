# Fit M0, M1, and M2 as a response-target ablation sequence

Fits response-only (M0), response+RT (M1), and response+RT+gaze (M2)
with compatible hierarchical Stan implementations. The sequence is
designed for response-target comparison rather than for asserting that
information from distinct channels is algebraically additive.

## Usage

``` r
multimodal_m2_ablation(x, ...)
```

## Arguments

- x:

  Data accepted by \[fit_multimodal_m2()\].

- ...:

  Sampling arguments forwarded to the internal reference fitters.

## Value

An \`eye_multimodal_m2_ablation\`.
