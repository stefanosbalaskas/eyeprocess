# Quantify response-target process information in the M0-M2 sequence

Computes two complementary quantities on a common response target:
response-target PSIS-LOO ELPD and posterior variance of person ability.
This avoids simply summing channel Fisher information under a joint
correlated model.

## Usage

``` r
multimodal_m2_process_information(x)
```

## Arguments

- x:

  An \`eye_multimodal_m2_ablation\`.

## Value

An \`eye_multimodal_m2_information\`.
