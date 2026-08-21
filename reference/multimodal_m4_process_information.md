# Quantify incremental response-target information supplied by M4 state structure

Uses commensurate response-target PSIS-LOO and ability-posterior
uncertainty to compare M3 with focused M4 variants. It explicitly allows
no benefit, redundancy, or destabilization and does not sum channel
Fisher information.

## Usage

``` r
multimodal_m4_process_information(x, decisive_z = 2)
```

## Arguments

- x:

  Executed M4 ablation object.

- decisive_z:

  Descriptive absolute delta/SE threshold.

## Value

An \`eye_multimodal_m4_information\`.
