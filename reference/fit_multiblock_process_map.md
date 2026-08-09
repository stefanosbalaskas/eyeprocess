# Fit a multiblock psychometric/gaze/pupil/quality structure map

Uses FactoMineR MFA when available/requested. A block-standardized PCA
fallback is available as a transparent exploratory reference and is
explicitly labeled.

## Usage

``` r
fit_multiblock_process_map(
  x,
  blocks = NULL,
  id = NULL,
  engine = c("auto", "FactoMineR", "pca_block_scaled"),
  ncp = 5L
)
```

## Arguments

- x:

  A \`process_feature_blocks()\` object or data frame.

- blocks:

  Required if \`x\` is a data frame.

- id:

  Optional identifier.

- engine:

  \`auto\`, \`FactoMineR\`, or \`pca_block_scaled\`.

- ncp:

  Number of components retained where supported.
