# Reconstruct media presentations as analysis trials

Converts contiguous Gazepoint media runs into explicit trial intervals.
This provides stable person-by-item-by-trial keys even when the original
export contains no behavioural response file.

## Usage

``` r
build_gazepoint_media_trials(x, item_map = NULL, overwrite = TRUE)
```

## Arguments

- x:

  An \`eye_dataset\` imported from Gazepoint.

- item_map:

  Optional data frame or CSV path containing \`stimulus_id\`,
  \`item_id\`, and optionally \`condition_id\`.

- overwrite:

  Replace existing trial intervals.

## Value

The updated \`eye_dataset\`.
