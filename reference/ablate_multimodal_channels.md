# Create formal channel-ablation datasets

Create formal channel-ablation datasets

## Usage

``` r
ablate_multimodal_channels(
  x,
  include = c("response", "rt", "gaze", "pupil"),
  include_response = TRUE
)
```

## Arguments

- x:

  An \`eye_multimodal_measurement\`.

- include:

  Character vector of channels eligible for ablation.

- include_response:

  Whether response must remain in every scenario.

## Value

An \`eye_multimodal_ablation\` list.
