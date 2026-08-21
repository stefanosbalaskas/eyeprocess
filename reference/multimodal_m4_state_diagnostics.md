# Summarize M4 latent-state uncertainty and dynamics

Returns posterior state probabilities, entropy, occupancy, MAP-run
summaries, transition probabilities, and posterior-weighted
process-channel profiles. MAP states are explicitly secondary summaries
of posterior probabilities.

## Usage

``` r
multimodal_m4_state_diagnostics(x)
```

## Arguments

- x:

  M4 fit or M4 simulation. Simulation diagnostics use known synthetic
  truth and are labelled accordingly.

## Value

An \`eye_multimodal_m4_states\` object.
