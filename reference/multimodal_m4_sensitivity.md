# Plan or run M4 state-count and modelling sensitivity analyses

The default returns a transparent sensitivity design without fitting. It
covers K, priors, trait conditioning, transition structure, state
channels, nuisance adjustment, and sequence-length threshold. No
automatic \`best_K\` is declared.

## Usage

``` r
multimodal_m4_sensitivity(x, n_states = 1:4, run = FALSE, fit_args = list())
```

## Arguments

- x:

  Data or simulation.

- n_states:

  Candidate state counts, default 1:4.

- run:

  Whether to execute K-sensitivity fits. Other sensitivity dimensions
  remain explicit design rows rather than an automatic combinatorial
  grid.

- fit_args:

  Optional fitting arguments.

## Value

An \`eye_multimodal_m4_sensitivity\`.
