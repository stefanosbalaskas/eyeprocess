# Estimate a descriptive continuous-time latent trajectory

Estimate a descriptive continuous-time latent trajectory

## Usage

``` r
latent_trait_trajectory(time, theta, spar = NULL)
```

## Arguments

- time:

  Time values.

- theta:

  Latent-trait values.

- spar:

  Value supplied to \`spar\`; see Details for its model-specific role.

## Value

An object of class "eye_latent_trait_trajectory", stored as a named
list, with components "model", "time", "theta". It contains a
descriptive continuous-time latent trajectory and associated metadata or
diagnostics needed to interpret the result.
