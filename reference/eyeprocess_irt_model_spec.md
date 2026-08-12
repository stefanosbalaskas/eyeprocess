# Declare an eyeprocess IRT model specification

This specification records a psychometric model contract. Estimation is
delegated to explicit engines where required; the specification itself
does not fit a model.

## Usage

``` r
eyeprocess_irt_model_spec(
  family = c("rasch", "2pl", "3pl", "4pl", "grm", "gpcm", "nominal", "multidimensional",
    "testlet", "latent_regression", "cdm", "joint_rt"),
  dimensions = 1L,
  identification = c("theta_standard", "item_sum_zero", "anchor"),
  engine = c("native_math", "mirt", "TAM", "GDINA", "LNIRT", "eRm", "custom"),
  process_channels = character(),
  status = c("reference", "experimental", "gated"),
  notes = NULL
)
```

## Arguments

- family:

  IRT response family or model family.

- dimensions:

  Value supplied for the dimensions argument.

- identification:

  Identification specification or identification audit.

- engine:

  Requested estimation or analysis engine.

- process_channels:

  Declared process-measure channels.

- status:

  Evidence, model, or governance status.

- notes:

  Value supplied for the notes argument.
