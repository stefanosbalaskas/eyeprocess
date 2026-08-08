# Multimodal trait-model convenience wrapper

Extends the gaze/RT architecture to noncognitive or other latent traits
by allowing caller-defined semantic labels. The statistical engine is
delegated to \`fit_joint_gaze_rt_irt()\`; interpretation remains the
researcher's job.

## Usage

``` r
fit_multimodal_trait_irt(
  data,
  response,
  rt,
  gaze,
  person,
  item,
  trait_label = "trait",
  process_label = "process",
  ...
)
```

## Arguments

- data:

  Input data frame or compatible tabular object.

- response:

  Response variable or response-column name.

- rt:

  Response-time variable or column name.

- gaze:

  Gaze/process variable or column name.

- person:

  Person or participant identifier column.

- item:

  Item identifier, name, or item column.

- trait_label:

  Value supplied to \`trait_label\`; see Details for its model-specific
  role.

- process_label:

  Value supplied to \`process_label\`; see Details for its
  model-specific role.

- ...:

  Additional arguments passed to the selected model, engine, or method.
