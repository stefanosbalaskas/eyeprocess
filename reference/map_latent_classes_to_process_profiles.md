# Map supplied latent-class memberships to process summaries

Map supplied latent-class memberships to process summaries

## Usage

``` r
map_latent_classes_to_process_profiles(
  class_membership,
  process_data,
  person = "person_id",
  class_col = "class",
  process_features
)
```

## Arguments

- class_membership:

  Data containing person/class assignments or probabilities.

- process_data:

  Person-level or trial-level process data.

- person:

  Person identifier present in both objects.

- class_col:

  Class-assignment column.

- process_features:

  Numeric process features.
