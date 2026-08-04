# Create a public, de-identified benchmark bundle

Create a public, de-identified benchmark bundle

## Usage

``` r
create_public_benchmark(
  x,
  path,
  max_participants = 50L,
  include_samples = FALSE,
  overwrite = FALSE
)
```

## Arguments

- x:

  An \`eye_dataset\`.

- path:

  Output directory.

- max_participants:

  Optional participant cap.

- include_samples:

  Whether to include sample-level tables.

- overwrite:

  Whether to replace the output directory.

## Value

Output directory.
