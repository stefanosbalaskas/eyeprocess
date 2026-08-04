# Run a preprocessing or AOI multiverse

Run a preprocessing or AOI multiverse

## Usage

``` r
preprocessing_multiverse(
  x,
  specifications,
  transform,
  analyse,
  extract = function(z) as.data.frame(z)
)
```

## Arguments

- x:

  Input object.

- specifications:

  Named list of specification objects.

- transform:

  Function receiving \`x\` and one specification.

- analyse:

  Analysis function receiving the transformed object.

- extract:

  Function converting an analysis result to a data frame.

## Value

An \`eye_multiverse\` object.
