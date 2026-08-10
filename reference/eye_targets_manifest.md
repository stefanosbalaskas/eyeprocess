# Create a targets-compatible dependency manifest

This function does not execute or silently translate arbitrary closures
into a targets pipeline. It provides the dependency contract required to
build an explicit \`\_targets.R\` file.

## Usage

``` r
eye_targets_manifest(x)
```

## Arguments

- x:

  Pipeline.
