# Fingerprint every file in a validation case

Part of the research-scale validation, advanced-model, interoperability,
storage, adapter, or reproducibility programme. Experimental model
functions remain subject to declared evidence gates.

## Usage

``` r
fingerprint_validation_case(path, algorithms = c("md5", "sha256"),
  include_hidden = FALSE)
```

## Arguments

- path:

  File or directory.

- algorithms:

  Hash algorithms. Base R always supplies MD5; SHA-256 is

- include_hidden:

  Include hidden files.

## Value

The documented eyeprocess object, data frame, plot, report path, or
adapter result.
