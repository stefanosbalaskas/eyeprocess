# Build a file hash manifest

Build a file hash manifest

## Usage

``` r
file_hash_manifest(paths, algorithm = c("md5", "sha256"))
```

## Arguments

- paths:

  File paths.

- algorithm:

  Hash algorithm; currently \`md5\` uses base R, \`sha256\` uses openssl
  when available.
