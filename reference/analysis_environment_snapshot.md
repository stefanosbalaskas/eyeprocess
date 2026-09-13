# Snapshot an eyeprocess analysis environment

Snapshot an eyeprocess analysis environment

## Usage

``` r
analysis_environment_snapshot(packages = loadedNamespaces())
```

## Arguments

- packages:

  Optional package names; defaults to loaded namespaces.

## Value

A named list with components "r_version", "platform", "os", "locale",
"timezone", "packages", containing snapshot an eyeprocess analysis
environment and associated metadata or diagnostics.
