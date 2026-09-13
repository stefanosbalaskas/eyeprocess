# Construct a lightweight provenance graph

Construct a lightweight provenance graph

## Usage

``` r
eye_prov_graph(
  nodes,
  edges = data.frame(from = character(), to = character(), relation = character()),
  metadata = list()
)
```

## Arguments

- nodes:

  Node table.

- edges:

  Edge table.

- metadata:

  Optional metadata.

## Value

A named list with components "nodes", "edges", "metadata", containing a
lightweight provenance graph and associated metadata or diagnostics.
