# Define a posterior-SBC replication contract

Posterior SBC is deliberately callback-driven: conditioning/augmentation
differs by model, and eyeprocess does not pretend that re-running
ordinary prior SBC on an observed-data neighbourhood is posterior SBC.

## Usage

``` r
posterior_sbc_contract(replication)
```

## Arguments

- replication:

  Function \`(replicate, observed_data)\` returning
  \`list(truth=named_numeric, draws=matrix_or_data_frame)\`. The
  callback is responsible for the conditional posterior-SBC construction
  appropriate to the model, including fitting to the observed data and
  the required self-consistency experiment.
