# Cognitive diagnosis and Q-matrix governance

The cognitive-diagnosis layer supplies transparent Q-matrix audits,
attribute-profile enumeration, and deterministic DINA
ideal-response/probability calculations.

``` r

Q <- rbind(c(1,0), c(0,1), c(1,1), c(1,0))
aud <- eyeprocess_cdm_qmatrix_audit(Q)
aud
#> eyeprocess CDM Q-matrix audit
#>   items     : 4 
#>   attributes: 2 
#>   empty items: 0 | unmeasured attributes: 0
profiles <- eyeprocess_cdm_attribute_profiles(2)[, c("A1","A2")]
eta <- eyeprocess_cdm_dina_ideal_response(Q, profiles)
eyeprocess_cdm_dina_probability(eta)
#>      [,1] [,2] [,3] [,4]
#> [1,]  0.2  0.2  0.2  0.2
#> [2,]  0.9  0.2  0.2  0.9
#> [3,]  0.2  0.9  0.2  0.2
#> [4,]  0.9  0.9  0.9  0.9
```

These utilities do not replace full cognitive-diagnosis estimation,
Q-matrix validation, or model comparison.
[`fit_eyeprocess_gdina()`](https://stefanosbalaskas.github.io/eyeprocess/reference/fit_eyeprocess_gdina.md)
delegates exact fitting to GDINA and gates cleanly when unavailable.

Primary package source: <https://cran.r-project.org/package=GDINA>.

## Visual audit

A compact deterministic Q-matrix makes the structural audit visible. The
display concerns declared item-attribute structure and does not by
itself establish substantive validity.

``` r

viz_Q <- rbind(
  c(1, 0),
  c(0, 1),
  c(1, 1),
  c(1, 0),
  c(0, 1),
  c(1, 1)
)

rownames(viz_Q) <- paste0('Item ', seq_len(nrow(viz_Q)))
colnames(viz_Q) <- c('Attribute 1', 'Attribute 2')

viz_qmatrix <- eyeprocess::eyeprocess_cdm_qmatrix_audit(viz_Q)

stopifnot(
  inherits(viz_qmatrix, 'eye_cdm_qmatrix_audit')
)

plot(viz_qmatrix)
```

![Q-matrix structure for a small deterministic cognitive-diagnosis
example.](cognitive-diagnosis-qmatrix-governance_files/figure-html/m2-visual-qmatrix-1.png)

Q-matrix structure for a small deterministic cognitive-diagnosis
example.
