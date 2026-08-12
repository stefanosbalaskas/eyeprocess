# IRT linking, DIF, DTF, and invariance evidence

Scale linking and invariance are treated as evidence components rather
than binary declarations of validity.

``` r

ref <- data.frame(item_id=paste0("I",1:8),a=seq(.8,1.5,length.out=8),b=seq(-1.5,1.5,length.out=8),c=0,d=1)
A <- 1.2; B <- -.3
foc <- ref; foc$a <- ref$a * A; foc$b <- (ref$b - B) / A
link <- eyeprocess_irt_mean_sigma_link(ref, foc)
link
#> eyeprocess IRT link
#>   method : mean-sigma 
#>   A      : 1.2 
#>   B      : -0.3 
#>   anchors: 8
eyeprocess_irt_apply_link(foc, link)
#>   item_id   a          b c d
#> 1      I1 0.8 -1.5000000 0 1
#> 2      I2 0.9 -1.0714286 0 1
#> 3      I3 1.0 -0.6428571 0 1
#> 4      I4 1.1 -0.2142857 0 1
#> 5      I5 1.2  0.2142857 0 1
#> 6      I6 1.3  0.6428571 0 1
#> 7      I7 1.4  1.0714286 0 1
#> 8      I8 1.5  1.5000000 0 1
```

Native utilities include mean-sigma, mean-mean, Stocking-Lord, and
Haebara objectives, anchor screening/purification, DIF/DTF effect
curves, anchor-set stability, session/device drift, and descriptive
process-channel concordance. For mature equating workflows, `equateIRT`
remains an explicit optional engine rather than being replaced by a
simpler estimator.

Primary package source: <https://cran.r-project.org/package=equateIRT>.
