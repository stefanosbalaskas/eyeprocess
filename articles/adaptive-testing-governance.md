# Adaptive testing design and governance

Native adaptive-design utilities deliberately remain simple and
auditable: item-bank declaration, maximum-information selection,
exposure summaries, content-balance audits, stopping rules, and
step-by-step adaptive traces.

``` r

items <- data.frame(item_id=paste0("I",1:6),a=c(.8,1,1.5,1.1,.9,1.2),b=seq(-1.5,1.5,length.out=6),c=0,d=1)
bank <- eyeprocess_irt_item_bank(items, content = c("A","A","B","B","C","C"))
eyeprocess_irt_item_selection(bank, theta = 0)
#> $selected
#> [1] "I3"
#> 
#> $information
#> [1] 0.5349577
#> 
#> $theta
#> [1] 0
#> 
#> $reason
#> [1] "maximum_information"
#> 
#> $candidate_count
#> [1] 6
#> 
#> attr(,"class")
#> [1] "eye_irt_item_selection"
eyeprocess_irt_stopping_rule(10, se = .25)
#> $stop
#> [1] TRUE
#> 
#> $reason
#> [1] "target_precision"
#> 
#> $n_administered
#> [1] 10
#> 
#> $se
#> [1] 0.25
```

For mature CAT simulation and constrained/shadow-testing designs,
eyeprocess delegates to `catR` and `mirtCAT`. Availability is explicit
and no simpler CAT engine is silently substituted.

Primary package sources: <https://cran.r-project.org/package=catR> and
<https://cran.r-project.org/package=mirtCAT>.
