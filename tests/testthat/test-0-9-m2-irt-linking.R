test_that("linking recovers a known linear transformation", {
  ref <- data.frame(item_id = paste0("I", 1:8), a = seq(.8,1.5,length.out=8), b = seq(-1.5,1.5,length.out=8), c=0,d=1)
  A <- 1.2; B <- -.3
  focal <- ref
  focal$a <- ref$a * A
  focal$b <- (ref$b - B) / A
  link <- eyeprocess_irt_mean_sigma_link(ref, focal)
  linked <- eyeprocess_irt_apply_link(focal, link)
  expect_equal(link$A, A, tolerance = 1e-8)
  expect_equal(link$B, B, tolerance = 1e-8)
  expect_equal(linked$a, ref$a, tolerance = 1e-8)
  expect_equal(linked$b, ref$b, tolerance = 1e-8)
})

test_that("DIF and drift summaries are finite-safe", {
  r <- data.frame(item_id="I1",a=1,b=0,c=0,d=1)
  f <- data.frame(item_id="I1",a=1.1,b=.2,c=0,d=1)
  d <- eyeprocess_irt_dif_effect_curve(r,f)
  s <- eyeprocess_irt_functioning_effect_summary(d)
  expect_true(is.finite(s$max_abs))
  drift <- eyeprocess_irt_session_drift(data.frame(item_id=c("I1","I1","I2"), session=c(1,2,1), b=c(.1,.3,NA)))
  expect_true(is.na(drift$change[drift$item_id == "I2"]))
})

test_that("characteristic-curve linking validates weights before normalization", {
  ref <- data.frame(item_id=paste0("I",1:4),a=1,b=seq(-1,1,length.out=4),c=0,d=1)
  focal <- ref
  expect_error(eyeprocess_irt_stocking_lord_link(ref,focal,theta=-1:1,weights=c(1,1)))
  expect_error(eyeprocess_irt_haebara_link(ref,focal,theta=-1:1,weights=c(0,0,0)))
  expect_error(eyeprocess_irt_stocking_lord_link(ref,focal,start=c(0,0)))
})

test_that("functioning summaries are finite-safe for unavailable effects", {
  z <- data.frame(theta=c(-1,0,1), absolute_difference=NA_real_, signed_difference=NA_real_)
  out <- eyeprocess_irt_functioning_effect_summary(z)
  expect_true(is.na(out$max_abs))
  expect_true(is.na(out$mean_abs))
  expect_true(is.na(out$signed_area))
})
