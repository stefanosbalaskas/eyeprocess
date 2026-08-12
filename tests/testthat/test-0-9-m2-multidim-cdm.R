test_that("multidimensional information and loading audits work", {
  L <- matrix(c(1,0,1,0,0,1,0,1,1,0,0,1), ncol=2, byrow=TRUE)
  sp <- eyeprocess_mirt_loading_spec(paste0("I",1:6), L, c("D1","D2"), simple_structure=TRUE)
  au <- eyeprocess_mirt_loading_audit(sp, min_items_per_dimension=2)
  expect_true(all(au$meets_minimum))
  im <- eyeprocess_mirt_information_matrix(c(0,0), c(1,.5))
  expect_equal(dim(im), c(2L,2L))
  expect_true(all(eigen(im, symmetric=TRUE, only.values=TRUE)$values >= -1e-10))
})

test_that("Q-matrix and DINA utilities obey binary contracts", {
  Q <- rbind(c(1,0),c(0,1),c(1,1),c(1,0))
  q <- eyeprocess_cdm_qmatrix_audit(Q)
  expect_true(q$complete_identity_block)
  pr <- eyeprocess_cdm_attribute_profiles(2)[,c("A1","A2")]
  eta <- eyeprocess_cdm_dina_ideal_response(Q, pr)
  pp <- eyeprocess_cdm_dina_probability(eta)
  expect_true(all(pp >= 0 & pp <= 1))
})

test_that("CDM classification uncertainty requires competing profiles", {
  expect_error(eyeprocess_cdm_classification_uncertainty(matrix(1,nrow=3,ncol=1)))
})
