testthat::test_that("trait-conditioned Markov M4 is structurally REVIEW-gated", {
    sim <- simulate_multimodal_m4(
        n_person = 30L,
        n_item = 10L,
        n_states = 2L,
        scenario = "clear",
        seed = 20260821L
    )

    spec <- multimodal_m4_spec(
        n_states = 2L,
        transition_structure = "markov",
        trait_conditioning = c("theta", "tau"),
        initial_trait_conditioning = TRUE
    )

    a <- audit_multimodal_m4_identifiability(
        sim,
        spec = spec,
        include_posterior = FALSE
    )

    z <- a$checks[
        a$checks$criterion == "trait_conditioned_markov",
        ,
        drop = FALSE
    ]

    testthat::expect_equal(nrow(z), 1L)
    testthat::expect_identical(z$status, "REVIEW")
    testthat::expect_identical(a$overall, "REVIEW")
    testthat::expect_true(a$supported)
})


testthat::test_that("unconditional Markov M4 does not trigger trait-conditioning gate", {
    sim <- simulate_multimodal_m4(
        n_person = 30L,
        n_item = 10L,
        n_states = 2L,
        scenario = "clear",
        seed = 20260822L
    )

    spec <- multimodal_m4_spec(
        n_states = 2L,
        transition_structure = "markov",
        trait_conditioning = character(),
        initial_trait_conditioning = FALSE
    )

    a <- audit_multimodal_m4_identifiability(
        sim,
        spec = spec,
        include_posterior = FALSE
    )

    z <- a$checks[
        a$checks$criterion == "trait_conditioned_markov",
        ,
        drop = FALSE
    ]

    testthat::expect_equal(nrow(z), 1L)
    testthat::expect_identical(z$status, "PASS")
})


testthat::test_that("M4 fit contains explicit trait-conditioned Markov warning", {
    txt <- paste(
        deparse(body(fit_multimodal_m4)),
        collapse = "\n"
    )

    testthat::expect_true(
        grepl(
            "M4 trait-conditioned Markov specification is gated",
            txt,
            fixed = TRUE
        )
    )
})
