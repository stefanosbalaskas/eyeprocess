testthat::test_that("M4 RT anchor is protected from the zero-separation identification boundary", {
    stan <- system.file("stan", "m4-trait-conditioned-state-0-11.stan", package = "eyeprocess")
    if (!nzchar(stan) || !file.exists(stan)) {
        stan <- file.path("inst", "stan", "m4-trait-conditioned-state-0-11.stan")
    }
    testthat::expect_true(file.exists(stan))

    txt <- readLines(stan, warn = FALSE, encoding = "UTF-8")
    code <- paste(txt, collapse = "\n")

    testthat::expect_true(grepl("ordered[K] state_rt_raw;", code, fixed = TRUE))
    testthat::expect_true(grepl("vector[K] delta_rt = state_rt_raw - mean(state_rt_raw);", code, fixed = TRUE))

    testthat::expect_false(grepl("state_rt_raw ~ normal(0, 0.45);", code, fixed = TRUE))
    testthat::expect_false(grepl("state_rt_raw ~ normal(0, 0.75);", code, fixed = TRUE))

    testthat::expect_true(grepl("mean(state_rt_raw) ~ normal(0, 0.45);", code, fixed = TRUE))
    testthat::expect_true(grepl("mean(state_rt_raw) ~ normal(0, 0.75);", code, fixed = TRUE))

    testthat::expect_true(grepl("state_rt_raw[k] - state_rt_raw[k - 1] | 3, 5", code, fixed = TRUE))
    testthat::expect_true(grepl("state_rt_raw[k] - state_rt_raw[k - 1] | 2, 2.5", code, fixed = TRUE))
    testthat::expect_true(sum(grepl("if (K > 1)", txt, fixed = TRUE)) >= 2L)
})

testthat::test_that("M4 public identification metadata records adjacent-gap anchoring", {
    s <- multimodal_m4_spec(n_states = 2L)
    testthat::expect_identical(s$state_identification, "ordered_rt_effect")
    testthat::expect_match(
        s$identification$m4_state$note,
        "adjacent-gap prior",
        fixed = TRUE
    )
    testthat::expect_match(
        s$identification$m4_state$note,
        "does not order psychological meaning",
        fixed = TRUE
    )
})
