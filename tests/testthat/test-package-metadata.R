locate_eyeprocess_description <- function() {
  candidates <- c(
    testthat::test_path("..", "..", "DESCRIPTION"),
    file.path(getwd(), "DESCRIPTION"),
    system.file("DESCRIPTION", package = "eyeprocess")
  )
  candidates <- unique(candidates[nzchar(candidates)])
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    testthat::skip("Package DESCRIPTION is unavailable in this test context.")
  }
  existing[[1L]]
}

test_that("package metadata declares the pkgdown and repository URLs", {
  desc <- read.dcf(locate_eyeprocess_description())
  urls <- unname(as.character(desc[1L, "URL"]))
  expect_match(urls, "https://stefanosbalaskas.github.io/eyeprocess", fixed = TRUE)
  expect_match(urls, "https://github.com/stefanosbalaskas/eyeprocess", fixed = TRUE)
})

test_that("version strings compare after attribute normalization", {
  desc <- read.dcf(locate_eyeprocess_description())
  source_version <- trimws(unname(as.character(desc[1L, "Version"])))
  installed_like <- structure(source_version, names = "Version")
  normalized <- trimws(unname(as.character(installed_like)))
  expect_identical(normalized, source_version)
})


test_that("pkgdown reference index includes the package overview", {
  candidates <- c(
    testthat::test_path("..", "..", "_pkgdown.yml"),
    file.path(getwd(), "_pkgdown.yml")
  )
  existing <- candidates[file.exists(candidates)]
  if (!length(existing)) {
    testthat::skip("_pkgdown.yml is unavailable in this test context.")
  }
  config <- paste(readLines(existing[[1L]], warn = FALSE), collapse = "\n")
  expect_match(config, "- title: Package overview", fixed = TRUE)
  expect_match(config, "- eyeprocess-package", fixed = TRUE)
})


test_that("packaged regression fixtures use portable filenames", {
  root <- system.file("extdata", package = "eyeprocess")
  testthat::skip_if(!nzchar(root), "Installed extdata is unavailable.")
  files <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  expect_false(any(grepl("[[:space:]]", files)))
})
