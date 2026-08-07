test_that("measurement-intelligence API inventory is installed and exported", {
  inventory_path <- system.file(
    "extdata",
    "measurement-intelligence-api-inventory.csv",
    package = "eyeprocess"
  )
  expect_true(nzchar(inventory_path))
  inventory <- utils::read.csv(inventory_path, stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(nrow(inventory), 150)
  expect_true("function" %in% names(inventory))
  expect_equal(length(unique(inventory[["function"]])), 150)

  namespace <- asNamespace("eyeprocess")
  exported <- getNamespaceExports("eyeprocess")
  missing_functions <- inventory[["function"]][
    !vapply(inventory[["function"]], exists, logical(1), envir = namespace, mode = "function", inherits = FALSE)
  ]
  missing_exports <- setdiff(inventory[["function"]], exported)

  expect_length(missing_functions, 0)
  expect_length(missing_exports, 0)
  expect_true(all(vapply(inventory[["function"]][grepl("^plot", inventory[["function"]])], function(name) {
    is.function(get(name, envir = namespace, inherits = FALSE))
  }, logical(1))))
})
