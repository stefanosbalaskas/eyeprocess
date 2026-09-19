eyeprocess_source_root <- function() {
  root <- normalizePath(
    file.path(testthat::test_path(), "..", ".."),
    mustWork = FALSE
  )
  if (file.exists(file.path(root, "DESCRIPTION")) &&
      file.exists(file.path(root, "_pkgdown.yml"))) {
    return(root)
  }
  NULL
}

eyeprocess_asset <- function(source_path, installed_path = NULL) {
  root <- eyeprocess_source_root()
  if (!is.null(root)) {
    candidate <- file.path(root, source_path)
    if (file.exists(candidate)) return(candidate)
  }
  if (!is.null(installed_path)) {
    candidate <- do.call(
      system.file,
      c(as.list(installed_path), list(package = "eyeprocess"))
    )
    if (nzchar(candidate) && file.exists(candidate)) return(candidate)
  }
  ""
}
