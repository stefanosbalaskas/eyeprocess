# Install eyeprocess from its local source directory on Windows.
# Run with: source("C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess/install_eyeprocess.R")

package_path <- normalizePath(
  "C:/Users/Stefanos-PC/Documents/Rstudio/eyeprocess",
  winslash = "/",
  mustWork = TRUE
)

if ("package:eyeprocess" %in% search() || "eyeprocess" %in% loadedNamespaces()) {
  stop(
    "The eyeprocess package is currently loaded. Restart R (Ctrl+Shift+F10) ",
    "and run this installer again before loading eyeprocess.",
    call. = FALSE
  )
}

# Remove known usethis/devtools scaffold artefacts that can survive an
# overwrite-only ZIP extraction from an earlier package directory.
stale_scaffold <- file.path(
  package_path,
  c("R/hello.R", "man/hello.Rd", "tests/testthat/test-hello.R")
)
stale_scaffold <- stale_scaffold[file.exists(stale_scaffold)]
if (length(stale_scaffold)) {
  unlink(stale_scaffold, force = TRUE)
  cat(
    "Removed stale scaffold file(s):\n",
    paste(" -", stale_scaffold, collapse = "\n"),
    "\n",
    sep = ""
  )
}

required <- c("testthat", "knitr", "rmarkdown", "jsonlite")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)

source_description <- read.dcf(file.path(package_path, "DESCRIPTION"))
source_version <- trimws(unname(as.character(source_description[1L, "Version"])))

install.packages(package_path, repos = NULL, type = "source")

library(eyeprocess)
installed_version <- trimws(unname(as.character(packageVersion("eyeprocess"))))
if (!identical(installed_version, source_version)) {
  stop(
    "Installed version ", installed_version,
    " does not match source version ", source_version, ".",
    call. = FALSE
  )
}
cat("Installed eyeprocess ", installed_version, "\n", sep = "")
print(supported_eye_formats())

# Minimal smoke test.
x <- simulate_eye_dataset(
  n_person = 3,
  n_item = 3,
  sampling_rate = 20,
  trial_duration = 0.5,
  seed = 42
)
stopifnot(inherits(x, "eye_dataset"), nrow(x$gaze_samples) > 0L)
cat("Installation smoke test passed.\n")
