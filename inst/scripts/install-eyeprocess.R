args <- commandArgs(trailingOnly = TRUE)
path <- if (length(args)) args[[1L]] else getwd()
install.packages(normalizePath(path, winslash = "/", mustWork = TRUE), repos = NULL, type = "source")
