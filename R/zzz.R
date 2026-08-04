.detect_generic_delimited <- function(path, inspect_rows = 20L) {
  if (dir.exists(path)) return(0.05)
  if (!tolower(tools::file_ext(path)) %in% c("csv", "tsv", "txt", "asc")) return(0)
  d <- tryCatch(.read_delimited(path, nrows = min(2L, inspect_rows)), error = function(e) NULL)
  if (is.null(d) || ncol(d) < 2L) 0 else 0.10
}

.read_generic_adapter <- function(path, ...) read_eye_generic(path, ...)

.onLoad <- function(libname, pkgname) {
  .eye_env$adapters <- list()
  .eye_env$id_counters <- new.env(parent = emptyenv())
  register_eye_adapter("gazepoint", is_gazepoint_export, read_gazepoint, gp_validate_export, priority = 100, overwrite = TRUE)
  register_eye_adapter("tobii", is_tobii_export, read_tobii, validate_tobii_export, priority = 80, overwrite = TRUE)
  register_eye_adapter("pupillabs", is_pupil_labs_export, read_pupillabs, validate_pupillabs_export, priority = 80, overwrite = TRUE)
  register_eye_adapter("eyelink", is_eyelink_export, function(path, ...) {
    ext <- tolower(tools::file_ext(path))
    if (ext == "edf") read_eyelink_edf(path, ...) else if (ext == "asc") read_eyelink_asc(path, ...) else read_eyelink_report(path, ...)
  }, validate_eyelink_export, priority = 80, overwrite = TRUE)
  register_eye_adapter("smi", is_smi_export, read_smi, validate_smi_export, priority = 70, overwrite = TRUE)
  register_eye_adapter("generic", .detect_generic_delimited, .read_generic_adapter, validate_generic_export, priority = 1, overwrite = TRUE)
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "eyeprocess ", utils::packageVersion(pkgname),
    ": vendor-neutral eye/process data harmonization with first-class Gazepoint support."
  )
}
