#' Check if a path exists and delete the file
#' if overwrite is TRUE
check_exists_and_overwrite <- function(path, overwrite) {
  checkmate::assert_string(path)
  checkmate::assert_logical(overwrite, len = 1)
  if (fs::file_exists(path)) {
    if (isTRUE(overwrite)) {
      fs::file_delete(path)
    } else {
      abort(glue("{path} already exists. Pass overwrite = TRUE to overwrite it."))
    }
  }
}
