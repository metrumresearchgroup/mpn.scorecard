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

#' Assign output file path for various outputs during scorecard rendering
#'
#' @param out_dir output directory for saving results and json
#' @param pkg_path path to installed/untarred package
#' @param ext file name and extension
#'
#' @keywords internal
get_result_path <- function(
    out_dir,
    pkg_path,
    ext = c("scorecard.json", "scorecard.pdf", "check.rds", "covr.rds")
){

  # Directory contains version, pkg_path does not
  pkg_name <- basename(dirname(pkg_path))

  ext <- match.arg(ext)

  file.path(out_dir, paste0(pkg_name,".",ext))
}
