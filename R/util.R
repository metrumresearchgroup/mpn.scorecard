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

#' Read Description file and parse the package name and version
#'
#' @param pkg_path path to installed/untarred package
#'
#' @keywords internal
get_pkg_desc <- function(pkg_path, fields = NULL){

  pkg_desc_path <- file.path(pkg_path, "DESCRIPTION")
  desc_file <- tryCatch(read.dcf(pkg_desc_path, fields = fields)[1L,], error = identity)
  if (!inherits(desc_file, "error")) {
    pkg_desc <- as.list(desc_file)
  }else{
    message(gettextf("reading DESCRIPTION for package %s failed with message:\n  %s",
                     sQuote(basename(dirname(pkg_desc_path))), conditionMessage(desc_file)),
            domain = NA)
    pkg_desc <- NULL # TODO: maybe add error handling for this later
  }

  return(pkg_desc)
}
