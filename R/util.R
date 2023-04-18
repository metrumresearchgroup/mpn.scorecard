#' Check if a path exists and delete the file
#' if overwrite is TRUE
#' @param path a file path to check if it exists
#' @param overwrite Logical (T/F). If `TRUE`, delete the file at the specified path
#'
#' @keywords internal
check_exists_and_overwrite <- function(path, overwrite) {
  checkmate::assert_string(path)
  checkmate::assert_logical(overwrite, len = 1)
  if (fs::file_exists(path)) {
    if (isTRUE(overwrite)) {
      fs::file_delete(path)
    } else {
      abort(glue::glue("{path} already exists. Pass overwrite = TRUE to overwrite it."))
    }
  }
}

#' Assign output file path for various outputs during scorecard rendering
#'
#' @param out_dir output directory for saving results and json
#' @param ext file name and extension
#'
#' @details
#' The basename of `out_dir` should be the package name and version pasted together
#'
#' @keywords internal
get_result_path <- function(
    out_dir,
    ext = c("scorecard.json", "scorecard.pdf", "check.rds", "covr.rds", "mitigation.txt", "summary.pdf")
){

  ext <- match.arg(ext)

  pkg_name <- basename(out_dir)

  file.path(out_dir, paste0(pkg_name,".",ext))
}

#' Read Description file and parse the package name and version
#'
#' @param pkg_source_path path to package source code (untarred)
#'
#' @keywords internal
get_pkg_desc <- function(pkg_source_path, fields = NULL){

  pkg_desc_path <- file.path(pkg_source_path, "DESCRIPTION")

  desc_file <- read.dcf(pkg_desc_path, fields = fields)[1L,]
  pkg_desc <- as.list(desc_file)

  return(pkg_desc)
}

#' Untar package and return installation directory
#'
#' @param pkg_tar path to tarball package
#' @param temp_file_name name of `tempfile`
#'
#' @keywords internal
unpack_tarball <- function(pkg_tar, temp_file_name = "SCORECARD_"){
  # Create temporary location for package installation
  temp_pkg_dir <- tempfile(temp_file_name)
  if (!dir.create(temp_pkg_dir)) stop("unable to create ", temp_pkg_dir)

  source_tar_dir <- file.path(temp_pkg_dir)

  # unpack tarball

  utils::untar(pkg_tar, exdir = source_tar_dir)

  # unpackacked package path
  pkg_source_path <- dir_ls(source_tar_dir)

  # Confirm tar is unpackacked in expected directory
  checkmate::assert_string(pkg_source_path)
  checkmate::assert_directory_exists(pkg_source_path)

  return(pkg_source_path)
}

