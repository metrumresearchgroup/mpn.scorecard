#' Generate scorecard metrics for package
#'
#' Returns collected metrics as a named list, and also writes
#' results to disk for downstream consumption.
#'
#' @param pkg a package tarball
#' @param out_dir output directory for saving results and json
#' @param pkg_info optional manually filled info
#' @param overwrite Logical (T/F). Whether or not to overwrite existing scorecard results
#' @param rcmdcheck_args list of arguments to pass to `rcmdcheck`
#'
#' @returns a file path to a json file containing all scores
#'
#' @export
score_pkg <- function(
  pkg,
  out_dir,
  pkg_info = NULL,  # optional manually filled info, probably as JSON/YAML
  overwrite = FALSE,
  rcmdcheck_args = list(
    timeout = Inf,
    args = "--no-manual",
    quiet = TRUE,
    error_on = "never"
  )
) {
  # Input checking
  checkmate::assert_string(pkg)
  checkmate::assert_file_exists(pkg)
  checkmate::assert_string(out_dir)
  checkmate::assert_list(rcmdcheck_args)

  pkg_desc <- get_pkg_desc(pkg_path)
  pkg_name <- pkg_desc$Package
  pkg_ver <- pkg_desc$Version
  pkg_name_ver <- paste0(pkg_name, "_", pkg_ver)

  # Create temporary location for package installation
  temp_pkg_dir <- tempfile("PACKAGES")
  if (!dir.create(temp_pkg_dir)) stop("unable to create ", temp_pkg_dir)
  on.exit(unlink(temp_pkg_dir, recursive = TRUE), add = TRUE)

  # unpack tarball
  source_tar_dir <- file.path(temp_pkg_dir, "scorecard", pkg_name_ver)
  utils::untar(pkg, exdir = source_tar_dir)

  # unpackacked package path
  pkg_path <- dir_ls(source_tar_dir)

  # Confirm tar is unpackacked in expected directory
  checkmate::assert_string(pkg_path)
  checkmate::assert_directory_exists(pkg_path)


  # start building up scorecard list
  res <- create_score_list_from_riskmetric(pkg_path, pkg_name, pkg_ver)

  if (!fs::dir_exists(out_dir)) fs::dir_create(out_dir)

  # TODO: get name and version _not_ from riskmetric
  # so that we can a) be independent and b) put this at the top.
  # We'll also need to remove the pkg_name and pkg_version from create_score_list_from_riskmetric()
  out_path <- get_result_path(out_dir, pkg_path, "scorecard.json")
  check_exists_and_overwrite(out_path, overwrite)

  # run check and covr and write results to disk
  rcmdcheck_args$path <- pkg
  res$scores$testing$check <- add_rcmdcheck(pkg_path, out_dir, rcmdcheck_args) # use tarball
  res$scores$testing$covr <- add_coverage(pkg_path, out_dir) # must use untarred package dir

  # capture system and package metadata
  res$metadata <- get_metadata() # TODO: at some point expose args to this

  # overwrite anything that was passed via pkg_info
  if(!is.null(pkg_info)) {
    res <- add_pkg_info(res, pkg_info)
  }

  # calculate overall scores
  res <- calc_overall_scores(res) # TODO: do we want this algorithm to be configurable? If so, what's the interface?

  # and writes out to a JSON file for other functions to consume
  writeLines(
    jsonlite::toJSON(res, pretty = TRUE, auto_unbox = TRUE),
    out_path
  )


  return(invisible(out_path))
}
