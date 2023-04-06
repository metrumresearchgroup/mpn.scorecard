#' Generate scorecard metrics for package
#'
#' Returns collected metrics as a named list, and also writes
#' results to disk for downstream consumption.
#'
#' @param pkg a package tarball
#' @param out_dir output directory for saving results and json
#' @param pkg_info optional manually filled info
#' @param use_lib library path. Defaults to `.libPaths()`
#' @param overwrite Logical (T/F). Whether or not to overwrite existing scorecard results
#'
#' @returns a file path to a json file containing all scores
#'
#' @export
score_pkg <- function(
  pkg,
  out_dir,
  pkg_info = NULL,  # optional manually filled info, probably as JSON/YAML
  use_lib = .libPaths(),
  overwrite = FALSE
) {
  # Input checking
  checkmate::assert_string(pkg)
  checkmate::assert_file_exists(pkg)
  checkmate::assert_directory_exists(use_lib)
  checkmate::assert_string(out_dir)

  # unpack tarball
  source_tar_dir <- file.path(tempdir(), "scorecard", gsub(".tar.gz", "", basename(pkg)))
  utils::untar(pkg, exdir = source_tar_dir)

  # make helper to determine package name
  source_dir <- dir_ls(source_tar_dir)

  # Confirm tar is unpackacked in expected directory
  checkmate::assert_string(source_dir)
  checkmate::assert_directory_exists(source_dir)

  pkg_name <- basename(source_dir)

  # start building up scorecard list
  res <- create_score_list_from_riskmetric(source_dir)

  if (!fs::dir_exists(out_dir)) fs::dir_create(out_dir)

  # TODO: get name and version _not_ from riskmetric
  # so that we can a) be independent and b) put this at the top.
  # We'll also need to remove the pkg_name and pkg_version from create_score_list_from_riskmetric()
  out_path <- file.path(out_dir, paste0(res$pkg_name, ".scorecard.json"))
  check_exists_and_overwrite(out_path, overwrite)

  # run check and covr and write results to disk
  res$scores$testing$check <- add_rcmdcheck(source_dir, out_dir, use_lib)
  res$scores$testing$covr <- add_coverage(source_dir, out_dir)

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
