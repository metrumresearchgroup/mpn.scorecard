#' Generate scorecard metrics for package
#'
#' Returns collected metrics as a named list, and also writes
#' results to disk for downstream consumption.
#' @export
score_pkg <- function(
  pkg,  # github repo url, tarball, other ways to pass a pkg...
  out_dir,
  pkg_info = NULL  # optional manually filled info, probably as JSON/YAML
) {
  # TODO: some input checking

  # start building up scorecard list
  res <- create_score_list_from_riskmetric(pkg)

  # run check and covr and write results to disk
  res$testing$check <- add_rcmdcheck(pkg, out_dir)
  res$testing$check <- add_coverage(pkg, out_dir)

  # capture system and package metadata
  res$metadata <- get_metadata() # TODO: at some point expose args to this

  # overwrite anything that was passed via pkg_info
  if(!is.null(pkg_info)) {
    res <- add_pkg_info(res, pkg_info)
  }

  # and writes out to a JSON file for other functions to consume
  writeLines(
    jsonlite::toJSON(res),
    file.path(out_dir, paste0(res$pkg_name, ".scorecard.json"))
  )

  return(invisible(res))
}
