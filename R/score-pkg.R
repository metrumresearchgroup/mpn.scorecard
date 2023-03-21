#' Generate scorecard metrics for package
#'
#' Returns collected metrics as a named list, and also writes
#' results to disk for downstream consumption.
#' @export
score_pkg <- function(
  pkg,  # probably just tarball, maybe other ways to pass a pkg eventually...
  out_dir,
  pkg_info = NULL  # optional manually filled info, probably as JSON/YAML
) {
  # TODO: some input checking

  # start building up scorecard list
  res <- create_score_list_from_riskmetric(pkg)

  # run check and covr and write results to disk
  res$scores$testing$check <- add_rcmdcheck(pkg, out_dir)
  res$scores$testing$check <- add_coverage(pkg, out_dir)

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
    file.path(out_dir, paste0(res$pkg_name, ".scorecard.json"))
  )

  return(invisible(res))
}
