
#' Render PDF summary of scorecards
#'
#' @param json_paths a vector of json paths
#' @param snapshot a report subtitle indicating the grouping of these packages, such as an MPN snapshot.
#'
#' @export
render_scorecard_summary <- function(json_paths, out_dir, snapshot = NULL){

  checkmate::assert_string(snapshot, null.ok = TRUE)

  if(!is.null(snapshot)){
    snapshot <- paste("Snapshot", snapshot)
  }

  rmarkdown::render(
    system.file(SUM_SCORECARD_RMD_TEMPLATE, package = "mpn.scorecard"),
    output_dir = out_dir,
    output_file = basename(out_file),
    quiet = TRUE,
    params = list(
      pkg_scores = formatted_pkg_scores,
      set_subtitle = snapshot
    )
  )
}
