#' Create extra notes summarizing the covr & rcmdcheck outputs, and documentation
#'
#' @inheritParams render_scorecard
#'
#' @keywords internal
create_extra_notes <- function(
    results_dir
){
  covr_path <- get_result_path(results_dir, "covr.rds")
  check_path <- get_result_path(results_dir, "check.rds")

  # Format rcmdcheck
  check_results <- readRDS(check_path)

  # Format coverage
  covr_results <- readRDS(covr_path)
  if (inherits(covr_results$errors, "error")) {
    covr_results_df <- data.frame(
      r_script = "File coverage failed",
      test_coverage = conditionMessage(covr_results$errors)
    )
  } else if (length(covr_results$coverage$filecoverage)) {
    covr_results_df <- covr_results$coverage$filecoverage %>% as.data.frame()
    covr_results_df <- covr_results_df %>%
      mutate(r_script = row.names(covr_results_df)) %>%
      dplyr::select("r_script", "test_coverage" = ".")
    row.names(covr_results_df) <- NULL
  } else {
    covr_results_df <- data.frame(
      r_script = "No coverage results",
      test_coverage = covr_results$notes
    )
  }

  return(
    list(
      covr_results_df = covr_results_df,
      check_output = check_results$stdout
    )
  )
}

