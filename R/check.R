#' Run R CMD CHECK
#'
#' @param out_dir directory for saving results
#' @param rcmdcheck_args list of arguments to pass to `rcmdcheck::rcmdcheck`
#'
#' @details
#' rcmdcheck takes either a tarball or an installation directory.
#'
#' The basename of `out_dir` should be the package name and version pasted together
#'
#' The returned score is calculated via a weighted sum of notes (0.10), warnings (0.25), and errors (1.0). It has a maximum score of 1 (indicating no errors, notes or warnings)
#' and a minimum score of 0 (equal to 1 error, 4 warnings, or 10 notes). This scoring methodology is taken directly from [riskmetric::metric_score.pkg_metric_r_cmd_check()].
#'
#' @keywords internal
add_rcmdcheck <- function(out_dir, rcmdcheck_args) {

  # We never want the rcmdcheck to fail
  rcmdcheck_args$error_on <- "never"

  # run rcmdcheck
  pkg_name <- basename(out_dir)

  res_check <- do.call(rcmdcheck::rcmdcheck, rcmdcheck_args)

  # write results to RDS
  saveRDS(
    res_check,
    get_result_path(out_dir, "check.rds")
  )

  # Note that res_check$status is the opposite of what we want (1 means failure, 0 means passing)

  # Scoring is the weighted sum of notes (0.1), errors (1) and warnings (0.25) (scoring method taken from `riskmetric`)
  sum_vars <- c(notes = length(res_check$notes), warnings = length(res_check$warnings), errors = length(res_check$errors))
  score_weightings <- c(notes = 0.1, warnings = 0.25, errors = 1)
  check_score <- 1 - min(c(sum(score_weightings*sum_vars), 1))

  if(check_score == 1){
    message(glue::glue("rcmdcheck for {pkg_name} passed"))
  }else if(check_score < 1 && check_score > 0){
    message(glue::glue("rcmdcheck for {pkg_name} passed with warnings and/or notes"))
  }else if(check_score == 0){
    check_path <- get_result_path(out_dir, "check.rds")
    message(glue::glue("rcmdcheck for {pkg_name} failed. Read in the rcmdcheck output to see what went wrong: {check_path}"))
  }

  return(check_score)
}
