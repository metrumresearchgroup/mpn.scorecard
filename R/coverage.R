#' Run covr and potentially save results to disk
#'
#' @param pkg_source_path package installation directory
#' @param out_dir directory for saving results
#' @param timeout Timeout to pass to [callr::r_safe()] when running covr.
#'
#' @details
#' The basename of `out_dir` should be the package name and version pasted together
#'
#' @keywords internal
add_coverage <- function(pkg_source_path, out_dir, timeout = Inf) {
  # run covr
  pkg_name <- basename(out_dir)

  res_cov <- tryCatch({
    coverage_list <- run_covr(pkg_source_path, timeout)

    # If no testable functions are found in the package, `filecoverage` and `totalcoverage`
    # will yield logical(0) and NaN respectively. Coerce to usable format
    if(is.na(coverage_list$totalcoverage)){
      if(rlang::is_empty(coverage_list$filecoverage) && is.logical(coverage_list$filecoverage)){
        coverage_list$totalcoverage <- 0
        notes <- "no testable functions found"
      }else{
        abort("Total coverage returned NaN. This likely means the package had non-standard characteristics. Contact the developer to add support")
      }
    }else{
      notes <- NA
    }

    list(name = pkg_name, coverage = coverage_list, errors = NA, notes = notes)
  },
  error = function(cond){
    coverage_list <- list(filecoverage = NA, totalcoverage = NA_integer_)
    list(
      name = pkg_name, coverage = coverage_list,
      errors = wrap_callr_error(cond),
      notes = NA
    )
  })




  # write results to RDS
  saveRDS(
    res_cov,
    get_result_path(out_dir, "covr.rds")
  )

  # return total coverage as fraction
  total_cov <- as.numeric(res_cov$coverage$totalcoverage/100)

  if(is.na(total_cov)){
    covr_path <- get_result_path(out_dir, "covr.rds")
    message(glue::glue("R coverage for {pkg_name} failed. Read in the covr output to see what went wrong: {covr_path}"))
  }

  if(!is.na(res_cov$notes)){
    message(glue::glue("R coverage for {pkg_name} had notes: {res_cov$notes}"))
  }

  return(total_cov)
}

#' Run covr in subprocess with timeout
#'
#' @noRd
run_covr <- function(path, timeout) {
  callr::r_safe(
    function(p) {
      covr::coverage_to_list(covr::package_coverage(p, type = "tests"))
    },
    args = list(path),
    libpath = .libPaths(),
    repos = NULL,
    package = FALSE,
    user_profile = FALSE,
    error = "error",
    timeout = timeout
  )
}

wrap_callr_error <- function(e) {
  class(e) <- c("scorecard_covr_error", class(e))
  return(e)
}

#' @export
conditionMessage.scorecard_covr_error <- function(c) {
  # Prevent rlib_error_3_0 method from adding ansi escape sequences, which would
  # trigger a LateX failure on render.
  withr::local_options(list("crayon.enabled" = FALSE))
  NextMethod()
}
