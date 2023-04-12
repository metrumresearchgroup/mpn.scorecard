#' Run covr and potentially save results to disk
#'
#' @param pkg_source_path package installation directory
#' @param out_dir directory for saving results
#'
#' @details
#' The basename of `out_dir` should be the package name and version pasted together
#'
#' @keywords internal
add_coverage <- function(pkg_source_path, out_dir) {
  # run covr
  pkg_name <- basename(out_dir)

  res_cov <- tryCatch({
    coverage <- covr::package_coverage(pkg_source_path, type = "tests")
    coverage_list <- covr::coverage_to_list(coverage)
    list(name = pkg_name, coverage = coverage_list)
  },
  error = function(cond){
    coverage_list <- list(filecoverage = cond, totalcoverage = NA_integer_)
    list(name = pkg_name, coverage = coverage_list)
  },
  warning = function(cond){
    coverage_list <- list(filecoverage = cond, totalcoverage = NA_integer_)
    list(name = pkg_name, coverage = coverage_list)
  })


  # write results to RDS
  saveRDS(
    res_cov,
    get_result_path(out_dir, "covr.rds")
  )

  # return total coverage as fraction
  total_cov <- as.numeric(res_cov$coverage$totalcoverage/100)

  if(is.na(total_cov)){
    check_path <- get_result_path(out_dir, "check.rds")
    warning(glue::glue("R coverage failed. Read in the rcmdcheck output to see what went wrong: {check_path}"))
  }

  return(total_cov)
}
