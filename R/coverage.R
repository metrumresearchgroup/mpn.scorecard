#' Run covr and potentially save results to disk
#'
#' @param pkg_path package installation directory
#' @param out_dir directory for saving results
#' @param use_lib library path. Defaults to `.libPaths()`
#'
#' @keywords internal
add_coverage <- function(pkg_path, out_dir, use_lib = .libPaths()) {
  # run covr
  pkg_name <- basename(pkg_path)

  withr::with_libpaths(new = use_lib, {
    res_cov <- tryCatch({
      coverage <- covr::package_coverage(pkg_path, type = "tests")
      coverage_list <- covr::coverage_to_list(coverage)
      list(name = pkg_name, coverage = coverage_list)
    },
    error = function(cond){
      coverage_list <- list(filecoverage = cond, totalcoverage = NA_real_)
      list(name = pkg_name, coverage = coverage_list)
    },
    warning = function(cond){
      coverage_list <- list(filecoverage = cond, totalcoverage = NA_real_)
      list(name = pkg_name, coverage = coverage_list)
    })
  })


  # write results to RDS
  saveRDS(
    res_cov,
    get_result_path(out_dir, pkg_path, "covr.rds")
  )

  # return total coverage as fraction
  return(res_cov$coverage$totalcoverage/100)
}
