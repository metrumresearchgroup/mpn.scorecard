#' Run covr and potentially save results to disk
#'
#' @param source_dir package installation directory
#' @param out_dir directory for saving results
#'
#' @keywords internal
add_coverage <- function(source_dir, out_dir) {
  # run covr
  pkg_name <- basename(source_dir)

  coverage <- covr::package_coverage(source_dir, type = "tests")
  coverage_list <- covr::coverage_to_list(coverage)

  cov_res <- list(
    name = pkg_name,
    coverage = coverage_list
  )


  # write results to RDS
  saveRDS(
    cov_res,
    file.path(out_dir, paste0(pkg_name, ".coverage.RDS"))
  )

  # return total coverage as fraction
  return(coverage_list$totalcoverage/100)
}
