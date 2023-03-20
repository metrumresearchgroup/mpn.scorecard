# Run covr and potentially save results to disk
#
# Depending on how involved this gets, consider combining with check.R

# this is a stub...
add_coverage <- function(pkg, out_dir) {
  # run covr
  pkgname <- basename(pkg)
  res <- list(
    name = pkgname,
    coverage = round(runif(1, 0.4, 0.8), 3)
  )

  # potentially write results to RDS?
  # saveRDS(
  #   res,
  #   file.path(out_dir, paste0(pkgname, ".coverage.RDS"))
  # )

  # return percent coverage
  return(res$coverage)
}
