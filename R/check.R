# Run R CMD CHECK

# this is a stub...
add_rcmdcheck <- function(pkg, out_dir) {
  # run rcmdcheck
  pkgname <- basename(pkg)
  res <- list(name = pkgname)

  # write results to RDS
  saveRDS(
    res,
    file.path(out_dir, paste0(pkgname, ".check.rds"))
  )

  # return 1 if passed, 0 if not.
  # TODO: do we want anything more granular than this? riskmetric has some weighting system...
  return(1)
}

#### Code from toolchain repo to work off of...
#
# #library(callr)
# # from toolchain/check-scripts/check-queue-fs.R
# check_package <- function(pkgpath, lib_path, output_dir) {
#   .libPaths(lib_path)
#
#   name <- tools::file_path_sans_ext(tools::file_path_sans_ext(basename(pkgpath)))
#   check_save_path <- file.path(output_dir, paste0(name, ".rds"))
#
#   if (file.exists(check_save_path)) {
#     message("rcmdcheck object unexpectedly exists: ", check_save_path)
#     return(invisible(NULL))
#   }
#
#   tryCatch({
#     Sys.setenv("_R_CHECK_FORCE_SUGGESTS_" = 0)
#     Sys.setenv("NOT_CRAN" = TRUE)
#     check_res <- rcmdcheck::rcmdcheck(pkgpath, args = "--no-manual",
#                                       timeout = 7200)
#     saveRDS(check_res, check_save_path)
#   },
#   error = function(.e) {
#     message(glue::glue("Check for {name} failed: {.e}"))
#   })
#
#   return(invisible(NULL))
# }
#
# # from toolchain/check-scripts/check-queue-fs.R
# run_cmd_check <- function(pkgpath, lib_path, check_result_dir) {
#   callr::r_vanilla(
#     check_package,
#     args = list(pkgpath = pkgpath,  lib_path = lib_path,
#                 output_dir = check_result_dir),
#     libpath = lib_path,
#     env = c(R_LIBS_SITE = lib_path))
#
#   elapsed_time <- difftime(Sys.time(), start_time)
#
#   message(glue::glue("{pkgname}: completed after {elapsed_time}"))
# }
