#' Run R CMD CHECK
#'
#'
#' @param pkg_path package installation directory
#' @param out_dir directory for saving results
#'
#' @details
#' rcmdcheck takes either a tarball or an installation directory
#'
#' @keywords internal
add_rcmdcheck <- function(pkg_path, out_dir, rcmdcheck_args) {

  # rcmdcheck takes either a tarball or an installation directory
  # use installation directory so we dont have to pass the pacakge name as an additional argument

  # run rcmdcheck
  pkg_name <- basename(pkg_path)

  res_check <- tryCatch({
    do.call(rcmdcheck::rcmdcheck, rcmdcheck_args)
  },
  error = function(cond){
    return(cond)
  },
  warning = function(cond){
    return(cond)
  }
  )

  # write results to RDS
  saveRDS(
    res_check,
    get_result_path(out_dir, pkg_path, "check.rds")
  )

  # Note that res_check$status is the opposite of what we want (1 means failure, 0 means passing)

  # 1 if no errors or warnings (ignore notes)
  # 0.5 if any warnings, but no errors
  # 0 if any errors, or if the call failed
  status <- dplyr::case_when(
    rlang::is_empty(res_check$warnings) && rlang::is_empty(res_check$errors) && res_check$status == 0 ~ 1,
    !rlang::is_empty(res_check$warnings) && rlang::is_empty(res_check$errors) ~ 0.5,
    !rlang::is_empty(res_check$errors) || res_check$status == 1 ~ 0,
    TRUE ~ NA_integer_
  )

  return(status)
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
