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
