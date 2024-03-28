
library(pdftools)
library(dplyr)
library(roxygen2)

# Template file for comments
comments_template <- system.file("test-data", "comments-example.txt",
                                 package = "mpn.scorecard", mustWork = TRUE)

# Base rcmdcheck args. Set the `path` element to the file path of the tarball before using
rcmdcheck_args = list(
  timeout = Inf,
  args = "--no-manual",
  quiet = TRUE
)


#' Skip test if PDF rendering is required
#'
#' @keywords internal
skip_if_render_pdf <- function() {
  if (Sys.getenv("METWORX_VERSION") == "" || nzchar(Sys.getenv("SKIP_RENDER_TESTS"))) {
    testthat::skip("skipping pdf rendering test")
  }
}

### Set up packages ###

# Build all packages
pkg_dirs <- setup_multiple_pkgs()

# select specific packages for scoring (other types may only be used in `test-results` or other tests)
# only need a subset of these for majority of tests
pkg_select <- pkg_dirs$pkg_setups_df %>%
  dplyr::filter(pkg_type %in% c(
    "pass_success", "pass_no_docs", "pass_no_functions",
    "fail_func_syntax", "fail_test"
  ))
pkg_tars <- pkg_select %>% dplyr::pull(tar_file)
names(pkg_tars) <- pkg_select$pkg_type

# score select packages
result_dirs_select <- purrr::map_chr(pkg_tars, ~{
  local_check_envvar()
  score_pkg(.x, pkg_dirs$all_results_dir, overwrite = TRUE) %>% suppressMessages()
})

# run at the end of all tests
withr::defer(unlink(pkg_dirs$testing_dir, recursive = TRUE), teardown_env())

