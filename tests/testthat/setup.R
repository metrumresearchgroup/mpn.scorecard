
library(pdftools)
library(dplyr)



# Base rcmdcheck args. Set the `path` element to the file path of the tarball before using
rcmdcheck_args = list(
  timeout = Inf,
  args = "--no-manual",
  quiet = TRUE
)


#' Creates package scaffold
#'
#' @param pkg_name a name for the package. Useful to specify for informative messages or creating multiple packages at once
#' @param pass_warning Logical (T/F). If `TRUE`, don't copy over the license file to trigger a warning on rcmdcheck
create_package_template <- function(
    pkg_name = "mypackage",
    pass_warning = FALSE,
    pass_note = FALSE,
    add_tests = TRUE
){
  template_dir <- system.file("test-data", "pkg-templates", package = "mpn.scorecard", mustWork = TRUE)
  testing_dir <- file.path(system.file("", package = "mpn.scorecard", mustWork = TRUE), "testing_dir") %>% fs::path_norm() %>%
    as.character()

  pkg_dir <- file.path(testing_dir, pkg_name)
  if(fs::dir_exists(pkg_dir)) fs::dir_delete(pkg_dir)
  fs::dir_create(pkg_dir, recurse = TRUE)

  # tempalte files
  license_md_file <- file.path(template_dir, "license_md.txt")
  license_file <- file.path(template_dir, "license.txt")
  description_file <- file.path(template_dir, "description_file.txt")
  namespace_file <- file.path(template_dir, "namespace.txt")

  # modify and copy over core package files
  if(isFALSE(pass_warning)){ # (1 of 2) intentional warnings come from license file issues
    make_pkg_file(pkg_name, license_md_file, file.path(pkg_dir, "LICENSE.md"))
    make_pkg_file(pkg_name, license_file, file.path(pkg_dir, "LICENSE"))
  }
  # optional notes come from unused imports; additional warnings come from not importing dependencies
  make_pkg_file(pkg_name, description_file, file.path(pkg_dir, "DESCRIPTION"),
                pass_note = pass_note, pass_warning = pass_warning)
  fs::file_copy(namespace_file, file.path(pkg_dir, "NAMESPACE"))

  # init other directories and default files
  r_dir <- file.path(pkg_dir, "R")
  fs::dir_create(r_dir)
  script_file <- file.path(r_dir, "myscript.R")
  fs::file_create(script_file)

  if(isTRUE(add_tests)){
    # Add a test setup for running test suite (testthat.R)
    test_dir <- file.path(pkg_dir, "tests", "testthat")
    fs::dir_create(test_dir, recurse = TRUE)
    testthat_script <- file.path(dirname(test_dir), "testthat.R")
    fs::file_create(testthat_script)
    test_setup <- glue::glue("
    library(testthat)
    library({pkg_name})

    test_check('{pkg_name}')
    ")
    writeLines(test_setup, testthat_script)

    # Test file
    test_file <- file.path(test_dir, "test-myscript.R")
    fs::file_create(test_file)
  }


  return(
    list(
      testing_dir = testing_dir,
      pkg_dir = pkg_dir,
      r_file = script_file,
      test_file = test_file
    )
  )
}



#' Creates and builds a fake package
#'
#' @param pkg_name a name for the package. Useful to specify for informative messages or creating multiple packages at once
#' @param type type of package to make. See details.
#' @param nest_results_dir Logical (T/F). If `TRUE`, create a subdirectory in `results` named `<pkg_name>_<pkg_version>`
#'
#' @details
#' pass_success should pass with flying colors
#' pass_warning is meant to trigger rcmdcheck warnings
#' fail_func vs fail_test can be used to trigger various failures - might be expanded on if needed
#'
#' `nest_results_dir` should be set to `TRUE` when **NOT** calling `score_pkg`, and `FALSE` when it is.
#' `score_pkg` creates the subdirectory itself. The purpose for changing this parameter is to retain the informative messages regarding rcmdcheck and covr failures
#'
#' `type` = 'no_test' indicates an empty test file, `type` = 'no_test_suite' indicates there is no `test` directory at all
#'
#' @returns a list of file paths
#'
#' @keywords internal
create_testing_package <- function(
    pkg_name = "mypackage",
    type = c("pass_success", "pass_warning", "pass_notes", "pass_no_test", "pass_no_test_suite", "pass_no_functions",
             "fail_func_syntax", "fail_test"),
    nest_results_dir = TRUE
){

  type <- match.arg(type)

  # Create temp package
  pkg_setup_dirs <- create_package_template(
    pkg_name = pkg_name,
    pass_warning = (type == "pass_warning"),
    pass_note = (type == "pass_notes"),
    add_tests = !(type %in% c("pass_no_test_suite", "pass_no_functions"))
  )

  # Add function to R/  (unless type = 'pass_no_functions')
  if(type == "fail_func_syntax"){
    # Add a file with a syntax error to the package
    func_lines <- "myfunction <- function(x { x + 1"
  }else if(type != "pass_no_functions"){
    func_lines <- glue::glue("
    #' Adds 1 to x
    #' @param x a number
    #' @export
    myfunction <- function(x) { x + 1}
    ", .open = "{{", .close = "}}")
  }

  if(type != "pass_no_functions"){
    writeLines(func_lines, pkg_setup_dirs$r_file)
    # Export function
    if(type != "fail_func_syntax"){
      # Basically run `devtools::document()` if the function is suitable
      roxygen2::roxygenise(pkg_setup_dirs$pkg_dir) %>% suppressMessages()
    }else{
      # Manually export function if syntax issue is present (only way this scenario could happen)
      ns_file <- file.path(pkg_setup_dirs$pkg_dir, "NAMESPACE")
      writeLines("export(myfunction)", ns_file)
    }
  }

  # Add a test file to tests/testthat/ (unless type = 'pass_no_test_suite' or 'pass_no_functions')
  if(type == "fail_test"){
    # Add a test file that will fail
    test_lines <- "testthat::test_that('this fails', { myfunction('blah') })"
  }else if(type == "pass_no_test"){
    test_lines <- ""
  }else if(!(type %in% c("pass_no_test_suite", "pass_no_functions"))){
    test_lines <- "testthat::test_that('this works', { expect_equal(myfunction(1), 2)})"
  }

  if(!(type %in% c("pass_no_test_suite", "pass_no_functions"))){
    writeLines(test_lines, pkg_setup_dirs$test_file)
  }


  # Create temp directory for saving results, that follows the convention of this package:
  # namely, a directory with the basename of the package and version
  if(isTRUE(nest_results_dir)){
    pkg_name_ver <- get_pkg_desc(pkg_setup_dirs$pkg_dir, fields = c("Package", "Version")) %>%
      paste0(collapse = "_")
    results_dir <- file.path(pkg_setup_dirs$testing_dir, "results", pkg_name_ver) %>% fs::path_norm() %>%
      as.character()
  }else{
    results_dir <- file.path(pkg_setup_dirs$testing_dir, "results") %>% fs::path_norm() %>%
      as.character()
  }
  fs::dir_create(results_dir)

  # Build the package tarball
  tar_file <- devtools::build(pkg_setup_dirs$pkg_dir, quiet = TRUE)

  return(
    list(
      pkg_dir = pkg_setup_dirs$pkg_dir,
      tar_file = tar_file,
      pkg_result_dir = results_dir,
      testing_dir = pkg_setup_dirs$testing_dir
    )
  )
}


#' Create fake packages of all types
#'
#' @returns a named list containing the `results_dir` and overall `testing_dir` (for easy unlinking)
#'
#' @keywords internal
setup_multiple_pkgs <- function(){

  pkg_names <- paste0("package", 1:8)
  pkg_types <- c("pass_success", "pass_warning", "pass_notes", "pass_no_test", "pass_no_test_suite", "pass_no_functions",
                 "fail_func_syntax", "fail_test")

  pkg_setups <- purrr::map2_dfr(pkg_names, pkg_types, ~{
    pkg_setup <- create_testing_package(
      pkg_name = .x, type = .y,
      nest_results_dir = TRUE
    )

    cbind(
      pkg_name = .x,
      pkg_type = .y,
      tibble::as_tibble(pkg_setup),
      all_results_dir = dirname(pkg_setup$pkg_result_dir)
    )
  })

  overall_dirs <- pkg_setups %>% dplyr::select(c(all_results_dir, testing_dir)) %>% dplyr::distinct() %>% as.list()
  pkg_setups_df <- pkg_setups %>% dplyr::select(-c(all_results_dir, testing_dir))

  pkg_setup <- c(list(pkg_setups_df = pkg_setups_df), overall_dirs)

  return(pkg_setup)
}



### Helpers ###

make_pkg_file <- function(
    pkg_name,
    template_file,
    new_file,
    # below args are only used for description file
    pass_note = FALSE,
    pass_warning = FALSE
){
  template_text <- readLines(template_file) %>% paste(collapse = "\n")
  imports <- if(isTRUE(pass_note)) "dplyr" else ""
  suggests <- if(isFALSE(pass_warning)) "testthat" else ""
  template_text_new <- glue::glue(template_text) # references {pkg_name}
  writeLines(template_text_new, new_file)
}


#' Skip test if PDF rendering is required
#'
#' @keywords internal
skip_if_render_pdf <- function() {
  if (Sys.getenv("METWORX_VERSION") == "" || nzchar(Sys.getenv("SKIP_RENDER_TESTS"))) {
    testthat::skip("skipping pdf rendering test")
  }
}

# Build all packages
pkg_dirs <- setup_multiple_pkgs()

# select specific packages for scoring (other types may only be used in `test-results` or other tests)
# only need a subset of these for majority of tests
pkg_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type %in% c("pass_success", "pass_warning", "fail_func_syntax", "fail_test"))
pkg_tars <- pkg_select %>% dplyr::pull(tar_file)

# score select packages
result_dirs_select <- purrr::map_chr(pkg_tars, ~{
  score_pkg(.x, pkg_dirs$all_results_dir, overwrite = TRUE) %>% suppressMessages()
})

# run at the end of all tests
withr::defer(unlink(pkg_dirs$testing_dir, recursive = TRUE), teardown_env())

