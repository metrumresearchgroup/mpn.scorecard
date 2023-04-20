
library(usethis)

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
    pass_warning = FALSE
){
  template_dir <- system.file("test-data", "pkg-templates", package = "mpn.scorecard", mustWork = TRUE)
  testing_dir <- file.path(system.file("", package = "mpn.scorecard", mustWork = TRUE), "testing_dir")

  pkg_dir <- file.path(testing_dir, pkg_name)
  if(fs::dir_exists(pkg_dir)) fs::dir_delete(pkg_dir)
  fs::dir_create(pkg_dir, recurse = TRUE)

  # tempalte files
  license_file <- file.path(template_dir, "license.txt")
  description_file <- file.path(template_dir, "description_file.txt")
  namespace_file <- file.path(template_dir, "namespace.txt")

  # modify and copy over core package files
  if(isFALSE(pass_warning)){
    make_pkg_file(pkg_name, license_file, file.path(pkg_dir, "LICENSE.md"))
  }
  make_pkg_file(pkg_name, description_file, file.path(pkg_dir, "DESCRIPTION"))
  fs::file_copy(namespace_file, file.path(pkg_dir, "NAMESPACE"))

  # init other directories and default files
  r_dir <- file.path(pkg_dir, "R")
  fs::dir_create(r_dir)
  script_file <- file.path(r_dir, "myscript.R")
  fs::file_create(script_file)

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
#' @param nest_resuts_dir Logical (T/F). If `TRUE`, create a subdirectory in `results` named `<pkg_name>_<pkg_version>`
#'
#' @details
#' pass_success should pass with flying colors
#' pass_warning is meant to trigger rcmdcheck warnings
#' fail_func vs fail_test can be used to trigger various failures - might be expanded on if needed
#'
#' `nest_resuts_dir` should be set to `TRUE` when **NOT** calling `score_pkg`, and `FALSE` when it is.
#' `score_pkg` creates the subdirectory itself. The purpose for changing this parameter is to retain the informative messages regarding rcmdcheck and covr failures
#'
#' @returns a list of file paths
#'
#' @keywords internal
create_testing_package <- function(
    pkg_name = "mypackage",
    type = c("pass_success", "pass_warning", "fail_func", "fail_test"),
    nest_resuts_dir = TRUE
){

  type <- match.arg(type)

  # Create temp package
  pkg_setup_dirs <- create_package_template(
    pkg_name = pkg_name,
    pass_warning = (type == "pass_warning")
  )

  # Add function to R/
  if(type == "fail_func"){
    # Add a file with a syntax error to the package
    writeLines("myfunction <- function(x { x + 1", pkg_setup_dirs$r_file)
  }else{
    writeLines("myfunction <- function(x) { x + 1}", pkg_setup_dirs$r_file)
  }

  # Add a test file to tests/testthat/
  if(type == "fail_test"){
    # Add a test file that will fail
    test_lines <- "testthat::test_that('this fails', { myfunction('blah') })"
  }else{
    test_lines <- "testthat::test_that('this works', { expect_equal(myfunction(1), 2)})"
  }
  writeLines(test_lines, pkg_setup_dirs$test_file)


  # Create temp directory for saving results, that follows the convention of this package:
  # namely, a directory with the basename of the package and version
  if(isTRUE(nest_resuts_dir)){
    pkg_name_ver <- get_pkg_desc(pkg_setup_dirs$pkg_dir, fields = c("Package", "Version")) %>%
      paste0(collapse = "_")
    results_dir <- file.path(pkg_setup_dirs$testing_dir, "results", pkg_name_ver)
  }else{
    results_dir <- file.path(pkg_setup_dirs$testing_dir, "results")
  }
  fs::dir_create(results_dir)

  # Build the package tarball
  tar_file <- devtools::build(pkg_setup_dirs$pkg_dir)

  return(
    list(
      testing_dir = pkg_setup_dirs$testing_dir,
      pkg_dir = pkg_setup_dirs$pkg_dir,
      tar_file = tar_file,
      results_dir = results_dir
    )
  )
}


#' Create five fake packages of all types and score them
#'
#' @returns a named list containing the `result_dirs` and overall `testing_dir` (for easy unlinking)
#'
#' @keywords internal
setup_multiple_pkg_scores <- function(){

  pkg_names <- paste0("package", seq(1:5))
  pkg_types <- c(rep("pass_success", 2), "pass_warning", "fail_func", "fail_test")

  result_dirs <- purrr::map2_chr(pkg_names, pkg_types, ~{
    pkg_setup <- create_testing_package(
      pkg_name = .x, type = .y,
      nest_resuts_dir = FALSE
    )

    score_pkg(
      pkg = pkg_setup$tar_file,
      out_dir = pkg_setup$results_dir,
      overwrite = TRUE
    )
  }) %>% suppressMessages()

  return(
    list(
      testing_dir = testing_dir,
      result_dirs = result_dirs
    )
  )
}



### Helpers ###

make_pkg_file <- function(
    pkg_name,
    template_file,
    new_file
){
  template_text <- readLines(template_file) %>% paste(collapse = "\n")
  template_text_new <- glue::glue(template_text) # references {pkg_name}
  writeLines(template_text_new, new_file)
}


cleanup_temp_dir <- function(dir){
  unlink(dir, recursive = TRUE)
  exists <- fs::dir_exists(dir)
  tibble::tibble(file_path = names(exists), exists = unname(exists))
}







### This will have to be changes, but structuring tests around this for now ###
# package_dir <- file.path("/data", "Projects", "package_dev")
# pkg_tar <- file.path(package_dir,"review_3.1.0.tar.gz") # tarball
# out_dir <- file.path(tempdir(), "results")

# A function like this could be used if we wanted to test real packages, but unlikely (keeping during development)
# setup_files <- function(pkg_tar){
#   # Unpack tarball
#   pkg_source_path <- unpack_tarball(pkg_tar)
#
#   # Get package name and version
#   pkg_desc <- get_pkg_desc(pkg_source_path, fields = c("Package", "Version"))
#   pkg_name <- pkg_desc$Package
#   pkg_ver <- pkg_desc$Version
#   pkg_name_ver <- paste0(pkg_name, "_", pkg_ver)
#
#   return(
#     list(
#       pkg_source_path = pkg_source_path,
#       pkg_name = pkg_name,
#       pkg_ver = pkg_ver,
#       pkg_name_ver = pkg_name_ver
#     )
#   )
# }
