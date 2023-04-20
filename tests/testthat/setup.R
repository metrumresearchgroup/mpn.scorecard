
library(usethis)

# Base rcmdcheck args. Set the `path` element to the file path of the tarball before using
rcmdcheck_args = list(
  timeout = Inf,
  args = "--no-manual",
  quiet = TRUE
)


#' Creates and builds a fake package
#'
#' @param pkg_name a name for the package. Useful to specify for informative messages or creating multiple packages at once
#' @param testing_dir directory to contain package tarball, installed package, and results (RDS and json)
#' @param type type of package to make. See details.
#' @param nest_resuts_dir Logical (T/F). If `TRUE`, create a subdirectory in `results` named `<pkg_name>_<pkg_version>`
#'
#' @details
#' pass_success should pass with flying colors
#' pass_warning is meant to trigger rcmdcheck warnings
#' fail_func vs fail_test can be used to trigger various failures - might be expanded on if needed
#'
#' `testing_dir` can be set outside the function in the event you want all packages to nest in the same tempfile
#' This should be done in most cases (when running multiple packages) to reflect the default package behavior
#'
#' `nest_resuts_dir` should be set to `TRUE` when **NOT** calling `score_pkg`, and `FALSE` when it is.
#' `score_pkg` creates the subdirectory itself. The purpose for changing this parameter is to retain the informative messages regarding rcmdcheck and covr failures
#'
#' @returns a list of file paths
#'
#' @keywords internal
create_testing_package <- function(
    pkg_name = "mypackage",
    testing_dir = tempfile("pkg_temp_"),
    type = c("pass_success", "pass_warning", "fail_func", "fail_test"),
    nest_resuts_dir = TRUE
){

  type <- match.arg(type)

  # Create temp package
  pkg_dir <- file.path(testing_dir, pkg_name)
  fs::dir_create(pkg_dir)
  create_package(pkg_dir, rstudio = FALSE, open = FALSE,
                 fields = list(
                   License = "MIT + file LICENSE",
                   `Authors@R` = 'person("Jane", "Doe", email = "jane@example.com",
                          role = c("aut", "cre"),
                          comment = c(ORCID = "YOUR-ORCID-ID"))'
                 )
  )

  # If success - need to add license file to suppress warnings
  if(type == "pass_success"){
    # Add license file and suggested packages to avoid warnings
    local_project(pkg_dir, setwd = FALSE)
    use_mit_license(copyright_holder = NULL)
    use_package("testthat", type = "Suggests", min_version = NULL)
    withr::deferred_run()
  }

  # Add function to R/
  script_file <- file.path(pkg_dir, "R", "myscript.R")
  fs::file_create(script_file)
  if(type == "fail_func"){
    # Add a file with a syntax error to the package
    writeLines("myfunction <- function(x { x + 1", script_file)
  }else{
    writeLines("myfunction <- function(x) { x + 1}", script_file)
  }

  # Add a test setup for running test suite
  test_dir <- file.path(pkg_dir, "tests", "testthat")
  testthat_script <- file.path(dirname(test_dir), "testthat.R")
  fs::dir_create(test_dir, recurse = TRUE)
  fs::file_create(testthat_script)
  test_setup <- glue::glue("
    library(testthat)
    library({pkg_name})

    test_check('{pkg_name}')
    ")
  writeLines(test_setup, testthat_script)

  # Add a test file to tests/testthat/
  test_file <- file.path(test_dir, "test-myscript.R")
  fs::file_create(test_file)
  if(type == "fail_test"){
    # Add a test file that will fail
    test_lines <- "testthat::test_that('this fails', { myfunction('blah') })"
  }else{
    test_lines <- "testthat::test_that('this works', { expect_equal(myfunction(1), 2)})"
  }
  writeLines(test_lines, test_file)


  # Create temp directory for saving results, that follows the convention of this package:
  # namely, a directory with the basename of the package and version
  if(isTRUE(nest_resuts_dir)){
    desc_file <- get_pkg_desc(pkg_dir, fields = c("Package", "Version"))
    pkg_name_ver <- paste0(desc_file, collapse = "_")
    results_dir <- file.path(testing_dir, "results", pkg_name_ver)
  }else{
    results_dir <- file.path(testing_dir, "results")
  }

  fs::dir_create(results_dir)


  # Build the package tarball
  tar_file <- devtools::build(pkg_dir)

  return(
    list(
      temp_dir = testing_dir,
      pkg_dir = pkg_dir,
      tar_file = tar_file,
      results_dir = results_dir
    )
  )
}


#' Create five fake packages of all types and score them
#'
#' @param testing_dir directory to contain packages and results
#'
#' @returns a named list containing the `result_dirs` and overall `testing_dir` (for easy unlinking)
#'
#' @keywords internal
setup_multiple_pkg_scores <- function(
    testing_dir = tempfile("pkg_temp_")
){

  pkg_names <- paste0("package", seq(1:5))
  pkg_types <- c(rep("pass_success", 2), "pass_warning", "fail_func", "fail_test")

  result_dirs <- purrr::map2_chr(pkg_names, pkg_types, ~{
    pkg_setup <- create_testing_package(
      pkg_name = .x, type = .y,
      testing_dir = testing_dir,
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
