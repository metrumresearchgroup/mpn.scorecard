
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
#' @param type type of package to make. See details.
#'
#' @details
#' pass_success should pass with flying colors
#' pass_warning is meant to trigger rcmdcheck warnings
#' fail_func vs fail_test can be used to trigger various failures - might be expanded on if needed
#'
#' @returns a list of file paths
#'
#' @keywords internal
create_testing_package <- function(
    pkg_name = "mypackage",
    type = c("pass_success", "pass_warning", "fail_func", "fail_test")
){

  type <- match.arg(type)

  # Create temp package
  temp_dir <- tempfile("pkg_temp_")
  pkg_dir <- file.path(temp_dir, pkg_name)
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
  desc_file <- get_pkg_desc(pkg_dir, fields = c("Package", "Version"))
  pkg_name_ver <- paste0(desc_file, collapse = "_")
  results_dir <- file.path(temp_dir, "results", pkg_name_ver)
  fs::dir_create(results_dir)


  # Build the package tarball
  tar_file <- devtools::build(pkg_dir)

  return(
    list(
      temp_dir = temp_dir,
      pkg_dir = pkg_dir,
      tar_file = tar_file,
      results_dir = results_dir
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
