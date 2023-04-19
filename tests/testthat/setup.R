
library(usethis)

### This will have to be changes, but structuring tests around this for now ###
package_dir <- file.path("/data", "Projects", "package_dev")


pkg_tar <- file.path(package_dir,"review_3.1.0.tar.gz") # tarball
out_dir <- file.path(tempdir(), "results")


rcmdcheck_args = list(
  timeout = Inf,
  args = "--no-manual",
  quiet = TRUE
)


setup_files <- function(pkg_tar){
  # Unpack tarball
  pkg_source_path <- unpack_tarball(pkg_tar)

  # Get package name and version
  pkg_desc <- get_pkg_desc(pkg_source_path, fields = c("Package", "Version"))
  pkg_name <- pkg_desc$Package
  pkg_ver <- pkg_desc$Version
  pkg_name_ver <- paste0(pkg_name, "_", pkg_ver)

  return(
    list(
      pkg_source_path = pkg_source_path,
      pkg_name = pkg_name,
      pkg_ver = pkg_ver,
      pkg_name_ver = pkg_name_ver
    )
  )
}


# create_failing_package <- function(
    #     pkg_name = "mypackage",
#     type = c("fail_func", "fail_test")
# ){
#
#   type <- match.arg(type)
#
#   # Create temp package
#   temp_dir <- tempfile("pkg_temp_")
#   pkg_dir <- file.path(temp_dir, pkg_name)
#   fs::dir_create(pkg_dir)
#   devtools::create(pkg_dir)
#
#   # Add a file with a syntax error to the package
#   script_file <- file.path(pkg_dir, "R", "myscript.R")
#   fs::file_create(script_file)
#   if(type == "fail_func"){
#     writeLines("myfunction <- function(x { x + 1", script_file)
#   }else{
#     writeLines("myfunction <- function(x) { x + 1}", script_file)
#   }
#
#
#   # Add a test setup for running test suite
#   test_dir <- file.path(pkg_dir, "tests", "testthat")
#   testthat_script <- file.path(dirname(test_dir), "testthat.R")
#   fs::dir_create(test_dir, recurse = TRUE)
#   fs::file_create(testthat_script)
#   test_setup <- glue::glue("
#     library(testthat)
#     library({pkg_name})
#
#     test_check('{pkg_name}')
#     ")
#   writeLines(test_setup, testthat_script)
#
#   # Add a test file that will fail
#   test_file <- file.path(test_dir, "test-myscript.R")
#   fs::file_create(test_file)
#   # Test will fail regardless
#   writeLines("testthat::test_that('this fails', { myfunction('blah') })", test_file)
#
#
#   pkg_desc_path <- file.path(pkg_dir, "DESCRIPTION")
#
#   # Create temp directory for saving results, that follows the convention of this package:
#   # namley, a directory with the basename of the package and version
#   desc_file <- read.dcf(pkg_desc_path, fields = c("Package", "Version"))[1L,]
#   pkg_name_ver <- paste0(desc_file, collapse = "_")
#   results_dir <- file.path(temp_dir, "results", pkg_name_ver)
#   fs::dir_create(results_dir)
#
#   # Build the package tarball
#   tar_file <- devtools::build(pkg_dir)
#
#   return(
#     list(
#       temp_dir = temp_dir,
#       pkg_dir = pkg_dir,
#       tar_file = tar_file,
#       results_dir = results_dir
#     )
#   )
# }


create_testing_package <- function(
    pkg_name = "mypackage",
    type = c("pass_success", "pass_warning", "fail_func", "fail_test")
){

  type <- match.arg(type)

  # Create temp package
  temp_dir <- tempfile("pkg_temp_")
  pkg_dir <- file.path(temp_dir, pkg_name)
  fs::dir_create(pkg_dir)
  # devtools::create(pkg_dir)
  create_package(pkg_dir, rstudio = FALSE, open = FALSE,
                 fields = list(
                   License = "MIT + file LICENSE",
                   `Authors@R` = 'person("Jane", "Doe", email = "jane@example.com",
                          role = c("aut", "cre"),
                          comment = c(ORCID = "YOUR-ORCID-ID"))'
                 )
  )

  # Add a file with a syntax error to the package
  script_file <- file.path(pkg_dir, "R", "myscript.R")
  fs::file_create(script_file)
  if(type == "fail_func"){
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

  # Add a test file that can fail
  test_file <- file.path(test_dir, "test-myscript.R")
  fs::file_create(test_file)
  if(type == "fail_test"){
    test_lines <- "testthat::test_that('this fails', { myfunction('blah') })"
  }else{
    test_lines <- "testthat::test_that('this works', { expect_equal(myfunction(1), 2)})"
  }
  writeLines(test_lines, test_file)


  pkg_desc_path <- file.path(pkg_dir, "DESCRIPTION")

  # If success - need to update description file and add license
  if(type == "pass_success"){
    # desc_file <- read.dcf(pkg_desc_path)[1L,]
    desc_file <- usethis::use_description_defaults(
      package = pkg_name,
      fields = list(
        License = "MIT + file LICENSE",
        `Authors@R` = 'person("Jane", "Doe", email = "jane@example.com",
                          role = c("aut", "cre"),
                          comment = c(ORCID = "YOUR-ORCID-ID"))'
      )
    )

    write.dcf(desc_file, pkg_desc_path)

    # Add license file to avoid warnings - the method can hopefully be improved (see notes)
    old_proj <- proj_get()
    withr::with_dir(pkg_dir, { # couldnt get `use_mit_license` to work this way - uses `proj_get` under the hood (currently doesnt do anything)
      # proj_set seemed required - couldnt find a workaround, but this is definitely not ideal and may have unintended consequences
      # (also probably wont work on drone - hunch)
      proj_set(path = pkg_dir, force = FALSE)
      use_mit_license(copyright_holder = NULL)
      use_package("testthat", type = "Suggests", min_version = NULL)
    })
    proj_set(path = old_proj, force = FALSE)
  }


  # Create temp directory for saving results, that follows the convention of this package:
  # namley, a directory with the basename of the package and version
  desc_file <- read.dcf(pkg_desc_path, fields = c("Package", "Version"))[1L,]
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
  tibble::tibble(file = names(exists), exists = unname(exists))
}
