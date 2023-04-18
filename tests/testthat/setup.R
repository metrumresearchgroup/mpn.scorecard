
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


create_failing_package <- function(type = c("fail_func", "fail_test")){

  type <- match.arg(type)

  # Create temp package
  temp_dir <- tempfile("pkg_temp_")
  pkg_dir <- file.path(temp_dir, "mypackage")
  fs::dir_create(pkg_dir)
  devtools::create(pkg_dir)

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
    library(mypackage)

    test_check('mypackage')
    ")
  writeLines(test_setup, testthat_script)

  # Add a test file that will fail
  test_file <- file.path(test_dir, "test-myscript.R")
  fs::file_create(test_file)
  # Test will fail regardless
  writeLines("testthat::test_that('this fails', { myfunction('blah') })", test_file)

  # Build the package tarball
  tar_file <- devtools::build(pkg_dir)

  return(
    list(
      temp_dir = temp_dir,
      pkg_dir = pkg_dir,
      tar_file = tar_file
    )
  )
}



