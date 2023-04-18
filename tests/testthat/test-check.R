testthat::test_that('rcmdcheck works correctly on success', {

  pkg_setup <- setup_files(pkg_tar)
  on.exit(unlink(dirname(pkg_setup$pkg_source_path), recursive = TRUE), add = TRUE)

  out_dir_pkg <- file.path(out_dir, pkg_setup$pkg_name_ver)

  # Set path and run check
  rcmdcheck_args$path <- pkg_tar
  res_check <- add_rcmdcheck(out_dir_pkg, rcmdcheck_args)

  # confirm success
  expect_equal(res_check, 1)

  # check outputs
  check_output <- readRDS(get_result_path(out_dir_pkg, "check.rds"))
  expect_equal(check_output$status, 0)
  expect_true(rlang::is_empty(check_output$errors))
  expect_true(rlang::is_empty(check_output$warnings))
  expect_true(rlang::is_empty(check_output$test_fail))
})

testthat::test_that('rcmdcheck works correctly on failure', {

  # Create temp package that will fail
  pkg_setup <- create_failing_package()
  on.exit(unlink(pkg_setup$temp_dir))

  # Set path and run check
  rcmdcheck_args$path <- pkg_setup$tar_file
  res_check <- add_rcmdcheck(pkg_setup$temp_dir, rcmdcheck_args)

  # confirm failure
  expect_equal(res_check, 0)

  # check outputs
  check_output <- readRDS(get_result_path(pkg_setup$temp_dir, "check.rds"))
  expect_equal(check_output$status, 1)
  expect_true(!rlang::is_empty(check_output$errors))
})
