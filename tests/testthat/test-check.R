
testthat::test_that('rcmdcheck works correctly on success - rcmdcheck success', {

  # Create temp package that will fail
  pkg_setup <- create_testing_package(type = "pass_success")
  # on.exit(cleanup_temp_dir(pkg_setup$temp_dir))
  rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

  # Set path and run check
  rcmdcheck_args$path <- pkg_setup$tar_file
  res_check <- add_rcmdcheck(pkg_setup$results_dir, rcmdcheck_args)


  # confirm success
  expect_equal(res_check, 1)

  # check outputs
  check_output <- readRDS(get_result_path(pkg_setup$results_dir, "check.rds"))
  expect_equal(check_output$status, 0)
  expect_true(rlang::is_empty(check_output$errors))
  expect_true(rlang::is_empty(check_output$warnings))
  expect_true(rlang::is_empty(check_output$test_fail))

  cleanup_temp_dir(dirname(pkg_setup$temp_dir))
})


testthat::test_that('rcmdcheck works correctly on success - rcmdcheck warning', {

  # Create temp package that will fail
  pkg_setup <- create_testing_package(type = "pass_warning")
  on.exit(unlink(pkg_setup$temp_dir))

  # Set path and run check
  rcmdcheck_args$path <- pkg_setup$tar_file
  res_check <- add_rcmdcheck(pkg_setup$temp_dir, rcmdcheck_args)

  # confirm success
  expect_equal(res_check, 0.5)

  # check outputs
  check_output <- readRDS(get_result_path(pkg_setup$temp_dir, "check.rds"))
  expect_equal(check_output$status, 0)
  expect_true(rlang::is_empty(check_output$errors))
  expect_true(rlang::is_empty(check_output$warnings))
  expect_true(rlang::is_empty(check_output$test_fail))
})


testthat::test_that('rcmdcheck works correctly on failure', {

  # Create temp package that will fail
  pkg_setup <- create_testing_package(type = "fail_test")
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
