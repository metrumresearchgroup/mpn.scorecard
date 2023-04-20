

describe("covr and rcmdcheck success", {

  it("no rcmdcheck warnings", {
    # Create temp package that will succeed
    pkg_setup <- create_testing_package(type = "pass_success")
    on.exit(cleanup_temp_dir(pkg_setup$testing_dir))

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$results_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$results_dir)} passed"),
      fixed = TRUE
    )
    res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$results_dir)


    # confirm success
    expect_equal(res_check, 1)
    expect_equal(res_covr, 1)

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$results_dir, "check.rds"))
    expect_equal(check_output$status, 0)
    expect_true(rlang::is_empty(check_output$errors))
    expect_true(rlang::is_empty(check_output$warnings))
    expect_true(rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$results_dir, "covr.rds"))
    expect_true(is.na(covr_output$errors))
    expect_equal(covr_output$coverage$totalcoverage, 100)
  })

  it("with warnings", {
    # Create temp package that will succeed with warnings
    pkg_setup <- create_testing_package(pkg_name = "packageWarn", type = "pass_warning")
    on.exit(cleanup_temp_dir(pkg_setup$testing_dir))

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$results_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$results_dir)} passed with warnings"),
      fixed = TRUE
    )
    res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$results_dir)

    # confirm success - rcmdcheck has warnings
    expect_equal(res_check, 0.5)
    expect_equal(res_covr, 1)

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$results_dir, "check.rds"))
    expect_equal(check_output$status, 0)
    expect_true(rlang::is_empty(check_output$errors))
    expect_true(!rlang::is_empty(check_output$warnings))
    expect_true(rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$results_dir, "covr.rds"))
    expect_true(is.na(covr_output$errors))
    expect_equal(covr_output$coverage$totalcoverage, 100)
  })

})




describe("covr and rcmdcheck failures", {

  it("failing tests", {
    # Create temp package that will fail
    pkg_setup <- create_testing_package(type = "fail_test")
    on.exit(cleanup_temp_dir(pkg_setup$testing_dir))

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$results_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$results_dir)} failed")
    )
    expect_message(
      res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$results_dir),
      glue::glue("R coverage for {basename(pkg_setup$results_dir)} failed")
    )

    # confirm failure
    expect_equal(res_check, 0)
    expect_true(is.na(res_covr))

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$results_dir, "check.rds"))
    expect_equal(check_output$status, 1)
    expect_true(!rlang::is_empty(check_output$errors))
    expect_true(rlang::is_empty(check_output$warnings))
    expect_true(!rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$results_dir, "covr.rds"))
    expect_true(!is.na(covr_output$errors))
    expect_true(is.na(covr_output$coverage$totalcoverage)) # technically tested above as well
  })


  it("bad functions - failure before tests are run", {
    # Create temp package that will fail
    pkg_setup <- create_testing_package(type = "fail_func")
    on.exit(cleanup_temp_dir(pkg_setup$testing_dir))

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$results_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$results_dir)} failed")
    )
    expect_message(
      res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$results_dir),
      glue::glue("R coverage for {basename(pkg_setup$results_dir)} failed")
    )

    # confirm failure
    expect_equal(res_check, 0)
    expect_true(is.na(res_covr))

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$results_dir, "check.rds"))
    expect_equal(check_output$status, 1)
    expect_true(!rlang::is_empty(check_output$errors))
    expect_true(rlang::is_empty(check_output$warnings))
    expect_true(rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$results_dir, "covr.rds"))
    expect_true(!is.na(covr_output$errors$message))
    expect_true(is.na(covr_output$coverage$totalcoverage)) # technically tested above as well
  })

})

