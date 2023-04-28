

describe("covr and rcmdcheck success", {

  it("no rcmdcheck warnings", {
    # Create temp package that will succeed
    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$pkg_result_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$pkg_result_dir)} passed"),
      fixed = TRUE
    )
    res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$pkg_result_dir)


    # confirm success
    expect_equal(res_check, 1)
    expect_equal(res_covr, 1)

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "check.rds"))
    expect_equal(check_output$status, 0)
    expect_true(rlang::is_empty(check_output$errors))
    expect_true(rlang::is_empty(check_output$warnings))
    expect_true(rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "covr.rds"))
    expect_true(is.na(covr_output$errors))
    expect_equal(covr_output$coverage$totalcoverage, 100)
  })

  it("with warnings", {
    # Create temp package that will succeed with warnings
    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_warning")

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$pkg_result_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$pkg_result_dir)} passed with warnings and/or notes"),
      fixed = TRUE
    )
    res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$pkg_result_dir)

    # confirm success and values - rcmdcheck has warnings
    expect_equal(res_check, 0.5)
    expect_equal(res_covr, 1)

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "check.rds"))
    expect_equal(check_output$status, 0)
    expect_true(rlang::is_empty(check_output$errors))
    expect_true(!rlang::is_empty(check_output$warnings))
    expect_true(rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "covr.rds"))
    expect_true(is.na(covr_output$errors))
    expect_equal(covr_output$coverage$totalcoverage, 100)
  })

  it("no test directory included in package", {
    # Create temp package that will succeed due to no test suite
    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_no_test_suite")

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$pkg_result_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$pkg_result_dir)} passed")
    )

    res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$pkg_result_dir)

    # confirm success and values - zero coverage
    expect_equal(res_check, 1)
    expect_equal(res_covr, 0)

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "check.rds"))
    expect_equal(check_output$status, 0)
    expect_true(rlang::is_empty(check_output$errors))
    expect_true(rlang::is_empty(check_output$warnings))
    expect_true(rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "covr.rds"))
    expect_true(is.na(covr_output$errors))
    expect_equal(covr_output$coverage$totalcoverage, 0) # technically tested above as well
  })

  it("no tests included in test file", {
    # Create temp package that will succeed
    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_no_test")

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$pkg_result_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$pkg_result_dir)} passed")
    )
    res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$pkg_result_dir)

    # confirm success and values - zero coverage
    expect_equal(res_check, 1)
    expect_equal(res_covr, 0)

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "check.rds"))
    expect_equal(check_output$status, 0)
    expect_true(rlang::is_empty(check_output$errors))
    expect_true(rlang::is_empty(check_output$warnings))
    expect_true(rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "covr.rds"))
    expect_true(is.na(covr_output$errors))
    expect_equal(covr_output$coverage$totalcoverage, 0) # technically tested above as well
  })

  it("no functions in R/", {
    # Create temp package that will succeed
    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_no_functions")

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$pkg_result_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$pkg_result_dir)} passed")
    )
    expect_message(
      res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$pkg_result_dir),
      glue::glue("R coverage for {basename(pkg_setup$pkg_result_dir)} had notes: No testable functions found")
    )

    # confirm success and values - zero coverage
    expect_equal(res_check, 1)
    expect_equal(res_covr, 0)

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "check.rds"))
    expect_equal(check_output$status, 0)
    expect_true(rlang::is_empty(check_output$errors))
    expect_true(rlang::is_empty(check_output$warnings))
    expect_true(rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "covr.rds"))
    expect_true(is.na(covr_output$errors))
    expect_equal(covr_output$notes, "No testable functions found")
    expect_equal(covr_output$coverage$totalcoverage, 0) # technically tested above as well
  })

  it("success with notes - rcmdcheck math and messages only", {
    skip_if_render_pdf() # The behavior changes when run via R CMD CHECK
    # Create temp package that will succeed
    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_notes")

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$pkg_result_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$pkg_result_dir)} passed with warnings and/or notes")
    )

    # confirm success and value
    expect_equal(res_check, 0.9)

  })

})




describe("covr and rcmdcheck failures", {

  it("failing tests", {
    # Create temp package that will fail
    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "fail_test")

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$pkg_result_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$pkg_result_dir)} failed")
    )
    expect_message(
      res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$pkg_result_dir),
      glue::glue("R coverage for {basename(pkg_setup$pkg_result_dir)} failed")
    )

    # confirm failure
    expect_equal(res_check, 0)
    expect_true(is.na(res_covr))

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "check.rds"))
    expect_equal(check_output$status, 1)
    expect_true(!rlang::is_empty(check_output$errors))
    expect_true(rlang::is_empty(check_output$warnings))
    expect_true(!rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "covr.rds"))
    expect_true(!is.na(covr_output$errors))
    expect_true(is.na(covr_output$coverage$totalcoverage)) # technically tested above as well
  })


  it("bad functions - failure before tests are run", {
    # Create temp package that will fail
    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "fail_func_syntax")

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_setup$pkg_dir)

    # Run check and coverage - expect message
    rcmdcheck_args$path <- pkg_setup$tar_file
    expect_message(
      res_check <- add_rcmdcheck(pkg_setup$pkg_result_dir, rcmdcheck_args),
      glue::glue("rcmdcheck for {basename(pkg_setup$pkg_result_dir)} failed")
    )
    expect_message(
      res_covr <- add_coverage(pkg_setup$pkg_dir, pkg_setup$pkg_result_dir),
      glue::glue("R coverage for {basename(pkg_setup$pkg_result_dir)} failed")
    )

    # confirm failure
    expect_equal(res_check, 0)
    expect_true(is.na(res_covr))

    # check rcmdcheck output
    check_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "check.rds"))
    expect_equal(check_output$status, 1)
    expect_true(!rlang::is_empty(check_output$errors))
    expect_true(rlang::is_empty(check_output$warnings))
    expect_true(rlang::is_empty(check_output$test_fail))

    # check covr output
    covr_output <- readRDS(get_result_path(pkg_setup$pkg_result_dir, "covr.rds"))
    expect_true(!is.na(covr_output$errors$message))
    expect_true(is.na(covr_output$coverage$totalcoverage)) # technically tested above as well
  })

})

