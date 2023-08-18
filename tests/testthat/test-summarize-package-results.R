
describe("summarize_package_results", {

  it("default behavior", {
    # `result_dirs_select` and `pkg_select` defined in tests/testthat/setup.R

    # summarize package results - join pkg_type for potential inspection
    pkg_results <- summarize_package_results(result_dirs_select) %>%
      left_join(pkg_select %>% select(pkg_name, pkg_type), by = c("package" = "pkg_name")) %>%
      relocate(c("package", "pkg_type"))

    expect_equal(dim(pkg_results), c(length(result_dirs_select), 12))

    # Make sure package names are parsed correctly - integration test
    expect_equal(pkg_results$package, pkg_select$pkg_name)

    # confirm no mitigation file for any packages, and that the behavior is different from `build_risk_summary`
    expect_true(all(pkg_results$has_mitigation == "no"))

    # spot check additional expected column outputs
    cases <- names(result_dirs_select)
    order_cases <- function(x) unname(x[cases])

    expect_equal(
      pkg_results$check_status,
      order_cases(c(
        pass_success = 0, pass_warning = 0,
        fail_func_syntax = 1, fail_test = 1
      ))
    )
    expect_equal(
      pkg_results$covr_success,
      order_cases(c(
        pass_success = TRUE, pass_warning = TRUE,
        fail_func_syntax = FALSE, fail_test = FALSE
      ))
    )
    expect_equal(
      pkg_results$overall_score,
      order_cases(c(
        pass_success = 0.5, pass_warning = 0.4,
        fail_func_syntax = 0.1, fail_test = 0.1
      ))
    )
  })

  it("with mitigation for some packages", {

    mitigation_template <- system.file("test-data", "mitigation-example.txt", package = "mpn.scorecard")

    # Add mitigation to non-high-risk package
    pass_idx <- match("pass_success", names(result_dirs_select))
    mitigation_file <- fs::file_copy(mitigation_template, result_dirs_select[[pass_idx]])
    new_mitigation_name <- get_result_path(result_dirs_select[[pass_idx]], "mitigation.txt")
    fs::file_move(mitigation_file, new_mitigation_name)
    on.exit(fs::file_delete(new_mitigation_name), add = TRUE)

    # Add mitigation to high-risk package
    fail_idx <- match("fail_func_syntax", names(result_dirs_select))
    mitigation_file <- fs::file_copy(mitigation_template, result_dirs_select[[fail_idx]])
    new_mitigation_name_high <- get_result_path(result_dirs_select[[fail_idx]], "mitigation.txt")
    fs::file_move(mitigation_file, new_mitigation_name_high)
    on.exit(fs::file_delete(new_mitigation_name_high), add = TRUE)

    pkg_results <- summarize_package_results(result_dirs_select)

    expect_identical(pkg_results$has_mitigation[pass_idx], "yes")
    expect_identical(pkg_results$has_mitigation[fail_idx], "yes")
    expect_identical(
      unique(pkg_results$has_mitigation[-c(pass_idx, fail_idx)]),
      "no"
    )
  })

})
