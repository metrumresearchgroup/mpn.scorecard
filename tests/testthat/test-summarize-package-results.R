
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
    expect_equal(pkg_results$check_status, c(rep(0, 2), rep(1, 2)))
    expect_equal(pkg_results$covr_success, c(rep(TRUE, 2), rep(FALSE, 2)))
    expect_equal(pkg_results$overall_score, c(0.5, 0.4, 0.1, 0.1))

  })

  it("with mitigation for some packages", {

    mitigation_template <- system.file("test-data", "mitigation-example.txt", package = "mpn.scorecard")

    # Add mitigation to non-high-risk package
    mitigation_file <- fs::file_copy(mitigation_template, result_dirs_select[1])
    new_mitigation_name <- get_result_path(result_dirs_select[1], "mitigation.txt")
    fs::file_move(mitigation_file, new_mitigation_name)
    on.exit(fs::file_delete(new_mitigation_name), add = TRUE)

    # Add mitigation to high-risk package
    mitigation_file <- fs::file_copy(mitigation_template, result_dirs_select[3])
    new_mitigation_name_high <- get_result_path(result_dirs_select[3], "mitigation.txt")
    fs::file_move(mitigation_file, new_mitigation_name_high)
    on.exit(fs::file_delete(new_mitigation_name_high), add = TRUE)

    pkg_results <- summarize_package_results(result_dirs_select)

    expect_true(pkg_results$has_mitigation[1] == "yes")
    expect_true(pkg_results$has_mitigation[2] == "no")
    expect_true(pkg_results$has_mitigation[3] == "yes")

  })

})
