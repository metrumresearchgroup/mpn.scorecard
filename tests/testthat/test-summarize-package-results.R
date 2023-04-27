
describe("summarize_pacakge_results", {

  it("default behavior", {

    result_dirs <- pkg_dirs$pkg_setups_df$pkg_result_dir

    pkg_results <- summarize_package_results(result_dirs)

    expect_equal(dim(pkg_results), c(7, 11))

    # Make sure package names are parsed correctly - integration test
    pkg_names_expect <- paste0("package", seq(1:7))
    expect_equal(pkg_results$package, pkg_names_expect)

    # confirm no mitigation file for any packages, and that the behavior is different from `build_risk_summary`
    expect_true(all(pkg_results$has_mitigation == "no"))

    # spot check additional expected column outputs
    expect_equal(pkg_results$check_status, c(rep(0, 5), rep(1, 2)))
    expect_equal(pkg_results$covr_success, c(rep(TRUE, 5), rep(FALSE, 2)))

    # TODO: revisit this test - most of this can/should be tested via other functions called by this one
    # dont know how much we really want to test here

  })

  it("with mitigation for some packages", {

    result_dirs <- pkg_dirs$pkg_setups_df$pkg_result_dir

    # Add mitigation to non-high-risk package
    mitigation_file <- fs::file_copy(mitigation_template, result_dirs[1])
    new_mitigation_name <- get_result_path(result_dirs[1], "mitigation.txt")
    fs::file_move(mitigation_file, new_mitigation_name)
    on.exit(fs::file_delete(new_mitigation_name), add = TRUE)

    # Add mitigation to high-risk package
    mitigation_file <- fs::file_copy(mitigation_template, result_dirs[7])
    new_mitigation_name_high <- get_result_path(result_dirs[7], "mitigation.txt")
    fs::file_move(mitigation_file, new_mitigation_name_high)
    on.exit(fs::file_delete(new_mitigation_name_high), add = TRUE)

    pkg_results <- summarize_package_results(result_dirs)

    expect_true(pkg_results$has_mitigation[1] == "yes")
    expect_true(pkg_results$has_mitigation[7] == "yes")

  })

})
