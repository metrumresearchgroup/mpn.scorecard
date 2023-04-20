
describe("summarize_pacakge_results", {

  it("default behavior", {
    # should probably do this in the setup script so we can use it across multiple tests, but then im not sure how/when to unlink the directory
    pkg_score_paths <- setup_multiple_pkg_scores()
    on.exit(cleanup_temp_dir(pkg_score_paths$testing_dir))

    # For inspecting
    # rstudioapi::filesPaneNavigate(pkg_score_paths$testing_dir)

    result_dirs <- pkg_score_paths$result_dirs

    pkg_results <- summarize_package_results(result_dirs)

    expect_equal(dim(pkg_results), c(5, 11))

    # Make sure package names are parsed correctly - integration test
    pkg_names_expect <- paste0("package", seq(1:5))
    expect_equal(pkg_results$package, pkg_names_expect)

    # TODO: revisit this test - most of this can/should be tested via other functions called by this one
    # dont know how much we really want to test here

  })

})
