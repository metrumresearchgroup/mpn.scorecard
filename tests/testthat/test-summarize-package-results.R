
describe("summarize_pacakge_results", {

  it("default behavior", {

    result_dirs <- pkg_dirs$pkg_setups_df$pkg_result_dir

    pkg_results <- summarize_package_results(result_dirs)

    expect_equal(dim(pkg_results), c(7, 11))

    # Make sure package names are parsed correctly - integration test
    pkg_names_expect <- paste0("package", seq(1:7))
    expect_equal(pkg_results$package, pkg_names_expect)

    # TODO: revisit this test - most of this can/should be tested via other functions called by this one
    # dont know how much we really want to test here

  })

})
