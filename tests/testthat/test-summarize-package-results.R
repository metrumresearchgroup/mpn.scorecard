
describe("summarize_package_results", {

  it("default behavior", {
    # `result_dirs_select` and `pkg_select` defined in tests/testthat/setup.R

    # summarize package results - join pkg_type for potential inspection
    pkg_results <- summarize_package_results(result_dirs_select) %>%
      left_join(pkg_select %>% dplyr::select(pkg_name, pkg_type), by = c("package" = "pkg_name")) %>%
      relocate(c("package", "pkg_type"))

    expect_equal(dim(pkg_results), c(length(result_dirs_select), 12))

    # Make sure package names are parsed correctly - integration test
    expect_equal(pkg_results$package, pkg_select$pkg_name)

    # confirm no comments file for any packages, and that the behavior is different from `build_risk_summary`
    expect_true(all(pkg_results$has_comments == "no"))

    # spot check additional expected column outputs
    cases <- names(result_dirs_select)
    order_cases <- function(x) unname(x[cases])

    expect_equal(
      pkg_results$check_status,
      order_cases(c(
        pass_success = 0, pass_no_docs = 0, pass_no_functions = 0,
        fail_func_syntax = 1, fail_test = 1
      ))
    )
    expect_equal(
      pkg_results$covr_success,
      order_cases(c(
        pass_success = TRUE, pass_no_docs = TRUE, pass_no_functions = TRUE,
        fail_func_syntax = FALSE, fail_test = FALSE
      ))
    )
    expect_equal(
      pkg_results$overall_score,
      order_cases(c(
        pass_success = 0.5, pass_no_docs = 0.45, pass_no_functions = 0.3,
        fail_func_syntax = 0.1, fail_test = 0.1
      ))
    )
  })

  it("with comments for some packages", {

    comments_template <- system.file("test-data", "comments-example.txt", package = "mpn.scorecard")

    # Add comments to non-high-risk package
    pass_idx <- match("pass_success", names(result_dirs_select))
    comments_file <- fs::file_copy(comments_template, result_dirs_select[[pass_idx]])
    new_comments_name <- get_result_path(result_dirs_select[[pass_idx]], "comments.txt")
    fs::file_move(comments_file, new_comments_name)
    on.exit(fs::file_delete(new_comments_name), add = TRUE)

    # Add comments to high-risk package
    fail_idx <- match("fail_func_syntax", names(result_dirs_select))
    comments_file <- fs::file_copy(comments_template, result_dirs_select[[fail_idx]])
    new_comments_name_high <- get_result_path(result_dirs_select[[fail_idx]], "comments.txt")
    fs::file_move(comments_file, new_comments_name_high)
    on.exit(fs::file_delete(new_comments_name_high), add = TRUE)

    pkg_results <- summarize_package_results(result_dirs_select)

    expect_identical(pkg_results$has_comments[pass_idx], "yes")
    expect_identical(pkg_results$has_comments[fail_idx], "yes")
    expect_identical(
      unique(pkg_results$has_comments[-c(pass_idx, fail_idx)]),
      "no"
    )
  })

  it("aborts on external scores", {
    rdir <- local_create_external_results()
    expect_error(render_scorecard_summary(rdir), "not supported")
  })
})
