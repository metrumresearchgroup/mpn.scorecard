# These are slimmed down variants of some tests from test-format-report.R,
# focused on checking if the formatting functions are compatible with the
# objects read in from external results.

results_dir <- local_create_external_results()

describe("formatting functions with external scores", {
  it("format_scores_for_render", {
    pkg_scores <- build_pkg_scores(results_dir)
    formatted <- format_scores_for_render(pkg_scores)[["formatted"]]

    overall_scores <- formatted[["overall_scores"]]
    expect_identical(
      names(overall_scores),
      c("category", "category_score", "risk")
    )
    expect_identical(
      overall_scores[["category"]],
      c(METRIC_CATEGORIES, "overall")
    )

    expect_identical(
      purrr::map(formatted[["category_scores"]], "criteria"),
      list(
        testing = TESTING_METRICS,
        documentation = c("has_website", "has_news"),
        maintenance = MAINTENANCE_METRICS,
        transparency = TRANSPARENCY_METRICS
      )
    )
  })

  it("format_testing_scores", {
    pkg_scores <- build_pkg_scores(results_dir)
    flex_df <- format_testing_scores(format_scores_for_render(pkg_scores))
    res <- flex_df[["body"]][["dataset"]]
    expect_identical(res[["Result"]], c("Passing", "91.54%"))
  })

  it("format_traceability_matrix", {
    pkg_scores <- build_pkg_scores(results_dir)
    tmat <- read_traceability_matrix(results_dir)
    formatted <- format_traceability_matrix(tmat,
      scorecard_type = pkg_scores[["scorecard_type"]]
    )
    res <- formatted[["body"]][["dataset"]]
    expect_identical(
      names(res),
      c("Command", "Code File", "Documentation", "Test Files")
    )
    expect_identical(res[["Command"]], c("foo bar", "foo baz"))
  })

  it("format_appendix", {
    input <- list(
      check_output = read_check_output(results_dir),
      cov_results_df = read_coverage_results(results_dir)
    )
    formatted <- format_appendix(input, return_vals = TRUE)

    cov_res <- formatted[["cov_results_flex"]][["body"]][["dataset"]]
    expect_identical(names(cov_res), c("Code File", "Test Coverage"))

    expect_identical(
      formatted[["check_output"]],
      "check\noutput\n"
    )
  })
})
