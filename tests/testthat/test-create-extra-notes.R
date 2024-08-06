
describe("creating extra notes", {

  it("create_extra_notes - success integration test", {

    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    extra_notes_data <- create_extra_notes(result_dir_x)

    # Confirm values - covr
    expect_equal(unique(extra_notes_data$covr_results_df$test_coverage), 100)
    expect_equal(unique(extra_notes_data$covr_results_df$code_file), "R/myscript.R")
    # Confirm values - R CMD Check
    expect_true(grepl("Status: OK", extra_notes_data$check_output))

  })

  it("create_extra_notes - failure integration test", {
    # Bad package - no documentation (at all)
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "fail_func_syntax")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    extra_notes_data <- create_extra_notes(result_dir_x)

    # Confirm values - covr
    expect_true(grepl("cannot open", unique(extra_notes_data$covr_results_df$test_coverage)))
    expect_identical(extra_notes_data$covr_results_df$code_file, "File coverage failed")
    # Confirm values - R CMD Check
    expect_true(grepl("ERROR", extra_notes_data$check_output))
  })

  it("create_extra_notes - no coverage results", {
    setups <- pkg_dirs$pkg_setups_df
    case <- setups$pkg_type == "pass_no_functions"

    result_dir <- setups$pkg_result_dir[case]
    res <- create_extra_notes(result_dir)
    expect_identical(
      res$covr_results_df$code_file,
      "No coverage results"
    )
    expect_identical(
      res$covr_results_df$test_coverage,
      "no testable functions found"
    )
  })
})
