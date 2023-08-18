
describe("creating extra notes", {

  it("create_extra_notes - success integration test", {

    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file
    extra_notes_data <- create_extra_notes(result_dir_x, pkg_tar_path = pkg_tar_x)

    # Confirm values - covr
    expect_equal(unique(extra_notes_data$covr_results_df$test_coverage), 100)
    expect_equal(unique(extra_notes_data$covr_results_df$r_script), "R/myscript.R")
    # Confirm values - R CMD Check
    expect_true(grepl("Status: OK", extra_notes_data$check_output))

  })

  it("create_extra_notes - failure integration test", {
    # Bad package - no documentation (at all)
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "fail_func_syntax")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file
    extra_notes_data <- create_extra_notes(result_dir_x, pkg_tar_path = pkg_tar_x)

    # Confirm values - covr
    expect_true(grepl("cannot open", unique(extra_notes_data$covr_results_df$test_coverage)))
    expect_identical(extra_notes_data$covr_results_df$r_script, "File coverage failed")
    # Confirm values - R CMD Check
    expect_true(grepl("ERROR", extra_notes_data$check_output))
  })

  it("create_extra_notes - no coverage results", {
    setups <- pkg_dirs$pkg_setups_df
    case <- setups$pkg_type == "pass_no_functions"

    result_dir <- setups$pkg_result_dir[case]
    pkg_tar <- setups$tar_file[case]

    res <- create_extra_notes(result_dir, pkg_tar)
    expect_identical(
      res$covr_results_df$r_script,
      "No coverage results"
    )
    expect_identical(
      res$covr_results_df$test_coverage,
      "No testable functions found"
    )
  })
})
