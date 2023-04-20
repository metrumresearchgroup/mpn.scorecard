
describe("score_pkg", {

  it("default behavior", {

    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_name == "package1")
    pkg_tar <- pkg_setup %>% pull(tar_file)

    expected_results_dir <- pkg_setup %>% pull(result_dir)

    # Check messages and final result paths
    expect_message(
      result_dir <- score_pkg(pkg_tar, pkg_dirs$results_dir, overwrite = TRUE),
      glue::glue("rcmdcheck for {basename(expected_results_dir)} passed"),
    )
    expect_equal(result_dir, expected_results_dir)

    # Check for all results
    expected_result_paths <- purrr::map_chr(c("check.rds", "covr.rds", "scorecard.json"), ~{
      get_result_path(expected_results_dir, .x)
    })
    expect_equal(fs::dir_ls(results_dir), expected_result_paths)

    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # Check json attributes
    expect_equal(
      names(pkg_scores),
      c("pkg_name", "pkg_version", "out_dir", "pkg_tar_path", "md5sum_check", "scores", "metadata", "category_scores")
    )
  })

})
