
describe("score_pkg", {

  it("score_pkg overall behavior - successful package", {

    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    pkg_tar <- pkg_setup %>% pull(tar_file)

    expected_results_dir <- pkg_setup %>% pull(pkg_result_dir)

    # Check messages and final result paths
    expect_message(
      result_dir <- score_pkg(pkg_tar, pkg_dirs$all_results_dir, overwrite = TRUE),
      glue::glue("rcmdcheck for {basename(expected_results_dir)} passed"),
    )
    expect_equal(result_dir, expected_results_dir)

    # Check for all results
    expected_result_paths <- purrr::map_chr(c("check.rds", "covr.rds", "scorecard.json"), ~{
      get_result_path(expected_results_dir, .x)
    })
    expect_equal(fs::dir_ls(result_dir), expected_result_paths)

    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # Check json attributes
    expect_equal(
      names(pkg_scores),
      c("pkg_name", "pkg_version", "out_dir", "pkg_tar_path", "md5sum_check", "scores", "metadata", "category_scores")
    )
  })


  it("score_pkg overall behavior - failed package", {

    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "fail_test")
    pkg_tar <- pkg_setup %>% pull(tar_file)

    expected_results_dir <- pkg_setup %>% pull(pkg_result_dir)

    # Check messages and final result paths
    expect_message(
      result_dir <- score_pkg(pkg_tar, pkg_dirs$all_results_dir, overwrite = TRUE),
      glue::glue("rcmdcheck for {basename(expected_results_dir)} failed"),
    )
    expect_equal(result_dir, expected_results_dir)

    # Check for all results
    expected_result_paths <- purrr::map_chr(c("check.rds", "covr.rds", "scorecard.json"), ~{
      get_result_path(expected_results_dir, .x)
    })
    expect_equal(fs::dir_ls(result_dir), expected_result_paths)

    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # Check json attributes
    expect_equal(
      names(pkg_scores),
      c("pkg_name", "pkg_version", "out_dir", "pkg_tar_path", "md5sum_check", "scores", "metadata", "category_scores")
    )

    expect_equal(pkg_scores$scores$testing$check, 0)
    expect_equal(pkg_scores$scores$testing$covr, "NA") # NA character when read from json
    expect_equal(pkg_scores$category_scores$testing, 0) # confirm overall category score is still a number

  })

  it("create_score_list_from_riskmetric", {

    pkg_setup <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    pkg_tar <- pkg_setup %>% pull(tar_file)

    res <- list(
      pkg_name = "test",
      pkg_version = "0.0.0.9000",
      out_dir = pkg_setup$pkg_result_dir,
      pkg_tar_path = pkg_tar,
      md5sum_check = tools::md5sum(pkg_tar),
      # for results
      scores = list(
        testing = list(), # not updated here
        documentation = list(),
        maintenance = list(),
        transparency = list()
      )
    )

    # Test expected results and format (unchanged)
    # Dont need to test that scores are accurate - done in riskmetric package
    # This is an integration test of create_score_list_from_riskmetric and other nested functions,
    # confirming that the values are still in the expected format
    res <- create_score_list_from_riskmetric(res, pkg_setup$pkg_dir)
    purrr::imap(res$scores, ~{
      if(.y == "testing"){
        return(NULL)
      }else{
        purrr::imap(.x, function(attr.x, attr.y){
          if(attr.y == "has_maintainer"){
            expect_true(attr.x == 1)
          }else{
            expect_true(attr.x == 0)
          }
        })
      }
    })

  })

})



