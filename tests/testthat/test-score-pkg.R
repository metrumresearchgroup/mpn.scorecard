
describe("score_pkg", {

  it("score_pkg overall behavior - successful package", {
    local_check_envvar()
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
      c("mpn_scorecard_version","pkg_name", "pkg_version", "out_dir",
        "pkg_tar_path", "md5sum_check", "scores", "metadata", "category_scores")
    )

    # These tests also serve to confirm the correct environment vars in `local_check_envvar` were set
    # Check category scores
    expect_equal(pkg_scores$category_scores$testing, 1)
    expect_equal(pkg_scores$category_scores$documentation, 0)
    expect_equal(pkg_scores$category_scores$maintenance, 0.5)
    expect_equal(pkg_scores$category_scores$transparency, 0)
    expect_equal(pkg_scores$category_scores$overall, 0.5)

    # check individual scores
    expect_equal(pkg_scores$scores$testing$check, 1)
    expect_equal(pkg_scores$scores$testing$covr, 1)

  })


  it("score_pkg overall behavior - failed package", {
    local_check_envvar()
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
      c("mpn_scorecard_version", "pkg_name", "pkg_version", "out_dir",
        "pkg_tar_path", "md5sum_check", "scores", "metadata", "category_scores")
    )

    expect_equal(pkg_scores$scores$testing$check, 0)
    expect_equal(pkg_scores$scores$testing$covr, "NA") # NA character when read from json
    expect_equal(pkg_scores$category_scores$testing, 0) # confirm overall category score is still a number

  })

  it("supports covr timeout", {
    tdir <- withr::local_tempdir("mpn-scorecard-tests-")
    local_check_envvar()

    setups <- pkg_dirs$pkg_setups_df
    tar_orig <- setups$tar_file[setups$pkg_type == "pass_success"]

    checkmate::assert_string(tar_orig)
    pkg_tar <- file.path(tdir, basename(tar_orig))
    fs::file_copy(tar_orig, pkg_tar)

    result_dir <- score_pkg(pkg_tar, tdir, covr_timeout = 0.1)

    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    expect_equal(pkg_scores$scores$testing$check, 1)
    expect_identical(pkg_scores$scores$testing$covr, "NA")

    covr_res <- readRDS(get_result_path(result_dir, "covr.rds"))
    expect_s3_class(covr_res$errors, "callr_timeout_error")
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
    purrr::iwalk(res$scores, ~{
      if(.y == "testing"){
        return(NULL)
      }else{
        purrr::iwalk(.x, function(attr.x, attr.y){
          if(attr.y == "has_maintainer"){
            expect_true(attr.x == 1)
          }else{
            expect_true(attr.x == 0)
          }
        })
      }
    })

  })

  it("calc_overall_scores summarizes scores correctly", {
    result_dir <- result_dirs_select[["pass_success"]]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # Remove overall scores
    pkg_scores$category_scores <- NULL

    # Normal behavior
    res <- calc_overall_scores(pkg_scores)
    expect_equal(res$category_scores$testing, 1)
    expect_equal(res$category_scores$documentation, 0)
    expect_equal(res$category_scores$maintenance, 0.5)
    expect_equal(res$category_scores$transparency, 0)
    expect_equal(res$category_scores$overall, 0.5)

    # With failed coverage
    pkg_scores$scores$testing$covr <- NA
    res <- calc_overall_scores(pkg_scores)
    expect_equal(res$category_scores$testing, 0.5)
    expect_equal(res$category_scores$overall, 0.3)
  })

  it("error out for NA and non-numeric category scores", {
    result_dir <- result_dirs_select[["pass_success"]]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # Manually set category scores to problematic values (NA and non-numeric)
    pkg_scores$category_scores$documentation <- "NA"
    pkg_scores$category_scores$maintenance <- "error"
    pkg_scores$category_scores$transparency <- list(error="error")

    # Test that all category scores show up in error message
    error_msgs <- tryCatch(
      check_scores_valid(pkg_scores, json_path),
      error = function(cond) cond
    )
    expect_true(grepl("documentation", error_msgs$message))
    expect_true(grepl("maintenance", error_msgs$message))
    expect_true(grepl("transparency", error_msgs$message))
  })

  it("error out if any individual scores are missing", {
    result_dir <- result_dirs_select[["pass_success"]]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    pkg_scores$scores$documentation$has_vignettes <- NULL
    pkg_scores$scores$maintenance$news_current <- NULL

    error_msgs <- tryCatch(
      check_scores_valid(pkg_scores, json_path),
      error = function(cond) cond
    )
    expect_true(grepl("has_vignettes", error_msgs$message))
    expect_true(grepl("news_current", error_msgs$message))
  })

})

