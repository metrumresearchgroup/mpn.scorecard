
describe("formatting functions", {

  it("format_scores_for_render", {

    for(result_dir.i in result_dirs_select){
      json_path <- get_result_path(result_dir.i, "scorecard.json")
      pkg_scores <- jsonlite::fromJSON(json_path)

      # map scores to risk and format into tables to be written to PDF
      formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))
      formatted_only <- formatted_pkg_scores$formatted


      # Combine all formatted scores to df
      scores_df <- purrr::imap(formatted_only$category_scores, ~ {
        .x %>% mutate(category = .y)
      }) %>% purrr::list_rbind()

      # ensure all scores are numerics
      expect_true(all(is.numeric(scores_df$score)))

      # ensure all results are 'Yes', 'No', 'Failed', or a percent
      expect_true(all(grepl("Yes|No|Failed|Passing|\\%", scores_df$result)))

      # ensure all risks are valid values
      expect_true(all(grepl(paste0(RISK_LEVELS, collapse = "|"), scores_df$risk)))

      # ensure all category criteria are present
      expected_criteria <- c(names(purrr::list_c(pkg_scores$scores)), "R CMD CHECK", "coverage")
      expected_criteria <- expected_criteria[!grepl("check|covr", expected_criteria)]
      expect_true(all(scores_df$criteria %in% expected_criteria))

      # Check overall category scores
      overall_scores_df <- formatted_pkg_scores$formatted$overall_scores
      expect_true(all(is.numeric(overall_scores_df$category_score)))
      expect_true(all(grepl(paste0(RISK_LEVELS, collapse = "|"), overall_scores_df$risk)))
      # No NAs for category scores
      expect_false(any(is.na(overall_scores_df$category_score)))
    }
  })

  it("format_overall_scores", {

    result_dir <- result_dirs_select[1]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # map scores to risk and format into tables to be written to PDF
    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))

    flex_df <- format_overall_scores(formatted_pkg_scores)
    expect_true(all(c("Category", "Category Score", "Risk") %in% names(flex_df$body$dataset)))
  })

  it("format_package_details", {

    result_dir <- result_dirs_select[1]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # map scores to risk and format into tables to be written to PDF
    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))

    flex_df <- format_package_details(formatted_pkg_scores)
    expect_true(all(c("Category","Criteria", "Result", "Risk") %in% names(flex_df$body$dataset)))
    expect_true(all(c("Criteria", "Result", "Risk") %in% flex_df$col_keys))
  })

  it("format_testing_scores", {

    result_dir <- result_dirs_select[1]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # map scores to risk and format into tables to be written to PDF
    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))

    flex_df <- format_testing_scores(formatted_pkg_scores)
    expect_true(all(c("Criteria", "Score", "Result", "Risk") %in% names(flex_df$body$dataset)))
    expect_true(all(c("Criteria", "Result", "Risk") %in% flex_df$col_keys))
  })

})
