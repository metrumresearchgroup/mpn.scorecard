
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

    result_dir <- result_dirs_select[["pass_success"]]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # map scores to risk and format into tables to be written to PDF
    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))

    flex_df <- format_overall_scores(formatted_pkg_scores)
    expect_true(all(c("Category", "Category Score", "Risk") %in% names(flex_df$body$dataset)))
  })

  it("format_package_details", {

    result_dir <- result_dirs_select[["pass_success"]]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # map scores to risk and format into tables to be written to PDF
    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))

    flex_df <- format_package_details(formatted_pkg_scores)
    expect_true(all(c("Category","Criteria", "Result", "Risk") %in% names(flex_df$body$dataset)))
    expect_true(all(c("Criteria", "Result", "Risk") %in% flex_df$col_keys))
  })

  it("format_testing_scores", {

    ## Fully passing rcmdcheck score ##
    result_dir <- result_dirs_select[["pass_success"]]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # map scores to risk and format into tables to be written to PDF
    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))
    flex_df <- format_testing_scores(formatted_pkg_scores)

    expect_true(all(c("Criteria", "Score", "Result", "Risk") %in% names(flex_df$body$dataset)))
    expect_true(all(c("Criteria", "Result", "Risk") %in% flex_df$col_keys))
    expect_equal(flex_df$body$dataset$Score, c(1, 1))
    expect_equal(flex_df$body$dataset$Result, c("Passing (score: 1)", "100%"))

    ## High rcmdcheck score ##
    result_dir <- result_dirs_select[["pass_no_docs"]]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # map scores to risk and format into tables to be written to PDF
    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))
    flex_df <- format_testing_scores(formatted_pkg_scores)

    expect_equal(flex_df$body$dataset$Score, c(0.75, 1))
    expect_equal(flex_df$body$dataset$Result, c("Passing (score: 0.75)", "100%"))


    ## Failing rcmdcheck score ##
    result_dir <- result_dirs_select[["fail_func_syntax"]]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # map scores to risk and format into tables to be written to PDF
    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))
    flex_df <- format_testing_scores(formatted_pkg_scores)

    expect_equal(flex_df$body$dataset$Score, c(0, NA))
    expect_equal(unique(flex_df$body$dataset$Result), "Failed")
  })

  it("format_traceability_matrix", {
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file
    exports_df <- make_traceability_matrix(result_dir_x, pkg_tar_path = pkg_tar_x)

    export_doc_path <- get_result_path(result_dir_x, "export_doc.rds")
    expect_true(fs::file_exists(export_doc_path))
    on.exit(fs::file_delete(export_doc_path), add = TRUE)

    exported_func_flex <- format_traceability_matrix(exports_df)

    # Test exported functions dataframe
    exported_func_df <- exported_func_flex$body$dataset
    expect_equal(
      names(format_colnames_to_title(exports_df %>% dplyr::select(-"test_dirs"))),
      names(exported_func_df)
    )
    expect_equal(
      unique(unname(unlist(exported_func_flex$footer$dataset))),
      "Testing directories: tests/testthat"
    )

    ## Edge cases ##

    # Test multiple testing directories
    exports_df2 <- bind_rows(exports_df, exports_df)
    exports_df2$test_dirs[[2]] <- "inst/unit-tests/tests"
    exported_func_flex <- format_traceability_matrix(exports_df2)
    expect_equal(
      unique(unname(unlist(exported_func_flex$footer$dataset))),
      "Testing directories: tests/testthat, inst/unit-tests/tests"
    )

    # No tests found for some functions
    exports_df2$test_files[[2]] <- ""
    exports_df2$test_dirs[2] <- list(NULL)
    exported_func_flex <- format_traceability_matrix(exports_df2)
    exported_func_df <- exported_func_flex$body$dataset
    # make sure rows were not dropped (main test)
    expect_equal(nrow(exported_func_df), nrow(exports_df2))
    # check test files
    test_files <- exported_func_df$`Test Files`
    expect_equal(unique(test_files), c("test-myscript.R", ""))
  })

  it("format_appendix", {
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    result_dir_x <- pkg_setup_select$pkg_result_dir

    extra_notes_data <- create_extra_notes(result_dir_x)
    extra_notes_frmt <- format_appendix(extra_notes_data)

    # Test covr dataframe
    covr_results_df <- extra_notes_frmt$covr_results_flex$body$dataset
    expect_equal(
      names(format_colnames_to_title(extra_notes_data$covr_results_df)),
      names(covr_results_df)
    )
    expect_equal(
      unique(unname(unlist(extra_notes_frmt$covr_results_flex$footer$dataset))),
      paste(
        "Test coverage is calculated per script, rather than per function.",
        "See Traceability Matrix for function-to-test-file mapping."
      )
    )
  })

  it("dependency versions", {
    setups <- pkg_dirs$pkg_setups_df

    value_to_render <- function(type) {
      setup <- setups[setups$pkg_type == type, ]
      stopifnot(nrow(setup) == 1)

      dv <- get_dependency_versions(setup$pkg_result_dir, setup$pkg_name)
      prepare_dependency_versions(dv)
    }

    val <- value_to_render("pass_success")
    flexdf <- val$body$dataset
    expect_identical(names(flexdf), c("package", "version"))
    expect_true("checkmate" %in% flexdf$package)
    # Scored package is filtered out.
    expect_false(any(grepl("^package[0-9]+", flexdf$package)))

    val_rendered <- knitr::knit_print(val)
    expect_match(val_rendered, "Package", fixed = TRUE)
    expect_match(val_rendered, "Version", fixed = TRUE)

    val <- value_to_render("pass_no_functions")
    expect_identical(val, "Package has no required dependencies.")

    val <- value_to_render("fail_func_syntax")
    expect_identical(
      val,
      "Unable to calculate R dependency table due to failing `R CMD check`."
    )
  })
})

describe("cat_verbatim", {
  it("sanitizes spverbatim end", {
    expect_output(
      cat_verbatim("foo\n\\end{spverbatim}"),
      "foo\n<SANITIZED BACKSLASH>end{spverbatim}",
      fixed = TRUE
    )
    expect_output(
      cat_verbatim("\\end{spverbatim}\nfoo \\end{spverbatim} bar"),
      "<SANITIZED BACKSLASH>end{spverbatim}\nfoo <SANITIZED BACKSLASH>end{spverbatim} bar",
      fixed = TRUE
    )
  })

  it("sanitizes code block end", {
    expect_output(
      cat_verbatim("foo\n\t``` \n```\na```\n```b\n"),
      "foo\n<SANITIZED>\t``` \n<SANITIZED>```\na```\n```b\n",
      fixed = TRUE
    )
  })
})
