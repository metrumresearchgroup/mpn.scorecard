
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
      expect_setequal(
        scores_df[["criteria"]],
        c(
          "R CMD CHECK", "coverage",
          DOCUMENTATION_METRICS, MAINTENANCE_METRICS, TRANSPARENCY_METRICS
        )
      )

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
    expect_equal(flex_df$body$dataset$Result, c("Passing (score: 1)", "100.00%"))

    ## High rcmdcheck score ##
    result_dir <- result_dirs_select[["pass_no_docs"]]
    json_path <- get_result_path(result_dir, "scorecard.json")
    pkg_scores <- jsonlite::fromJSON(json_path)

    # map scores to risk and format into tables to be written to PDF
    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))
    flex_df <- format_testing_scores(formatted_pkg_scores)

    expect_equal(flex_df$body$dataset$Score, c(0.75, 1))
    expect_equal(flex_df$body$dataset$Result, c("Passing (score: 0.75)", "100.00%"))


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
    exports_df2$test_files[[2]] <- NA_character_
    exports_df2$test_dirs[2] <- list(NULL)
    exported_func_flex <- format_traceability_matrix(exports_df2)
    exported_func_df <- exported_func_flex$body$dataset
    # make sure rows were not dropped (main test)
    expect_equal(nrow(exported_func_df), nrow(exports_df2))
    # check test files
    test_files <- exported_func_df$`Test Files`
    expect_equal(unique(test_files), c("test-myscript.R", ""))
  })

  it("format_traceability_matrix: exports missing def", {
    scenario <- dplyr::filter(
      pkg_dirs[["pkg_setups_df"]],
      pkg_type == "fail_func_syntax"
    )
    stopifnot(nrow(scenario) == 1)
    res_dir <- scenario[["pkg_result_dir"]]
    tarfile <- scenario[["tar_file"]]

    export_doc_path <- get_result_path(res_dir, "export_doc.rds")
    on.exit(unlink(export_doc_path))

    expect_false(fs::file_exists(export_doc_path))
    expect_warning(
      exports_df <- make_traceability_matrix(res_dir, pkg_tar_path = tarfile),
      "Failed to parse"
    )
    expect_true(fs::file_exists(export_doc_path))

    tbl <- format_traceability_matrix(exports_df)
    expect_equal(
      unique(unname(unlist(tbl$footer$dataset))),
      "Testing directories: tests/testthat"
    )
  })

  it("format_traceability_matrix creates new entries that span multiple pages", {
    # Tests that `split_long_rows` works correctly
    pkg_setup_select <- dplyr::filter(
      pkg_dirs[["pkg_setups_df"]],
      pkg_type == "pass_success"
    )

    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file
    exports_df <- make_traceability_matrix(result_dir_x, pkg_tar_path = pkg_tar_x)

    export_doc_path <- get_result_path(result_dir_x, "export_doc.rds")
    on.exit(unlink(export_doc_path))

    # Duplicate test 100 times to create two `export (cont.)` entries
    exports_df$test_files[[1]] <- paste0("test-myscript_", seq(100), ".R")

    exported_func_flex <- format_traceability_matrix(exports_df)
    exported_func_df <- exported_func_flex$body$dataset
    expect_equal(
      exported_func_df$`Exported Function`,
      c("myfunction", "myfunction (cont.)", "myfunction (cont.)")
    )

    # Check that the test scripts are in the right location
    expect_true(all(grepl(paste(1:40, collapse = "|"), strsplit(exported_func_df[1,]$`Test Files`, "\n")[[1]])))
    expect_true(all(grepl(paste(41:80, collapse = "|"), strsplit(exported_func_df[2,]$`Test Files`, "\n")[[1]])))
    expect_true(all(grepl(paste(81:100, collapse = "|"), strsplit(exported_func_df[3,]$`Test Files`, "\n")[[1]])))
  })

  it("split_long_rows", {
    # Other formatting is done in format_traceability_matrix before
    # split_long_rows is called. Integration test will suffice
    mock_exports_df <- tibble::tibble(
      exported_function = "example_func",
      code_file = list("R/example_func.R"),
      documentation = list("man/example_func.Rd"),
      test_files = list(rep("test-example_func.R", 60)),
      test_dirs = list(NULL)
    )

    mock_exports_flex <- format_traceability_matrix(mock_exports_df)
    mock_exports_split <- tibble::as_tibble(mock_exports_flex$body$dataset)
    expect_equal(
      mock_exports_split$`Exported Function`,
      c("example_func", "example_func (cont.)")
    )
    # Default cutoff for making a new export is 40, so there should be 39 rows
    expect_equal(stringr::str_count(mock_exports_split$`Test Files`[1], "\n"), 39)

    # Regression test: works when other columns are empty
    # - This occurred when columns `code_file`, `documentation`, and `test_files`
    #   were _all_ empty
    mock_exports_df$code_file <- list(NA_character_)
    mock_exports_df$documentation <- list(NA_character_)
    mock_exports_df$test_files <- list(NA_character_)
    mock_exports_flex <- format_traceability_matrix(mock_exports_df)
    mock_exports_split <- tibble::as_tibble(mock_exports_flex$body$dataset)
    expect_equal(mock_exports_split$`Exported Function`, "example_func")
  })

  it("format_appendix", {
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    result_dir_x <- pkg_setup_select$pkg_result_dir

    extra_notes_data <- create_extra_notes(result_dir_x)
    extra_notes_frmt <- format_appendix(extra_notes_data, return_vals = TRUE)

    # Test covr dataframe
    covr_results_df <- extra_notes_frmt$cov_results_flex$body$dataset
    expect_equal(
      names(format_colnames_to_title(extra_notes_data$cov_results_df)),
      names(covr_results_df)
    )
    expect_equal(
      unique(unname(unlist(extra_notes_frmt$cov_results_flex$footer$dataset))),
      paste(
        "Test coverage is calculated per script, rather than per function.",
        "See Traceability Matrix for function-to-test-file mapping."
      )
    )
  })

  it("format_appendix: covr failure", {
    expect_output(
      format_appendix(
        list(
          check_output = "anything",
          cov_results_df = list(code_file = "File coverage failed")
        )
      ),
      "Calculating code coverage failed"
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

  it("format_metadata handles NULL input", {
    flex_df <- format_metadata(list(date = "2024-01-01", executor = "foo"))
    expect_identical(
      flex_df[["body"]][["dataset"]],
      data.frame(
        Category = c("Date", "Executor"),
        Value = c("2024-01-01", "foo")
      )
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
