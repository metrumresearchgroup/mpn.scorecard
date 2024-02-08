
describe("comments file is properly included", {

  it("check_for_comments", {

    result_dir <- result_dirs_select[["fail_func_syntax"]]

    # For inspecting
    # rstudioapi::filesPaneNavigate(result_dir)

    # Should be NULL if no file
    expect_true(is.null(check_for_comments(result_dir)))
    comments_file <- fs::file_copy(comments_template, result_dir)

    # Should be NULL if not named properly
    expect_true(is.null(check_for_comments(result_dir)))

    # Rename to expected name and check validity
    new_comments_name <- get_result_path(result_dir, "comments.txt")
    fs::file_move(comments_file, new_comments_name)
    on.exit(fs::file_delete(new_comments_name), add = TRUE)

    comments_block <- check_for_comments(result_dir)
    expect_false(is.null(comments_block))
    expect_true(is.character(comments_block))
    expect_true(length(comments_block) > 1)
  })

  it("build_risk_summary correctly formats comments column",{

    # Add comments to non-high-risk package
    pass_idx <- match("pass_success", names(result_dirs_select))
    comments_file <- fs::file_copy(comments_template, result_dirs_select[[pass_idx]])
    new_comments_name <- get_result_path(result_dirs_select[[pass_idx]], "comments.txt")
    fs::file_move(comments_file, new_comments_name)
    on.exit(fs::file_delete(new_comments_name), add = TRUE)

    # Add comments to high-risk package
    fail_idx <- match("fail_test", names(result_dirs_select))
    comments_file <- fs::file_copy(comments_template, result_dirs_select[[fail_idx]])
    new_comments_name_high <- get_result_path(result_dirs_select[[fail_idx]], "comments.txt")
    fs::file_move(comments_file, new_comments_name_high)
    on.exit(fs::file_delete(new_comments_name_high), add = TRUE)


    overall_risk_summary <- build_risk_summary(result_dirs_select, risk_breaks = c(0.3, 0.7), out_dir = NULL)
    overall_pkg_scores <- overall_risk_summary$overall_pkg_scores

    # Check specific packages for comments
    expect_true(overall_pkg_scores$comments[pass_idx] == "Yes")
    expect_true(overall_pkg_scores$comments[fail_idx] == "Yes")

    # Make sure we have at least one case of High risk and "no" ()
    overall_pkg_scores <- overall_pkg_scores %>% mutate(
      mit_high_no = ifelse(
        overall_risk == "High Risk" & comments == "No", TRUE, FALSE
      )
    )
    expect_true(sum(overall_pkg_scores$mit_high_no) >= 1)

    # Overall Logic Test (agnostic to which packages have comments)
    purrr::map2_lgl(overall_pkg_scores$overall_risk, overall_pkg_scores$comments, ~{
      if(grepl("Medium|Low", .x)){
        expect_true(is.na(.y) || .y == "Yes")
      }else if(grepl("High", .x)){
        expect_true(.y == "Yes" || .y == "No")
      }
    })
  })

  it("confirm presence in rendered report", {
    skip_if_render_pdf()

    result_dir <- result_dirs_select[["fail_func_syntax"]]

    # For inspecting
    # rstudioapi::filesPaneNavigate(result_dir)

    # Render once without comments (confirm section doesnt exist)
    pdf_path <- render_scorecard(results_dir = result_dir, overwrite = TRUE)

    rendered_pdf_text <- pdftools::pdf_text(pdf = pdf_path)
    expect_false(any(grepl("Comments", rendered_pdf_text)))

    # Warn if named mitigation
    comments_file <- fs::file_copy(comments_template, result_dir)
    comments_name_mit <- get_result_path(result_dir, "mitigation.txt")
    fs::file_move(comments_file, comments_name_mit)
    rlang::local_options(lifecycle_verbosity = "warning")
    expect_warning(
      render_scorecard(results_dir = result_dir, overwrite = TRUE),
      "deprecated as of mpn.scorecard"
    )

    # Rename comments file
    new_comments_name <- get_result_path(result_dir, "comments.txt")
    fs::file_move(comments_name_mit, new_comments_name)
    on.exit(fs::file_delete(new_comments_name), add = TRUE)

    # Render with comments section
    pdf_path <- render_scorecard(results_dir = result_dir, overwrite = TRUE)
    on.exit(fs::file_delete(pdf_path), add = TRUE)

    # Check attributes
    rendered_pdf_toc <- pdftools::pdf_toc(pdf = pdf_path)$children
    title_sub_sections <- purrr::map_chr(rendered_pdf_toc[[2]]$children, ~{.x$title})

    expect_equal(length(title_sub_sections), 3)
    expect_equal(title_sub_sections, c("Documentation, Maintenance & Transparency", "Testing", "Comments"))

    # Check content
    rendered_pdf_text <- pdftools::pdf_text(pdf = pdf_path)
    comments_section <- rendered_pdf_text[grepl("Comments\n", rendered_pdf_text)]

    # Get expected block
    expected_comments_text <- check_for_comments(result_dir)

    # confirm header
    expect_true(any(grepl("Comments", comments_section)))
    # spot check first line of text (bullet formatting wont matter)
    expect_true(grepl(paste(expected_comments_text, collapse = "\n|"), comments_section))
  })
})
