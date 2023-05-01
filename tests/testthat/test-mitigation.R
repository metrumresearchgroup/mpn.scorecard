
describe("mitigation file is properly included", {

  it("check_for_mitigation", {

    mitigation_template <- system.file("test-data", "mitigation-example.txt", package = "mpn.scorecard")

    result_dir <- result_dirs_select[3]

    # For inspecting
    # rstudioapi::filesPaneNavigate(result_dirs_select)

    # Should be NULL if no file
    expect_true(is.null(check_for_mitigation(result_dir)))
    mitigation_file <- fs::file_copy(mitigation_template, result_dir)

    # Should be NULL if not named properly
    expect_true(is.null(check_for_mitigation(result_dir)))

    # rename to expected name and check validity
    new_mitigation_name <- get_result_path(result_dir, "mitigation.txt")
    fs::file_move(mitigation_file, new_mitigation_name)
    on.exit(fs::file_delete(new_mitigation_name), add = TRUE)

    mitigation_block <- check_for_mitigation(result_dir)
    expect_false(is.null(mitigation_block))
    expect_true(is.character(mitigation_block))
    expect_true(length(mitigation_block) > 1)

  })

  it("build_risk_summary correctly formats mitigation column",{
    mitigation_template <- system.file("test-data", "mitigation-example.txt", package = "mpn.scorecard")

    # Add mitigation to non-high-risk package
    mitigation_file <- fs::file_copy(mitigation_template, result_dirs_select[1])
    new_mitigation_name <- get_result_path(result_dirs_select[1], "mitigation.txt")
    fs::file_move(mitigation_file, new_mitigation_name)
    on.exit(fs::file_delete(new_mitigation_name), add = TRUE)

    # Add mitigation to high-risk package
    mitigation_file <- fs::file_copy(mitigation_template, result_dirs_select[4])
    new_mitigation_name_high <- get_result_path(result_dirs_select[4], "mitigation.txt")
    fs::file_move(mitigation_file, new_mitigation_name_high)
    on.exit(fs::file_delete(new_mitigation_name_high), add = TRUE)


    overall_risk_summary <- build_risk_summary(result_dirs_select, risk_breaks = c(0.3, 0.7), out_dir = NULL)
    overall_pkg_scores <- overall_risk_summary$overall_pkg_scores

    # Check specific packages for mitigation
    expect_true(overall_pkg_scores$mitigation[1] == "Yes")
    expect_true(overall_pkg_scores$mitigation[4] == "Yes")

    # Make sure we have at least one case of High risk and "no" ()
    overall_pkg_scores <- overall_pkg_scores %>% mutate(
      mit_high_no = ifelse(
        overall_risk == "High Risk" & mitigation == "No", TRUE, FALSE
      )
    )
    expect_true(sum(overall_pkg_scores$mit_high_no) >= 1)

    # Overall Logic Test (agnostic to which packages have mitigation)
    purrr::map2_lgl(overall_pkg_scores$overall_risk, overall_pkg_scores$mitigation, ~{
      if(grepl("Medium|Low", .x)){
        expect_true(is.na(.y) || .y == "Yes")
      }else if(grepl("High", .x)){
        expect_true(.y == "Yes" || .y == "No")
      }
    })
  })

  it("confirm presence in rendered report", {
    skip_if_render_pdf()
    mitigation_template <- system.file("test-data", "mitigation-example.txt", package = "mpn.scorecard")

    result_dir <- result_dirs_select[3]

    # For inspecting
    # rstudioapi::filesPaneNavigate(result_dir)

    # Render once without mitigation (confirm section doesnt exist)
    pdf_path <- render_scorecard(results_dir = result_dir, overwrite = TRUE)


    rendered_pdf_text <- pdftools::pdf_text(pdf = pdf_path)
    expect_false(any(grepl("Mitigation", rendered_pdf_text)))

    # copy and rename mitigation file
    mitigation_file <- fs::file_copy(mitigation_template, result_dir)
    new_mitigation_name <- get_result_path(result_dir, "mitigation.txt")
    fs::file_move(mitigation_file, new_mitigation_name)
    on.exit(fs::file_delete(new_mitigation_name), add = TRUE)

    # Render with mitigation section
    pdf_path <- render_scorecard(results_dir = result_dir, overwrite = TRUE)
    on.exit(fs::file_delete(pdf_path), add = TRUE)

    # Check attributes
    rendered_pdf_toc <- pdftools::pdf_toc(pdf = pdf_path)$children
    title_sub_sections <- purrr::map_chr(rendered_pdf_toc[[2]]$children, ~{.x$title})

    expect_equal(length(title_sub_sections), 3)
    expect_equal(title_sub_sections, c("Documentation, Maintenance & Transparency", "Testing", "Mitigation"))


    # Check content
    rendered_pdf_text <- pdftools::pdf_text(pdf = pdf_path)
    mitigation_section <- rendered_pdf_text[grepl("Mitigation\n", rendered_pdf_text)]

    # Get expected block
    expected_mitigation_text <- check_for_mitigation(result_dir)

    # confirm header
    expect_true(any(grepl("Mitigation", mitigation_section)))
    # spot check first line of text (bullet formatting wont matter)
    expect_true(grepl(paste(expected_mitigation_text, collapse = "\n|"), mitigation_section))
  })


})
