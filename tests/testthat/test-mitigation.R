
describe("mitigation file is properly included", {

  it("check_for_mitigation", {

    mitigation_template <- system.file("test-data", "mitigation-example.txt", package = "mpn.scorecard")

    result_dir <- pkg_dirs$pkg_setups_df$pkg_result_dir[3]

    # For inspecting
    # rstudioapi::filesPaneNavigate(result_dir)

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

  it("confirm presence in rendered report", {
    skip_if_render_pdf()
    mitigation_template <- system.file("test-data", "mitigation-example.txt", package = "mpn.scorecard")

    result_dir <- pkg_dirs$pkg_setups_df$pkg_result_dir[3]

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
