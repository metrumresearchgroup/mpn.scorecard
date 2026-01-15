
skip_if_render_pdf()

describe("render scorecard and scorecard summary reports", {

  it("render_scorecard", {

    # `result_dirs_select` defined in tests/testthat/setup.R
    pdf_paths <- purrr::map_chr(result_dirs_select, ~{
      render_scorecard(results_dir = .x, overwrite = TRUE) %>% suppressWarnings()
    })

    on.exit(fs::file_delete(pdf_paths), add = TRUE)

    # Checking multiple PDFs to account for package failures and differences in outputs
    # This will mostly be valuable if we inspect more specific elements using `pdftools::pdf_text`
    for(pdf_path.i in pdf_paths){

      # Check attributes
      rendered_pdf_toc <- pdftools::pdf_toc(pdf = pdf_path.i)$children
      expect_equal(length(rendered_pdf_toc), 3)

      title_sections <- purrr::map_chr(rendered_pdf_toc, ~{.x$title})
      expect_equal(title_sections, c("Overview", "Details","Appendix"))

      title_sub_sections <- purrr::map_chr(rendered_pdf_toc[[2]]$children, ~{.x$title})
      expect_equal(length(title_sub_sections), 2)
      expect_equal(title_sub_sections, c("Documentation, Maintenance & Transparency", "Testing"))
    }
  })

  it("render_scorecard - relative path", {
    rdir <- result_dirs_select[1]
    withr::with_dir(dirname(rdir), {
      pdf_path <- render_scorecard(basename(rdir), overwrite = TRUE)
      on.exit(fs::file_delete(fs::path_abs(pdf_path)), add = TRUE)
      checkmate::expect_file_exists(pdf_path)
    })
  })

  it("render_scorecard - with traceability matrix", {

    # `result_dirs_select` defined in tests/testthat/setup.R
    result_dir_x <- pkg_select$pkg_result_dir[1]
    result_tar_x <- pkg_select$tar_file[1]

    expect_error(
      render_scorecard(
        results_dir = result_dir_x, overwrite = TRUE, add_traceability = TRUE
      ),
      "No traceability matrix found at"
    )

    # Create traceability matrix
    make_traceability_matrix(result_tar_x, result_dir_x)

    pdf_path <- render_scorecard(results_dir = result_dir_x, overwrite = TRUE)

    on.exit(fs::file_delete(pdf_path), add = TRUE)
    on.exit(fs::file_delete(get_result_path(result_dir_x, "export_doc.rds")), add = TRUE)

    # Check attributes
    rendered_pdf_toc <- pdftools::pdf_toc(pdf = pdf_path)$children
    expect_equal(length(rendered_pdf_toc), 4)

    title_sections <- purrr::map_chr(rendered_pdf_toc, ~{.x$title})
    expect_equal(title_sections, c("Overview", "Details", "Traceability Matrix","Appendix"))
  })

  it("render_scorecard_summary", {

    # `result_dirs_select` defined in tests/testthat/setup.R
    pdf_path <- render_scorecard_summary(result_dirs_select)
    on.exit(fs::file_delete(pdf_path), add = TRUE)

    # Check attributes
    rendered_pdf_toc <- pdftools::pdf_toc(pdf = pdf_path)$children
    expect_equal(length(rendered_pdf_toc), 4)

    title_sections <- purrr::map_chr(rendered_pdf_toc, ~{.x$title})
    expect_equal(title_sections, c("Summary", "Background", "Summary of Proof Points", "System Info"))

    title_sub_sections <- purrr::map_chr(rendered_pdf_toc[[2]]$children, ~{.x$title})
    expect_equal(length(title_sub_sections), 1)
    expect_equal(title_sub_sections, c("Principles of Good Practice"))
  })
})
