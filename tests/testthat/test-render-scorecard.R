
skip_if_render_pdf()

describe("render_scorecard", {

  it("confirm default behavior", {

    # `result_dirs_select` and `pkg_select` defined in tests/testthat/setup.R

    pdf_paths <- purrr::map_chr(result_dirs_select, ~{
      render_scorecard(results_dir = .x, overwrite = TRUE)
    })

    # For inspecting
    # rstudioapi::filesPaneNavigate(result_dirs[1])

    on.exit(fs::file_delete(pdf_paths), add = TRUE)

    # Checking multiple PDFs to account for package failures and differences in outputs
    # This will mostly be valuable if we inspect more specific elements using `pdftools::pdf_text`
    for(pdf_path.i in pdf_paths){

      # Check attributes
      rendered_pdf_toc <- pdftools::pdf_toc(pdf = pdf_path.i)$children
      expect_equal(length(rendered_pdf_toc), 3)

      title_sections <- purrr::map_chr(rendered_pdf_toc, ~{.x$title})
      expect_equal(title_sections, c("Overview", "Details", "System Info"))

      title_sub_sections <- purrr::map_chr(rendered_pdf_toc[[2]]$children, ~{.x$title})
      expect_equal(length(title_sub_sections), 2)
      expect_equal(title_sub_sections, c("Documentation, Maintenance & Transparency", "Testing"))
    }
  })


})
