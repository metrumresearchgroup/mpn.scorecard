# These are slimmed down variants of some tests from test-render-scorecard.R to
# check that render_scorecard() doesn't choke on external results.

skip_if_render_pdf()

describe("render scorecard for external scores", {
  it("render_scorecard - with traceability YAML", {
    rdir <- local_create_external_results()

    pdf_path <- render_scorecard(rdir)
    toc <- pdftools::pdf_toc(pdf = pdf_path)[["children"]]
    expect_identical(
      purrr::map_chr(toc, "title"),
      c("Overview", "Details", "Traceability Matrix", "Appendix")
    )

    pdf_path <- render_scorecard(rdir,
      add_traceability = FALSE, overwrite = TRUE
    )
    toc <- pdftools::pdf_toc(pdf = pdf_path)[["children"]]
    expect_identical(
      purrr::map_chr(toc, "title"),
      c("Overview", "Details", "Appendix")
    )
  })

  it("render_scorecard - without traceability YAML", {
    rdir <- local_create_external_results()
    fs::file_delete(get_result_path(rdir, "matrix.yaml"))

    pdf_path <- render_scorecard(rdir)
    toc <- pdftools::pdf_toc(pdf = pdf_path)[["children"]]
    expect_identical(
      purrr::map_chr(toc, "title"),
      c("Overview", "Details", "Appendix")
    )

    fs::file_delete(pdf_path)
    expect_error(render_scorecard(rdir, add_traceability = TRUE))
  })

  it("render_scorecard - without coverage", {
    rdir <- local_create_external_results()
    fs::file_delete(get_result_path(rdir, "coverage.json"))

    expect_error(render_scorecard(rdir), "coverage file is missing")

    scores <- jsonlite::read_json(get_result_path(rdir, "scores.json"))
    scores[["testing"]][["coverage"]] <- NULL
    jsonlite::write_json(scores, get_result_path(rdir, "scores.json"),
      auto_unbox = TRUE
    )

    pdf_path <- render_scorecard(rdir)
    toc <- pdftools::pdf_toc(pdf = pdf_path)[["children"]]
    appendix <- toc[[4]]
    expect_identical(appendix[["title"]], "Appendix")
    expect_identical(
      purrr::map_chr(appendix[["children"]], "title"),
      c("System Info", "Check output")
    )
  })
})
