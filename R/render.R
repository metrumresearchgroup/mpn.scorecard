#' Render an output file from an mpn.scorecard R Markdown template
#'
#' @param name Base name of an Rmd template in mpn.scorecard's `templates`
#'   directory.
#' @param output_file Name of the output file.
#' @param ... Arguments passed to `rmarkdown::render()`.
#' @return Absolute path of the `output_file`, invisibly.
#'
#' @noRd
render_template <- function(name, output_file, ...) {
  output_file <- fs::path_abs(output_file)

  # Note: Avoid passing vector of files to system.file call because
  # mustWork=TRUE only requires one of the files exists.
  src_files <- purrr::map_chr(
    c(
      name,
      "mpn_logo.png",
      "mpn_logo_header.png",
      "sum_links.bib",
      "title.tex"
    ),
    function(f) {
      system.file("templates", f, package = "mpn.scorecard", mustWork = TRUE)
    }
  )
  build_dir <- withr::local_tempdir(pattern = "mpn.scorecard-build-")
  fs::file_copy(src_files, build_dir)

  withr::local_dir(build_dir)
  tmpout <- rmarkdown::render(
    name,
    output_dir = build_dir,
    output_file = NULL,
    ...
  )
  fs::file_copy(tmpout, output_file)

  return(invisible(output_file))
}
