
update_readme <- function(png_dir = here::here("man", "figures")){
  # Build Package
  pkg_tar_dir <- file.path(system.file("", package = "mpn.scorecard", mustWork = TRUE), "build_package") %>% fs::path_norm()
  fs::dir_create(pkg_tar_dir)
  pkg_tar <- devtools::build(path = pkg_tar_dir, quiet = TRUE)
  withr::defer(unlink(pkg_tar_dir, recursive = TRUE))


  # Score package
  results_dir <- score_pkg(
    pkg = pkg_tar,
    out_dir = pkg_tar_dir,
    overwrite = TRUE
  )

  json_path <- get_result_path(results_dir, "scorecard.json")
  pkg_scores <- jsonlite::fromJSON(json_path)
  formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))
  exports_df <- make_traceability_matrix(pkg_scores$pkg_tar_path, results_dir)

  # Overall table
  overall_scores <- format_overall_scores(formatted_pkg_scores)
  flextable::save_as_image(overall_scores, file.path(png_dir, "overall_scores.png"))

  # Package Details
  category_scores <- format_package_details(formatted_pkg_scores)
  flextable::save_as_image(category_scores, file.path(png_dir, "category_scores.png"))

  # Traceability Matrix
  trac_matrix <- format_traceability_matrix(exports_df, return_vals = TRUE)
  flextable::save_as_image(trac_matrix, file.path(png_dir, "trac_matrix.png"))

  # Render README
  readme <- here::here("README.Rmd")
  rmarkdown::render(readme, quiet = TRUE, output_format = "github_document")
}

