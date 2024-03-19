
update_readme <- function(png_dir = here::here("man", "figures")){

  # Set up location for scoring package
  pkg_tar_dir <- withr::local_tempdir("mpn-scorecard-")

  # Download example package tarball (can change this to MPN snapshot later if desired)
  pkg_tar <- download.packages("nmrec", destdir = pkg_tar_dir, repos = "https://mpn.metworx.com/snapshots/stable/2023-09-19")

  # Score package
  results_dir <- score_pkg(
    pkg = pkg_tar[,2],
    out_dir = pkg_tar_dir,
    overwrite = TRUE
  )

  json_path <- get_result_path(results_dir, "scorecard.json")
  pkg_scores <- jsonlite::fromJSON(json_path)
  formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks = c(0.3, 0.7))
  exports_df <- make_traceability_matrix(pkg_scores$pkg_tar_path, results_dir)

  # Overall table
  overall_scores <- format_overall_scores(formatted_pkg_scores)

  # Package Details
  category_scores <- format_package_details(formatted_pkg_scores)

  # Testing
  testing_scores <- format_testing_scores(formatted_pkg_scores)

  # Traceability Matrix
  trac_matrix <- format_traceability_matrix(exports_df)

  # Save out PNGs
  flextable::save_as_image(overall_scores, file.path(png_dir, "overall_scores.png"))
  flextable::save_as_image(category_scores, file.path(png_dir, "category_scores.png"))
  flextable::save_as_image(testing_scores, file.path(png_dir, "testing_scores.png"))
  flextable::save_as_image(trac_matrix, file.path(png_dir, "trac_matrix.png"))

  # Render README
  readme <- here::here("README.Rmd")
  rmarkdown::render(readme, quiet = TRUE, output_format = "github_document")
}

