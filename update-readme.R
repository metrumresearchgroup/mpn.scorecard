
# Note: make sure to run devtools::load_all() before running this function
# This should source `tests/testthat/helpers-create-fake-package.R`, but otherwise
# make sure that script is sourced.
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

  # Summary Report: uses nmrec and 3 fake packages
  # Note: This was added to make sure formatting changes to the summary report
  # are always captured
  pkg_dirs <- setup_multiple_pkgs(
    pkg_types = c("pass_success", "pass_warning", "fail_test"), pkg_prefix = "pkg",
    testing_dir = file.path(tempdir(), "fake_pkgs")
  )
  result_dirs <- purrr::map_chr(pkg_dirs$pkg_setups_df$tar_file, ~{
    local_check_envvar()
    score_pkg(.x, pkg_dirs$all_results_dir, overwrite = TRUE) %>% suppressMessages()
  })
  overall_risk_summary <- build_risk_summary(
    result_dirs = c(results_dir, result_dirs), risk_breaks = c(0.3, 0.7),
    out_dir = pkg_dirs$all_results_dir
  )
  risk_summary_df <- overall_risk_summary$overall_pkg_scores %>% dplyr::mutate(
    # replace `nmrec` with first example package name in the readme
    package = ifelse(package == "nmrec", "package", package),
    # match to example versions in readme
    version = c("3.1.0", "2.0.4", "1.6.3", "0.3.0")
  )
  summary_tab <- format_score_summaries(risk_summary_df)

  # Save out PNGs
  flextable::save_as_image(overall_scores, file.path(png_dir, "overall_scores.png"))
  flextable::save_as_image(category_scores, file.path(png_dir, "category_scores.png"))
  flextable::save_as_image(testing_scores, file.path(png_dir, "testing_scores.png"))
  flextable::save_as_image(trac_matrix, file.path(png_dir, "trac_matrix.png"))
  flextable::save_as_image(summary_tab, file.path(png_dir, "summary_tab.png"))

  # Render README
  readme <- here::here("README.Rmd")
  rmarkdown::render(readme, quiet = TRUE, output_format = "github_document")
}

