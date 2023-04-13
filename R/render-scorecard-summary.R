
#' Render PDF summary of scorecards
#'
#' @param json_paths A vector of json paths
#' @param snapshot A report subtitle indicating the grouping of these packages, such as an MPN snapshot. See details.
#' @param out_dir Output directory for saving scorecard summary. If `NULL`, assumes all `json_paths` point to the same output directory
#' @inheritParams render_scorecard
#'
#' @details
#'
#' The argument `snapshot`, will appear in the document with language similar to '**MPN Snapshot `<snapshot>`**'.
#' In other words, `snapshot` should be a date, or some other grouping that makes sense grammatically according to the this example.
#'
#' @export
render_scorecard_summary <- function(json_paths,
                                     risk_breaks = c(0.3, 0.7),
                                     out_dir = NULL,
                                     snapshot = NULL,
                                     overwrite = TRUE){

  checkmate::assert_string(snapshot, null.ok = TRUE)

  # Format overall scores and risk
  overall_risk_summary <- build_risk_summary(json_paths, risk_breaks, out_dir)

  # Output file
  out_file <- get_result_path(overall_risk_summary$out_dir, "snapshot_summary.pdf")
  check_exists_and_overwrite(out_file, overwrite)

  # Render rmarkdown
  rendered_file <- rmarkdown::render(
    system.file(SUM_SCORECARD_RMD_TEMPLATE, package = "mpn.scorecard"),
    output_dir = overall_risk_summary$out_dir,
    output_file = basename(out_file),
    quiet = TRUE,
    params = list(
      overall_risk_summary = overall_risk_summary,
      set_subtitle = snapshot
    )
  )

  # Render to PDF, invisibly return the path to the PDF
  return(invisible(rendered_file))
}


#' Summarise overall risks for each package
#'
#' @inheritParams render_scorecard_summary
#'
#' @keywords internal
build_risk_summary <- function(json_paths, risk_breaks, out_dir){

  res_sum <- list()
  res_sum$metadata <- get_metadata()




  overall_pkg_scores <- purrr::map(json_paths, ~{
    checkmate::assert_file_exists(.x)
    pkg_scores <- jsonlite::fromJSON(.x)

    out_dir_stored <- pkg_scores$out_dir
    checkmate::assert_string(out_dir_stored, null.ok = TRUE)

    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks)
    overall_risk <- formatted_pkg_scores$formatted$overall_risk %>%
      dplyr::filter(Category == "overall") %>% dplyr::select(-c(Category)) %>%
      dplyr::rename(`Overall Risk` = Risk)
    pkg_info <- data.frame(
      Package = formatted_pkg_scores$pkg_name,
      Version = formatted_pkg_scores$pkg_version,
      out_dir = out_dir_stored
    )

    cbind(pkg_info, overall_risk)
  }) %>% purrr::list_rbind()


  # Directories can be assumed to exist still since the json is stored there
  # Check that they all point to the same path if specified `out_dir` is `NULL`
  out_dirs_stored <- overall_pkg_scores %>% dplyr::pull(out_dir) %>% dirname() %>% unique()
  if(length(out_dirs_stored) == 1 && is.null(out_dir)){
    out_dir <- out_dirs_stored
  }else if(!is.null(out_dir)){
    # checkmate::assert_directory_exists(out_dir)
    if(!fs::dir_exists(out_dir)) fs::dir_create(out_dir)
  }else{
    # Triggered only if `out_dir` is `NULL` but multiple directories found in `out_dirs_stored`
    out_dirs_stored_txt <- paste(out_dirs_stored, collapse = "\n")
    stop(glue::glue("out_dir was specified as `NULL`, but multiple directories were found in the provided json paths:
                 {out_dirs_stored_txt}

                 See ?render_scorecard_summary for more details
                 "))
  }

  res_sum$out_dir <- out_dir
  res_sum$overall_pkg_scores <- overall_pkg_scores %>% dplyr::select(-c(out_dir))

  return(res_sum)
}

