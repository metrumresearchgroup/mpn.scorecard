
#' Render PDF summary of scorecards
#'
#' @param result_dirs A vector of output directories
#' @param snapshot A report subtitle indicating the grouping of these packages, such as an MPN snapshot. See details.
#' @param out_dir Output directory for saving scorecard summary. If `NULL`, assumes all `result_dirs` point to the same output directory
#' @inheritParams render_scorecard
#'
#' @details
#'
#' The argument `snapshot`, will appear in the document with language similar to '**MPN Snapshot `<snapshot>`**'.
#' In other words, `snapshot` should be a date, or some other grouping that makes sense grammatically according to the this example.
#'
#' @export
render_scorecard_summary <- function(result_dirs,
                                     risk_breaks = c(0.3, 0.7),
                                     out_dir = NULL,
                                     snapshot = NULL,
                                     overwrite = TRUE){

  checkmate::assert_string(snapshot, null.ok = TRUE)
  if(is.null(snapshot)) snapshot <- as.character(Sys.Date())

  # Format overall scores and risk
  overall_risk_summary <- build_risk_summary(result_dirs, risk_breaks, out_dir)

  # Output file
  out_file <- get_result_path(overall_risk_summary$out_dir, "summary.pdf")
  check_exists_and_overwrite(out_file, overwrite)

  # mpn.scorecard version
  mpn_scorecard_ver <- format_scorecard_version(
    json_ver = NULL,
    as.character(utils::packageVersion("mpn.scorecard"))
  )

  # Render rmarkdown
  rendered_file <- rmarkdown::render(
    system.file(SUM_SCORECARD_RMD_TEMPLATE, package = "mpn.scorecard", mustWork = TRUE),
    output_dir = overall_risk_summary$out_dir,
    output_file = basename(out_file),
    quiet = TRUE,
    params = list(
      overall_risk_summary = overall_risk_summary,
      set_subtitle = snapshot,
      scorecard_footer = mpn_scorecard_ver
    )
  )

  # Render to PDF, invisibly return the path to the PDF
  return(invisible(rendered_file))
}


#' Summarise overall risks for each package
#'
#' @inheritParams render_scorecard_summary
#' @param append_out_dir Logical (T/F). If `TRUE`, keep the package-specific output directory as part of the returned table.
#'
#' @keywords internal
build_risk_summary <- function(result_dirs,
                               risk_breaks,
                               out_dir,
                               append_out_dir = FALSE
){

  res_sum <- list()
  res_sum$metadata <- get_metadata()


  overall_pkg_scores <- purrr::map(result_dirs, ~{
    json_path <- get_result_path(.x, "scorecard.json")
    checkmate::assert_file_exists(json_path)
    pkg_scores <- jsonlite::fromJSON(json_path)

    out_dir_stored <- pkg_scores$out_dir
    checkmate::assert_string(out_dir_stored, null.ok = TRUE)

    # Overall scores and risk
    formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks)

    overall_risk <- formatted_pkg_scores$formatted$overall_scores %>%
      dplyr::filter(category == "overall") %>% dplyr::select(-c(category)) %>%
      dplyr::rename("overall_risk" = "risk", "overall_score" = "category_score")


    # mitigation - we want empty mitigation cells by default
    mitigation_txt <- if(is.null(check_for_mitigation(.x))) NA_character_ else "Yes"
    # Overwrite to 'No' for High Risk packages
    if(is.na(mitigation_txt) && overall_risk$overall_risk == "High Risk"){
      mitigation_txt <- "No"
    }

    pkg_info <- data.frame(
      package = formatted_pkg_scores$pkg_name,
      version = formatted_pkg_scores$pkg_version,
      out_dir = out_dir_stored,
      mitigation = mitigation_txt
    )

    cbind(pkg_info, overall_risk) %>% dplyr::relocate(mitigation, .after = everything())
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

  # Remove out_dir for report
  if(!isTRUE(append_out_dir)){
    overall_pkg_scores <- overall_pkg_scores %>% dplyr::select(-c("out_dir"))
  }

  res_sum$overall_pkg_scores <- overall_pkg_scores

  return(res_sum)
}


#' Review important package metrics prior to running [render_scorecard_summary()]
#'
#' @inheritParams render_scorecard_summary
#'
#' @returns a dataframe
#' @export
summarize_package_results <- function(result_dirs){

  json_paths <- get_result_path(result_dirs, "scorecard.json")

  risk_summary_df <- tibble::tibble(
    result_dir = dirname(json_paths),
    res_obj = purrr::map(json_paths, jsonlite::read_json),
    package = purrr::map_chr(.data$res_obj, "pkg_name"),
    version = purrr::map_chr(.data$res_obj, "pkg_version"),
    pkg_name_ver = paste0(.data$package, "_", version),
    overall_score = purrr::map_dbl(.data$res_obj, c("category_scores", "overall")),
    has_mitigation = purrr::map_chr(.data$result_dir, ~ (if(is.null(check_for_mitigation(.x))) "no" else "yes")),
    check_obj = purrr::map(paste0(.data$result_dir, "/", .data$pkg_name_ver, ".check.rds"), readRDS),
    check_status = purrr::map_int(.data$check_obj, "status"),
    check_test_fail = purrr::map(.data$check_obj, "test_fail"),
    check_test_output = purrr::map(.data$check_obj, "test_output"),
    check_errors = purrr::map(.data$check_obj, "errors"),
    check_warnings = purrr::map(.data$check_obj, "warnings"),
    covr_obj = purrr::map(paste0(.data$result_dir, "/", .data$pkg_name_ver, ".covr.rds"), readRDS),
    covr_success = !is.na(purrr::map_dbl(.data$covr_obj, c("coverage", "totalcoverage"))),
    covr_errors = purrr::map(.data$covr_obj, "errors"),
    covr_error_msg = purrr::map_chr(
      .data$covr_errors,
      function(x) if (inherits(x, "error")) conditionMessage(x) else NA_character_)
  ) %>% dplyr::select(-c("result_dir", "res_obj", "pkg_name_ver", "check_obj", "covr_obj", "covr_errors"))


  return(risk_summary_df)
}
