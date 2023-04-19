
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

    pkg_info <- data.frame(
      package = formatted_pkg_scores$pkg_name,
      version = formatted_pkg_scores$pkg_version,
      out_dir = out_dir_stored,
      mitigation = mitigation_txt
    )

    cbind(pkg_info, overall_risk) %>% relocate(mitigation, .after = everything())
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
    overall_pkg_scores <- overall_pkg_scores %>% dplyr::select(-c(out_dir))
  }

  res_sum$overall_pkg_scores <- overall_pkg_scores

  return(res_sum)
}


#' Review important package metrics prior to running [render_scorecard_summary()]
#'
#' @inheritParams render_scorecard_summary
#' @param keep_result_paths Logical (T/F). If `TRUE`, return result paths and `out_dir` directory as columns
#'
#' @returns a datadrame
#' @export
package_summary <- function(result_dirs,
                            keep_result_paths = FALSE
){

  # We dont care about risk here:
  # assign arbitrary value to `risk_breaks` so we can still rely on `build_risk_summary`
  overall_risk_summary <- build_risk_summary(result_dirs,
                                             risk_breaks = c(0.3, 0.7),
                                             out_dir = NULL,
                                             append_out_dir = TRUE
  )

  risk_summary_df <- overall_risk_summary$overall_pkg_scores %>%
    tibble::as_tibble() %>%
    mutate(
      pkg_name_ver = basename(.data$out_dir), # Used for joining covr data
      has_mitigation = ifelse(is.na(.data$mitigation), "no", "yes"),
      check_output_path = get_result_path(.data$out_dir, "check.rds"),
      covr_output_path = get_result_path(.data$out_dir, "covr.rds")
    ) %>%
    dplyr::select(-c("mitigation", "overall_risk"))


  frmt_check_attr <- function(attr){
    attr <- if(rlang::is_empty(attr)) "" else attr
    as.list(attr)
  }


  check_results <- purrr::map(risk_summary_df$check_output_path, ~{
    output <- readRDS(.x)

    tibble::tibble(
      package = output$package,
      version = output$version,
      check_status = output$status,
      check_test_fail = frmt_check_attr(output$test_fail),
      check_test_output = frmt_check_attr(output$test_output),
      check_errors = frmt_check_attr(output$errors),
      check_warnings = frmt_check_attr(output$warnings)
    )
  }) %>% purrr::list_rbind()


  covr_results <- purrr::map(risk_summary_df$covr_output_path, ~{
    output <- readRDS(.x)
    covr_success <-  ifelse(is.na(output$coverage$totalcoverage), FALSE, TRUE)
    covr_test_fail <- if(isFALSE(covr_success)) output$errors$message else ""

    tibble::tibble(
      pkg_name_ver = output$name,
      covr_success = covr_success,
      covr_test_fail = frmt_check_attr(covr_test_fail)
    )
  }) %>% purrr::list_rbind()


    risk_summary_df <- risk_summary_df %>%
      dplyr::left_join(check_results, by = join_by("package", "version")) %>%
      dplyr::left_join(covr_results, by = join_by("pkg_name_ver")) %>%
      dplyr::select(-"pkg_name_ver")


  if(isFALSE(keep_result_paths)){
    risk_summary_df <- risk_summary_df %>% dplyr::select(-c("out_dir", "check_output_path", "covr_output_path"))
  }else{
    risk_summary_df <- risk_summary_df %>% dplyr::relocate(c("out_dir", "check_output_path", "covr_output_path"), .after = dplyr::everything())
  }

  return(risk_summary_df)
}
