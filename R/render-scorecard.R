#' Take a JSON from score_pkg() and render a pdf
#'
#' @param results_dir directory containing json file and individual results. Output file path from [score_pkg()]
#' @param risk_breaks A numeric vector of length 2, with both numbers being
#'   between 0 and 1. These are used for the "breaks" when classifying scores
#'   into Risk categories. For example, for the default value of `c(0.3, 0.7)`,
#'   for a given score: `0 <= score < 0.3` is "Low Risk", `0.3 <= score < 0.7`
#'   is "Medium Risk", and `0.7 <= score < 1` is "High Risk".
#' @param overwrite Logical (T/F). If `TRUE`, will overwrite an existing file path if it exists
#'
#' @details
#'
#' If a plain text mitigation file is found in `results_dir`, it will automatically be included.
#' **Note** that it must follow the naming convention of `<pkg_name>_<pkg_version>.mitigation.txt`
#'
#' A mitigation file includes any explanation necessary for justifying use of a potentially "high risk" package.
#'
#' @export
render_scorecard <- function(
    results_dir,
    risk_breaks = c(0.3, 0.7),
    overwrite = FALSE
) {

  json_path <- get_result_path(results_dir, "scorecard.json")

  # input checking
  checkmate::assert_string(json_path)
  checkmate::assert_file_exists(json_path)
  checkmate::assert_numeric(risk_breaks, lower = 0, upper = 1, len = 2)

  # load scores from JSON
  pkg_scores <- jsonlite::fromJSON(json_path)

  # map scores to risk and format into tables to be written to PDF
  formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks)

  mitigation_block <- check_for_mitigation(results_dir)

  # Output file
  checkmate::assert_string(results_dir, null.ok = TRUE)
  out_file <- get_result_path(results_dir, "scorecard.pdf")
  check_exists_and_overwrite(out_file, overwrite)

  # Render rmarkdown
  rendered_file <- rmarkdown::render(
    system.file(SCORECARD_RMD_TEMPLATE, package = "mpn.scorecard"), # TODO: do we want to expose this to users, to pass their own custom template?
    output_dir = results_dir,
    output_file = basename(out_file),
    quiet = TRUE,
    params = list(
      pkg_scores = formatted_pkg_scores,
      mitigation_block = mitigation_block,
      risk_breaks = risk_breaks,
      set_title = paste("Scorecard:", pkg_scores$pkg_name, pkg_scores$pkg_version)
    )
  )

  # Render to PDF, invisibly return the path to the PDF
  return(invisible(rendered_file))

}


#' Prepare the raw risk scores to be rendered into PDF
#'
#' Formats the risks, not the original scores
#'
#'
#' @param pkg_scores named list of all scores
#' @inheritParams render_scorecard
#'
#' @keywords internal
format_scores_for_render <- function(pkg_scores, risk_breaks = c(0.3, 0.7)) {

  # build list of formatted tibbles
  pkg_scores$formatted <- list()

  overall_scores <- purrr::imap(pkg_scores$overall$scores, ~{
    data.frame(
      Category = ifelse(.y=="weighted_score", "overall", .y),
      weighted_score = ifelse(.x == "NA", NA_integer_, .x)
    )
  }) %>% purrr::list_rbind() %>%
    dplyr::mutate(Risk = map_risk(.data$weighted_score, risk_breaks))

  pkg_scores$formatted$overall_risk <- overall_scores %>%
    dplyr::rename(`Weighted Score` = "weighted_score")

  # TODO: map riskmetric categories to human-readable names, and 1/0 to Yes/No
  pkg_scores$formatted$scores <- purrr::imap(pkg_scores$scores, function(category_list, category_name) {
    formatted_df <- purrr::imap(category_list, ~{
      data.frame(
        Criteria = .y,
        raw_score = ifelse(.x == "NA", NA_integer_, .x)
      ) %>%
        mutate(
          Result = map_answer(.data$raw_score),
          Risk = map_risk(.data$raw_score, risk_breaks)
        )
    }) %>% purrr::list_rbind()

    # Additional formatting for testing data
    if(category_name == "testing"){
      formatted_df <- formatted_df %>% mutate(
        Result = ifelse(
          (.data$Criteria == "covr" & !is.na(.data$raw_score)), paste0(.data$raw_score*100, "%"), .data$Result
        ),
        Criteria = ifelse(.data$Criteria == "check", "R CMD CHECK passing", "Coverage")
      )
    }

    formatted_df <- formatted_df %>% dplyr::rename(`Raw Score` = "raw_score")

    formatted_df
  })

  return(pkg_scores)
}

#' Use risk_breaks to map scores into character strings
#'
#' @param scores vector of risk scores (or other parameter if `desc = TRUE`)
#' @param risk_breaks breaks determining low, medium, and high risk
#'
#' @keywords internal
map_risk <- function(scores, risk_breaks) {
  checkmate::assert_numeric(scores, lower = 0, upper = 1)
  risk_breaks <- sort(risk_breaks, decreasing = TRUE)
  purrr::map_chr(scores, ~ {
    if(is.na(.x)) {
      "Blocking"
    } else if (.x > risk_breaks[1]) {
      "Low Risk"
    } else if (.x <= risk_breaks[1] && .x >= risk_breaks[2]) {
      "Medium Risk"
    } else if(.x < risk_breaks[2]) {
      "High Risk"
    } else {
      "NA - unexpected"
    }
  })
}

#' Use answer_breaks to map binary results into character strings
#'
#' @param scores vector of risk scores (or other parameter if `desc = TRUE`)
#' @param answer_breaks breaks determining 'Yes' or 'No'
#'
#' @details
#' If value is not found in `answer_breaks`, it is skipped over
#'
#' Note: A result of 0.5 indicates an answer of 'Yes' with warnings. This is a special case for rmdcheck
#'
#' @keywords internal
map_answer <- function(results, answer_breaks = c(0, 0.5, 1)) {
  checkmate::assert_numeric(results, lower = 0, upper = 1)
  answer_breaks <- sort(answer_breaks)
  purrr::map_chr(results, ~ {
    if(is.na(.x)) {
      "Failed"
    } else if (.x == answer_breaks[1]) {
      "No"
    } else if(.x == answer_breaks[2]) {
      "Yes (warnings occurred)"
    } else if(.x == answer_breaks[3]) {
      "Yes"
    } else{
      as.character(.x)
    }
  })
}


#' Look for mitigation file and return contents if any are found
#'
#' @inheritParams render_scorecard
#'
#' @keywords internal
check_for_mitigation <- function(results_dir){
  # mitigation (if any)
  # infer mitigation path from `results_dir`
  mitigation_path <- get_result_path(results_dir, "mitigation.txt")
  if(fs::file_exists(mitigation_path)){
    mitigation_block <- readLines(mitigation_path)
  }else{
    mitigation_block <- NULL
  }
  return(mitigation_block)
}
