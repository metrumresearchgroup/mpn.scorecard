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
    system.file(SCORECARD_RMD_TEMPLATE, package = "mpn.scorecard", mustWork = TRUE), # TODO: do we want to expose this to users, to pass their own custom template?
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

  overall_scores <- purrr::imap(pkg_scores$category_scores, ~{
    data.frame(
      category = .y,
      category_score = ifelse(.x == "NA", NA_integer_, .x)
    )
  }) %>% purrr::list_rbind() %>%
    dplyr::mutate(risk = map_risk(.data$category_score, risk_breaks))

  pkg_scores$formatted$overall_scores <- overall_scores

  # TODO: map riskmetric categories to human-readable names, and 1/0 to Yes/No
  pkg_scores$formatted$category_scores <- purrr::imap(pkg_scores$scores, function(category_list, category_name) {
    formatted_df <- purrr::imap(category_list, ~{
      data.frame(
        criteria = .y,
        score = ifelse(.x == "NA", NA_integer_, .x)
      ) %>%
        mutate(
          result = map_answer(.data$score, .data$criteria),
          risk = map_risk(.data$score, risk_breaks)
        )
    }) %>% purrr::list_rbind()

    # Additional formatting for testing data
    if(category_name == "testing"){
      formatted_df <- formatted_df %>% mutate(
        result = ifelse(
          (.data$criteria == "covr" & !is.na(.data$score)), paste0(.data$score*100, "%"), .data$result
        ),
        criteria = ifelse(.data$criteria == "check", "R CMD CHECK", "coverage")
      )
    }

    formatted_df
  })

  return(pkg_scores)
}

#' Use risk_breaks to map scores into character strings
#'
#' @param scores vector of risk scores
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

#' Use answer_breaks to map results into character strings
#'
#' @param scores vector of risk scores
#' @param criteria vector of criteria names
#' @param answer_breaks breaks determining 'Yes'/'Passing' or 'No'/'Failed'. `NA` has special handling. See details.
#'
#' @details
#' If value is not found in `answer_breaks`, it is skipped over
#'
#' Note: rmdcheck and covr are special cases
#' rmdcheck includes the raw score as part of the result, but a value of `answer_breaks[1]` (usually 0) vs `NA` are handled the same (both indicate failures)
#' covr is skipped over unless it is `NA` (indicates test failures), as this is formatted as a percent separately
#'
#' @keywords internal
map_answer <- function(scores, criteria, answer_breaks = c(0, 1)) {
  checkmate::assert_numeric(scores, lower = 0, upper = 1)
  answer_breaks <- sort(answer_breaks)
  purrr::map2_chr(scores, criteria, ~ {
    # special handling for R CMD CHECK
    if(.y == "check"){
      if(is.na(.x) || .x == answer_breaks[1]) {
        "Failed"
      } else {
        paste0("Passing (score: ", .x, ")")
      }
    }else if(.y != "covr"){
      if (.x == answer_breaks[1]) {
        "No"
      } else if(.x == answer_breaks[2]) {
        "Yes"
      } else{
        as.character(.x)
      }
    }else{
      if(is.na(.x)) {
        "Failed"
      } else {
        as.character(.x)
      }
    }
  })
}


#' Look for mitigation file and return contents if is found
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