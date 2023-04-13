#' Take a JSON from score_pkg() and render a pdf
#'
#' @param json_path Path to a JSON file created by [score_pkg()]
#' @param risk_breaks A numeric vector of length 2, with both numbers being
#'   between 0 and 1. These are used for the "breaks" when classifying scores
#'   into Risk categories. For example, for the default value of `c(0.3, 0.7)`,
#'   for a given score: `0 <= score < 0.3` is "Low Risk", `0.3 <= score < 0.7`
#'   is "Medium Risk", and `0.7 <= score < 1` is "High Risk".
#' @param mitigation (Optional) path to a plain text file with any explanation
#'   necessary for justifying use of a potentially "high risk" package.
#' @param overwrite Logical (T/F). If `TRUE`, will overwrite an existing file path if it exists
#' @export
render_scorecard <- function(
    json_path, # should this just be a package name? we name the JSON ourselves, so we can infer the path
    risk_breaks = c(0.3, 0.7),
    mitigation = NULL,
    overwrite = FALSE
) {
  # input checking
  checkmate::assert_string(json_path)
  checkmate::assert_file_exists(json_path)
  checkmate::assert_string(mitigation, null.ok = TRUE)
  checkmate::assert_numeric(risk_breaks, lower = 0, upper = 1, len = 2)

  # load scores from JSON
  pkg_scores <- jsonlite::fromJSON(json_path)

  checkmate::assert_string(pkg_scores$out_dir, null.ok = TRUE)


  out_file <- get_result_path(pkg_scores$out_dir, "scorecard.pdf")
  check_exists_and_overwrite(out_file, overwrite)

  # map scores to risk and format into tables to be written to PDF
  formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks)

  # mitigation (if any)
  if(!is.null(mitigation)){
    mitigation_block <- readLines(mitigation)
  }else{
    mitigation_block <- NULL
  }

  rendered_file <- rmarkdown::render(
    system.file(SCORECARD_RMD_TEMPLATE, package = "mpn.scorecard"), # TODO: do we want to expose this to users, to pass their own custom template?
    output_dir = pkg_scores$out_dir,
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
#' @param pkg_scores named list of all scores and risks
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
  }) %>% purrr::list_rbind()


  overall_risk <- purrr::imap(pkg_scores$overall$risk, ~{
    data.frame(
      Category = ifelse(.y=="weighted_risk", "overall", .y),
      risk_score = ifelse(.x == "NA", NA_integer_, .x)
    )
  }) %>% purrr::list_rbind() %>%
    dplyr::mutate(Risk = map_risk(.data$risk_score, risk_breaks)) %>%
    dplyr::select(-"risk_score")

   pkg_scores$formatted$overall_risk <- dplyr::left_join(overall_scores, overall_risk) %>%
     dplyr::rename(`Weighted Score` = weighted_score)

  # TODO: map riskmetric categories to human-readable names, and 1/0 to Yes/No
  pkg_scores$formatted$scores <- purrr::imap(pkg_scores$scores, function(category_list, category_name) {
    formatted_df <- purrr::imap(category_list, ~{
      data.frame(
        Criteria = .y,
        raw_score = ifelse(.x == "NA", NA_integer_, .x)
      ) %>%
        mutate(
          Result = map_answer(.data$raw_score),
          Risk = map_risk(.data$raw_score, risk_breaks, desc = TRUE)
        )
    }) %>%
      purrr::set_names(names(category_list)) %>%
      purrr::list_rbind()

    # Additional formatting for testing data
    if(category_name == "testing"){
      formatted_df <- formatted_df %>% mutate(
          Result = ifelse(
            (Criteria == "covr" & !is.na(raw_score)), paste0(raw_score*100, "%"), Result
          ),
          Criteria = ifelse(.data$Criteria == "check", "R CMD CHECK passing", "Coverage")
        )
    }

    formatted_df <- formatted_df %>% dplyr::rename(`Raw Score` = raw_score)

    formatted_df
  })

  return(pkg_scores)
}

#' Use risk_breaks to map scores into character strings
#'
#' @param scores vector of risk scores (or other parameter if `desc = TRUE`)
#' @param risk_breaks breaks determining low, medium, and high risk
#' @param desc Logical (T/F). If `TRUE`, sort `risk_breaks` in descending order.
#'
#' @details
#' Use `desc = TRUE` for color coding coverage and other parameters that follow and inverse relationship with risk.
#'
#' @keywords internal
map_risk <- function(scores, risk_breaks, desc = FALSE) {
  checkmate::assert_numeric(scores, lower = 0, upper = 1)
  if(isTRUE(desc)){
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
  }else{
    risk_breaks <- sort(risk_breaks)
    purrr::map_chr(scores, ~ {
      if(is.na(.x)) {
        "Blocking"
      } else if (.x < risk_breaks[1]) {
        "Low Risk"
      } else if (.x >= risk_breaks[1] && .x <= risk_breaks[2]) {
        "Medium Risk"
      } else if(.x > risk_breaks[2]) {
        "High Risk"
      } else {
        "NA - unexpected"
      }
    })
  }
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

