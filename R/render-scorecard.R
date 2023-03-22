#' Take a JSON from score_pkg() and render a pdf
#'
#' @param json_path Path to a JSON file created by [score_pkg()]
#' @param out_dir Path to output directory
#' @param risk_breaks A numeric vector of length 2, with both numbers being
#'   between 0 and 1. These are used for the "breaks" when classifying scores
#'   into Risk categories. For example, for the default value of `c(0.3, 0.7)`,
#'   for a given score: `0 <= score < 0.3` is "Low Risk", `0.3 <= score < 0.7`
#'   is "Medium Risk", and `0.7 <= score < 1` is "High Risk".
#' @param mitigation (Optional) path to a plain text file with any explanation
#'   necessary for justifying use of a potentially "high risk" package.
#' @export
render_scorecard <- function(
    json_path, # should this just be a package name? we name the JSON ourselves, so we can infer the path
    out_dir,
    risk_breaks = c(0.3, 0.7),
    mitigation = NULL,
    overwrite = FALSE
) {
  # input checking
  checkmate::assert_string(json_path)
  checkmate::assert_string(out_dir)
  checkmate::assert_string(mitigation, null.ok = TRUE)
  checkmate::assert_numeric(risk_breaks, lower = 0, upper = 1, len = 2)

  # load scores from JSON
  pkg_scores <- jsonlite::fromJSON(json_path)

  out_file <- paste0(pkg_scores$pkg_name, ".scorecard.pdf")
  out_path <- file.path(out_dir, out_file)
  check_exists_and_overwrite(out_path, overwrite)

  # map scores to risk and format into tables to be written to PDF
  formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks)

  rmarkdown::render(
    SCORECARD_RMD_TEMPLATE, # TODO: do we want to expose this to users, to pass their own custom template?
    output_dir = out_dir,
    output_file = out_file,
    quiet = TRUE,
    params = list(pkg_scores = formatted_pkg_scores)
  )

  # Render to PDF, invisibly return the path to the PDF

}


#' Prepare the raw scores to be rendered into PDF
format_scores_for_render <- function(pkg_scores, risk_breaks) {

  # build list of formatted tibbles
  pkg_scores$formatted <- list()

  pkg_scores$formatted$overall <- purrr::imap(pkg_scores$overall, ~{
    data.frame(
      Category = .y,
      risk_score = .x
    )
  }) %>% purrr::list_rbind() %>%
    dplyr::mutate(Risk = map_risk(.data$risk_score, risk_breaks)) %>%
    dplyr::select(-"risk_score")

  # TODO: map riskmetric categories to human-readable names, and 1/0 to Yes/No
  pkg_scores$formatted$scores <- purrr::map(pkg_scores$scores, function(category_list) {
    purrr::imap(category_list, ~{
      data.frame(
        Criteria = .y,
        Result = .x
      )
    }) %>%
      purrr::set_names(names(category_list)) %>%
      purrr::list_rbind()
  })

  return(pkg_scores)
}

#' Use risk_breaks to map scores into character strings
map_risk <- function(scores, risk_breaks) {
  checkmate::assert_numeric(scores, lower = 0, upper = 1)
  risk_breaks <- sort(risk_breaks)
  purrr::map_chr(scores, ~ {
    if (.x < risk_breaks[1]) {
      "Low Risk"
    } else if (.x < risk_breaks[2]) {
      "Medium Risk"
    } else {
      "High Risk"
    }
  })
}


#' #' Use risk_breaks to map scores into character strings
#' map_categories <- function(scores, risk_breaks) {
#'   checkmate::assert_numeric(scores, lower = 0, upper = 1)
#'   risk_breaks <- sort(risk_breaks)
#'   purrr::map_chr(scores, ~ {
#'     if (.x < risk_breaks[1]) {
#'       "Low"
#'     } else if (.x < risk_breaks[2]) {
#'       "Medium"
#'     } else {
#'       "High"
#'     }
#'   })
#' }
