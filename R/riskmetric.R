

#' Format riskmetric results into scorecard list
#'
#' @param res a named list containing the initial build up of score elements
#' @param pkg_source_path path to package source code (untarred)
#'
#' @keywords internal
create_score_list_from_riskmetric <- function(res, pkg_source_path) {
  risk_res <- pkg_riskmetric(pkg_source_path)

  # add riskmetric outputs
  res$scores$documentation <- extract_score_list(
    risk_res,
    DOCUMENTATION_METRICS
  )

  res$scores$maintenance <- extract_score_list(
    risk_res,
    MAINTENANCE_METRICS
  )

  res$scores$transparency <- extract_score_list(
    risk_res,
    TRANSPARENCY_METRICS
  )

  return(res)
}


#' Run all relevant riskmetric checks
#'
#' @inheritParams create_score_list_from_riskmetric
#'
#' @returns raw riskmetric outputs
#' @keywords internal
pkg_riskmetric <- function(pkg_source_path) {

  pref <- riskmetric::pkg_ref(pkg_source_path)

  passess <- riskmetric::pkg_assess(
    pref,
    assessments = list(
      #riskmetric::assess_export_help, # we should have this, see below
      riskmetric::assess_has_bug_reports_url,
      #riskmetric::assess_last_30_bugs_status, # probably want this, but need to decide how to handle NAs. Also hits the API so maybe not worth it...
      riskmetric::assess_has_maintainer,
      riskmetric::assess_has_news,
      riskmetric::assess_has_source_control, # R/pkg_ref_cache_source_control_url.R
      riskmetric::assess_has_vignettes,
      riskmetric::assess_has_website,
      #riskmetric::assess_license, # seems like a good idea, but not implemented yet
      riskmetric::assess_news_current
    )
  )

  # export_help seems like something we should really include, but they haven't implemented yet:
  ## https://github.com/pharmaR/riskmetric/blob/master/R/assess_export_help.R#L28
  #
  # we could implement, either internally or as a PR to them
  # basically do percentage of exported functions that have an associated help entry
  # they already give you a mechanism for counting exported functions: assess_exported_namespace()
  # we probably just need to loop over the man/ dir and look at all the names and aliases

  pscore <- riskmetric::pkg_score(passess)

  return(list(ref = pref, assess = passess, score = pscore))
}

# create a tibble of risk metric scores
# no real purpose right now...
riskmetric_tibble <- function(pkg_risk) {
  if (inherits(pkg_risk, "character")) {
    pkg_risk <- pkg_riskmetric(pkg_risk)
  }
  res_list <- pkg_risk$score
  tibble::tibble(score = names(res_list),
                 value = purrr::map_dbl(unname(res_list), ~ {
                   attributes(.x) <- NULL
                   .x
                 })) %>% dplyr::rename(!!basename(.data$pkg) := "value")
}

# extract score value from riskmetric score object
extract_score_value <- function(risk_res, metric) {
  .s <- risk_res[["score"]][[metric]]
  attributes(.s) <- NULL
  .s
}


# extract list of score values from riskmetric score object
extract_score_list <- function(risk_res, metrics) {
  .sl <- purrr::map(metrics, ~{
    extract_score_value(risk_res, .x)
  })
  names(.sl) <- metrics
  return(.sl)
}


#' Check that package scores are valid
#'
#' Check that individual scores are retained and category scores are numeric.
#'
#' @details
#'
#' **Individual Score checks**
#'
#' The only checks performed on the individual scores are that they still exist
#' when written out to json. This would only *not* be the case if an individual
#' score was set to `NULL`, removing it from the list.
#'
#' **Category Score checks**
#'
#' `covr` scores are the only individual scores that allow for NA scores.
#' However when determining overall category scores, all values should be
#' coerced to numeric (0 in the case of `covr`). Any potential errors or
#' character values should be coerced to `NA` when passed through `mean()` in
#' `calc_overall_scores`.
#'
#' This function serves to confirm that all category scores are in fact numeric
#' values. The only way a non-numeric value could be anything *other than* `NA`,
#' is if `mean()` returned an error message or some other value. This scenario
#' would likely cause issues before writing the score list out to json, so the
#' additional `is.numeric` check is primarily a precautionary measure.
#'
#'
#' @param pkg_scores a named list containing the build up of score elements and
#' overall category scores.
#' @param json_path a JSON file path.
#'
#' @keywords internal
check_scores_valid <- function(pkg_scores, json_path){

  # Check that names are preserved at the end - confirms `NULL` is not returned
  # for individual scores
  score_names <- purrr::map(pkg_scores$scores, function(score_name){
    names(score_name)
  }) %>% purrr::flatten_chr() %>% sort()

  score_names_chk <- c(DOCUMENTATION_METRICS, MAINTENANCE_METRICS,
                       TRANSPARENCY_METRICS, TESTING_METRICS) %>% sort()

  if(!all(score_names_chk %in% score_names)){
    missing_scores <- setdiff(score_names_chk, score_names) %>%
      paste(collapse = ", ")
    abort(glue("The following categories were unintentionally removed:
               {missing_scores}"))
  }

  # Coerce "NA" character to NA
  category_scores <- purrr::map(pkg_scores$category_scores, function(cat_score){
    ifelse(cat_score == "NA", NA_integer_, cat_score)
  })

  # Check if any values are NA or are otherwise non-numeric
  error_cond <- is.na(category_scores) | (!sapply(category_scores,is.numeric))
  if(any(error_cond)){
    error_cats <- names(category_scores[error_cond]) %>% paste(collapse = ", ")
    abort(glue("The following categories returned `NA` or were otherwise non-numeric:
               {error_cats}
               Read in the json to see what went wrong: {json_path}"))
  }

}
