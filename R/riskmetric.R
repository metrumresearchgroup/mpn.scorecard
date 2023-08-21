

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

