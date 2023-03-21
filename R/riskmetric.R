

#' Format riskmetric results into scorecard list
create_score_list_from_riskmetric <- function(pkgpath) {
  risk_res <- pkg_riskmetric(pkgpath)

  # TODO: get name and version _not_ from riskmetric
  # so that we can a) be independent and b) put this at the top
  res <- list(
    pkg_name = risk_res$ref$name,
    pkg_version = as.character(risk_res$ref$version),
    # for results
    testing = list(),
    documentation = list(),
    maintenance = list(),
    transparency = list()
  )

  # add riskmetric outputs
  res$documentation <- extract_score_list(
    risk_res,
    c("has_vignettes", "has_website", "has_news") #, export_help)
  )

  res$maintenance <- extract_score_list(
    risk_res,
    c("has_maintainer", "news_current", "last_30_bugs_status")
  )

  res$transparency <- extract_score_list(
    risk_res,
    c("has_source_control", "has_bug_reports_url")
  )

  return(res)
}


#' Run all relevant riskmetric checks
#'
#' and return raw riskmetric outputs
pkg_riskmetric <- function(pkg) {

  pref <- riskmetric::pkg_ref(pkg)

  passess <- riskmetric::pkg_assess(
    pref,
    assessments = list(
      #riskmetric::assess_export_help, # we should have this, see below
      riskmetric::assess_has_bug_reports_url,
      riskmetric::assess_last_30_bugs_status, # this could be good, but hits the API so maybe not...
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
                 })) %>% dplyr::rename(!!basename(pkg) := value)
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