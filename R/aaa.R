
#' @importFrom tidyselect everything all_of
NULL


SCORECARD_RMD_TEMPLATE <- file.path("templates", "scorecard-template.Rmd")

SUM_SCORECARD_RMD_TEMPLATE <- file.path("templates", "scorecard-summary-template.Rmd")


RISK_LEVELS <-  c("NA - unexpected", "High Risk", "Medium Risk", "Low Risk")

utils::globalVariables(c("."))

METRIC_CATEGORIES <- c("testing", "documentation", "maintenance", "transparency")

DOCUMENTATION_METRICS <- c("has_vignettes", "has_website", "has_news") #, export_help)
MAINTENANCE_METRICS <- c("has_maintainer", "news_current")#, "last_30_bugs_status")
TRANSPARENCY_METRICS <- c("has_source_control", "has_bug_reports_url")
TESTING_METRICS <- c("check", "coverage")

MPN_SCORECARD_FORMAT <- "1.0"

SCORECARD_JSON_KEYS <- c(
  "mpn_scorecard_format",
  "mpn_scorecard_version",
  "pkg_name",
  "pkg_version",
  "scorecard_type",
  "out_dir",
  "pkg_tar_path",
  "md5sum_check",
  "scores",
  "metadata",
  "category_scores"
)
