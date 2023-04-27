
#' @importFrom tidyselect everything
NULL


SCORECARD_RMD_TEMPLATE <- file.path("templates", "scorecard-template.Rmd")

SUM_SCORECARD_RMD_TEMPLATE <- file.path("templates", "scorecard-summary-template.Rmd")


RISK_LEVELS <-  c("NA - unexpected","Blocking","High Risk", "Medium Risk", "Low Risk")

utils::globalVariables(c("."))
