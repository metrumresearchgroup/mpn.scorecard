#' Take a JSON from score_pkg() and render a pdf
#'
#' @param results_dir directory containing json file and individual results. Output file path from [score_pkg()]
#' @param risk_breaks A numeric vector of length 2, with both numbers being
#'   between 0 and 1. These are used for the "breaks" when classifying scores
#'   into Risk categories. For example, for the default value of `c(0.3, 0.7)`,
#'   for a given score: `0 <= score < 0.3` is "Low Risk", `0.3 <= score < 0.7`
#'   is "Medium Risk", and `0.7 <= score < 1` is "High Risk".
#' @param overwrite Logical (T/F). If `TRUE`, will overwrite an existing file path if it exists
#' @param add_traceability Logical (T/F). If `TRUE`, append a table that links package functionality to the documentation and test files.
#' Defaults to "auto", which will include the matrix if found.
#'
#' @details
#'
#' If a plain text comments file is found in `results_dir`, it will automatically be included.
#' **Note** that it must follow the naming convention of `<pkg_name>_<pkg_version>.comments.txt`
#'
#' If a traceability matrix is found in `results_dir`, it will automatically be included unless overridden via `add_traceability`.
#' **Note** that it must follow the naming convention of `<pkg_name>_<pkg_version>.export_doc.rds`
#'
#' @export
render_scorecard <- function(
    results_dir,
    risk_breaks = c(0.3, 0.7),
    overwrite = FALSE,
    add_traceability = "auto"
) {

  json_path <- get_result_path(results_dir, "scorecard.json")

  # input checking
  checkmate::assert_string(json_path)
  checkmate::assert_file_exists(json_path)
  checkmate::assert_numeric(risk_breaks, lower = 0, upper = 1, len = 2)

  # load scores from JSON
  pkg_scores <- jsonlite::fromJSON(json_path)
  check_scores_valid(pkg_scores, json_path)

  # map scores to risk and format into tables to be written to PDF
  formatted_pkg_scores <- format_scores_for_render(pkg_scores, risk_breaks)

  comments_block <- check_for_comments(results_dir)

  # Output file
  checkmate::assert_string(results_dir, null.ok = TRUE)
  out_file <- get_result_path(results_dir, "scorecard.pdf")
  check_exists_and_overwrite(out_file, overwrite)


  # Appendix
  extra_notes_data <- create_extra_notes(results_dir)

  # Traceability Matrix
  exports_df <- check_for_traceability(results_dir, add_traceability)

  dep_versions_df <- get_dependency_versions(results_dir, pkg_scores$pkg_name)

  # mpn.scorecard version
  mpn_scorecard_ver <- format_scorecard_version(
    pkg_scores$mpn_scorecard_version,
    as.character(utils::packageVersion("mpn.scorecard"))
  )

  # Render rmarkdown
  rendered_file <- rmarkdown::render(
    system.file(SCORECARD_RMD_TEMPLATE, package = "mpn.scorecard", mustWork = TRUE), # TODO: do we want to expose this to users, to pass their own custom template?
    output_dir = results_dir,
    output_file = basename(out_file),
    quiet = TRUE,
    params = list(
      set_title = paste("Scorecard:", pkg_scores$pkg_name, pkg_scores$pkg_version),
      scorecard_footer = mpn_scorecard_ver,
      pkg_scores = formatted_pkg_scores,
      comments_block = comments_block,
      extra_notes_data = extra_notes_data,
      exports_df = exports_df,
      dep_versions_df = dep_versions_df
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
    mutate(risk = map_risk(.data$category_score, risk_breaks))

  pkg_scores$formatted$overall_scores <- overall_scores

  # map riskmetric categories
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
    if(is.na(.x) || .x < risk_breaks[2]) {
      "High Risk"
    } else if (.x > risk_breaks[1]) {
      "Low Risk"
    } else if (.x <= risk_breaks[1] && .x >= risk_breaks[2]) {
      "Medium Risk"
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


#' Look for comment file and return contents if is found
#'
#' @inheritParams render_scorecard
#'
#' @keywords internal
check_for_comments <- function(results_dir){

  # Implementation function for looking for comment file
  check_for_comments_impl <- function(r_dir, ext){
    # infer comments path from `results_dir`
    comments_path <- get_result_path(r_dir, ext)
    if(fs::file_exists(comments_path)){
      comments_block <- readLines(comments_path)
    }else{
      comments_block <- NULL
    }
    return(comments_block)
  }

  comment_block <- check_for_comments_impl(results_dir, "comments.txt")

  # deprecated as of 0.3.0, but still supported
  mitigation_block <- check_for_comments_impl(results_dir, "mitigation.txt")
  if(!is.null(mitigation_block)){

    if(is.null(comment_block)) {
      details_msg <- paste(
        "Including contents of", glue("`{basename(results_dir)}.mitigation.txt`."),
        "Please rename to", glue("`{basename(results_dir)}.comments.txt` for future usage.")
      )
    } else {
      details_msg <- paste(
        "Including contents of", glue("`{basename(results_dir)}.comments.txt`"),
        "and ignoring", glue("`{basename(results_dir)}.mitigation.txt`.")
      )
    }

    deprecate_warning(
      version = "0.3.0",
      what = "Using `mitigation.txt` extensions for including comments",
      details = details_msg
    )
  }

  # Return comments (if any)
  if(is.null(comment_block) && !is.null(mitigation_block)){
    return(mitigation_block)
  }else{
    return(comment_block)
  }

}


#' Look for Traceability matrix RDS file and return contents if is found
#'
#' @inheritParams render_scorecard
#'
#' @keywords internal
check_for_traceability <- function(results_dir, add_traceability){
  checkmate::assert_true(add_traceability == "auto" || is.logical(add_traceability))
  # infer traceability matrix path from `results_dir`
  trac_path <- get_result_path(results_dir, "export_doc.rds")
  if(fs::file_exists(trac_path) && !isFALSE(add_traceability)){
    trac_mat <- readRDS(trac_path)
  }else{
    if(isTRUE(add_traceability)){
      stop(glue("No traceability matrix found at {trac_path}"))
    }
    trac_mat <- NULL
  }
  return(trac_mat)
}

#' Extract dependency versions from rcmdcheck result
#'
#' @param pkg_name Name of package being scored.
#' @return A data frame with two columns, "package" and "version". This data
#'   frame may be empty if the package has no hard dependencies. If the session
#'   info `$packages` data frame is unpopulated due to a failure, return `NULL`.
#' @noRd
get_dependency_versions <- function(results_dir, pkg_name) {
  res <- readRDS(get_result_path(results_dir, "check.rds"))
  si <- res$session_info
  if (is.null(si)) {
    stop("rcmdcheck result unexpectedly missing session info.")
  }
  pkgs <- si$packages
  if (is.null(pkgs)) {
    stop("Session info unexpectedly missing $packages data frame.")
  }

  if (!pkg_name %in% rownames(pkgs)) {
    stop("packages_info object unexpectedly missing row for scored package.")
  }
  # If 'R CMD check' failed before installing the package being scored (e.g.,
  # with a "Package suggested but not available" failure),
  # $session_info$packages is data frame with one row for the package and NA for
  # all columns aside from "package".
  if (nrow(pkgs) == 1 && is.na(pkgs[pkg_name, "ondiskversion"])) {
    return(NULL)
  }

  na_vers <- is.na(pkgs$ondiskversion)
  if (any(na_vers)) {
    stop(
      "Some packages unexpectedly have an NA version: ",
      paste(pkgs$package[na_vers], collapse = ", ")
    )
  }

  rownames(pkgs) <- NULL
  # The sessioninfo$package_info() result includes the query package, but we
  # just want the dependencies.
  dplyr::select(pkgs, c("package", version = "ondiskversion")) %>%
    dplyr::filter(.data$package != pkg_name)
}

#' Format mpn.scorecard version as a footer note
#'
#' Formats mpn.scorecard version as a footer note for use in `render_scorecard`
#' or `render_scorecard_summary`
#'
#' @param json_ver version of mpn.scorecard as recorded in `score_pkg`.
#' Set to `NULL` for use with `render_scorecard_summary`.
#' @param scorecard_ver version of mpn.scorecard as recorded in `render_scorecard`
#' or `render_scorecard_summary`
#'
#' @keywords internal
format_scorecard_version <- function(json_ver = NULL, scorecard_ver){
  scorecard_version <-
    if(is.null(json_ver) || identical(json_ver, scorecard_ver)){
      glue::glue("Generated with mpn.scorecard {scorecard_ver}")
    }else{
      glue::glue("Package scored with mpn.scorecard {json_ver},
               document generated with mpn.scorecard {scorecard_ver}")
    }

  # Latex line breaks dont work with fancyfoot - remove for now
  as.character(scorecard_version) %>% gsub("\n", "", .)
}
