#' Calculate overall category scores
#'
#' This is where we'll need to build the algorithm for weighting
#' different metrics, etc. We also need to consider if we want
#' this algorithm (i.e. the weights) to be configurable be the user...
#'
#' @param scorelist a complete named list of scores. Should contain all the elementary blocks and just need to be summarized
#'
#' @keywords internal
calc_overall_scores <- function(scorelist) {
  scorelist$category_scores <- list()

  categories <- names(scorelist$scores)
  scorelist$category_scores <- purrr::map(categories, ~{
    indiv_scores <- unlist(scorelist$scores[[.x]])
    # Penalize coverage failures: NA --> 0
    if ("coverage" %in% names(indiv_scores) && is.na(indiv_scores[["coverage"]])) {
      indiv_scores[["coverage"]] <- 0
    }
   round(mean(indiv_scores), 3)
  }) %>% purrr::set_names(categories)

  # Category weighting
  category_score_weighting <- c(testing = 0.4, documentation = 0.2, maintenance = 0.2, transparency = 0.2)
  # Ensure names are in the correct order
  weighted_category_scores <- category_score_weighting * unlist(scorelist$category_scores[names(category_score_weighting)])

  scorelist$category_scores$overall <- round(sum(weighted_category_scores), 3)

  return(scorelist)

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
#' `covr` scores are the only *individual* scores that allow for `NA` values.
#' However when determining overall *category* scores, all values should be
#' coerced to numeric, non-`NA` values (0 in the case of `covr`). Any potential
#' errors or character values should be coerced to `NA` when passed through
#' `mean()` in `calc_overall_scores`.
#'
#' This function serves to confirm that all category scores are in fact numeric,
#' non-`NA` values. The only way a category score could be anything
#' *other than* a valid score or `NA`, is if `mean()` returned an error message
#' or some other value. This scenario would likely cause issues before writing
#' the score list out to json, so the additional `is.numeric` check is primarily
#' a precautionary measure.
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
  }) %>% purrr::list_c()

  score_names_chk <- c(DOCUMENTATION_METRICS, MAINTENANCE_METRICS,
                       TRANSPARENCY_METRICS, TESTING_METRICS)

  if(!all(score_names_chk %in% score_names)){
    missing_scores <- setdiff(score_names_chk, score_names) %>%
      paste(collapse = ", ")
    abort(glue("The following categories were unintentionally removed:
               {missing_scores}"))
  }

  # Coerce "NA" character to NA
  category_scores <- purrr::map(pkg_scores$category_scores, function(cat_score){
    if(identical(cat_score, "NA")) NA_integer_ else cat_score
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
