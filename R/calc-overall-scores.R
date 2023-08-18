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
    if("covr" %in% names(indiv_scores) && is.na(indiv_scores[["covr"]])){
      indiv_scores[["covr"]] <- 0
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
