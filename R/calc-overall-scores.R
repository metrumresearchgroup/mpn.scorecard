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
  scorelist$overall <- list(scores = list(), risk = list())

  # TODO: write real calc_overall_scores() algorithm
  # this is where the real algorithm will go
  # all of this is just stand-in code to hook up the pipes
  categories <- names(scorelist$scores)
  scorelist$overall$scores <- purrr::map(categories, ~{
   round(mean(unlist(scorelist$scores[[.x]]), na.rm = TRUE), 3)
  }) %>% purrr::set_names(categories)

  scorelist$overall$risk <- purrr::map(categories, ~{
    1 - round(mean(unlist(scorelist$scores[[.x]]), na.rm = TRUE), 3)
  }) %>% purrr::set_names(categories)

  scorelist$overall$scores$weighted_score <- round(mean(unlist(scorelist$overall$scores), na.rm = TRUE), 3)
  scorelist$overall$risk$weighted_risk <- round(mean(unlist(scorelist$overall$risk), na.rm = TRUE), 3)
  ##### end stand-in code

  return(scorelist)

}
