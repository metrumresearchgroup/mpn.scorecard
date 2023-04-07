#' Autofit and format flextables
#'
#'
#' @param tab a dataframe or flextable object. Must be coercible to a dataframe
#' @param autofit logical (T/F). Whether or not to autofit the table.
#' @param pg_width width (in inches) of the table. Generally 1 inch less than the default word document (8 in.)
#' @param column_width named vector, where the column names are assigned to the desired _relative_ width.
#'        If specified, set these column widths before fitting to word document
#' @param doc_type Word, PDF, or HTML. Controls font size
#' @param as_flextable logical (T/F). if `TRUE`, use `as_flextable` instead of `flextable`, which has different args and is necessary for grouped data
#' @param ... additional args to be passed to `as_flextable` or `flextable`
#'
#' @details
#' column_width is specified using the following convention:
#'
#' ```
#' tab %>%
#' flextable_word(column_width = c("col1" = 2, "col2" = 3))
#' ```
#'
#' `flextable` by default will make the tables as wide as possible in word. This function will correct the `autofit()` feature and make the contents fit.
#' Other formatting corrections are done depending on the specified `doc_type`
#'
#' @importFrom flextable flextable_dim width fontsize
#' @importFrom checkmate assert_true assert_character assert_numeric
#'
#' @return a formatted flextable
#'
#' @keywords internal
flextable_formatted <- function(tab,
                                autofit = TRUE,
                                pg_width = 5,
                                column_width = NULL,
                                doc_type = "PDF",
                                as_flextable = TRUE,
                                ...){

  if(isTRUE(as_flextable)){
    tab_out <- tab %>% flextable::as_flextable(...) #%>%
    # flextable::theme_vanilla()
  }else{
    tab_out <- tab %>% flextable::flextable(...) #%>%
    # flextable::theme_vanilla()
  }


  if(isTRUE(autofit)){
    tab_out <- tab_out %>% flextable::autofit()
  }

  if(!is.null(column_width)){
    checkmate::assert_character(names(column_width))
    checkmate::assert_true(all(names(column_width) %in% names(tab)))
    checkmate::assert_numeric(column_width)
    tab_out <- flextable::width(tab_out, glue::glue("{names(column_width)}"), width = column_width)
  }

  font_size <- ifelse(doc_type == "Word", 8, 10)

  tab_out <- tab_out %>%
    flextable::fontsize(size = 10, part = "header") %>%
    flextable::fontsize(size = font_size, part = "body")

  if(isTRUE(autofit)){
    tab_out <- flextable::width(tab_out, width = dim(tab_out)$widths*pg_width /(flextable::flextable_dim(tab_out)$widths))
    # Word needs additional autofit formatting (HTML is ok with this, but PDF gets wacky)
    if(doc_type %in% c("Word", "HTML")){
      tab_out <- tab_out %>% flextable::set_table_properties(width = 1, layout = "autofit")
    }
  }

  return(tab_out)
}


#' Format Overall Scores
#'
#' @param formatted_pkg_scores list containing all scores
#'
#' @returns a formatted flextable object
#'
#' @keywords internal
format_overall_scores <- function(formatted_pkg_scores){
  # Create overall table
  overall_tbl <- formatted_pkg_scores$formatted$overall %>%
    mutate(
      Risk = factor(Risk, levels = c("High Risk", "Medium Risk", "Low Risk")),
      Category = stringr::str_to_title(Category)
    ) %>% as.data.frame()

  overall_flextable <-
    flextable_formatted(
      overall_tbl,
      show_coltype = FALSE
    ) %>%
    bg(bg = "#ffffff", part = "all") %>%
    align(align = "center", part = "all") %>%
    color(color = "black", part = "body") %>%
    color(color = "darkgreen", j = 2, i = ~ `Risk` == 'Low Risk') %>%
    color(color = "orange", j = 2, i = ~ `Risk` == 'Medium Risk') %>%
    color(color = "red", j = 2, i = ~ `Risk` == 'High Risk') %>%
    set_header_labels(Category = "Category", Risk = "Risk Level") %>%
    set_caption("Package Risk Metrics Summary")

  return(overall_flextable)
}


#' Retrieve and format category labels
#'
#' @param formatted_pkg_scores list containing all scores
#' @param category one of `c('testing', 'documentation', 'maintenance', 'transparency', 'overall')`
#'
#' @returns a character string in the format of <category>: <risk>
#'
#' @keywords internal
get_overall_labels <- function(formatted_pkg_scores, category){
  overall_df <- formatted_pkg_scores$formatted$overall
  checkmate::assert_true(category %in% unique(overall_df$Category))
  overall_df <- overall_df %>% mutate(
    category_label = paste0(stringr::str_to_title(Category), ": ", Risk)
  )
  cat_label <- overall_df$category_label[grep(category, overall_df$Category)]
  return(cat_label)
}


#' Format Package Details
#'
#' Formats Documentation, Maintenance and Transparency scores
#'
#' @param formatted_pkg_scores list containing all scores
#'
#' @returns a formatted flextable object
#'
#' @keywords internal
format_package_details <- function(formatted_pkg_scores){

  package_details_list <- formatted_pkg_scores$formatted$scores
  # Get the scores for each category
  # TODO: assign risk to each category label (i.e. Maintenance: Low Risk) Need to figure out weighting first
  maintenance_df <- package_details_list$maintenance %>% mutate(Category = "maintenance")
  transparency_df <- package_details_list$transparency %>% mutate(Category = "transparency")
  documentation_df <- package_details_list$documentation %>% mutate(Category = "documentation")

  # Combine the data frames into one. Risk column is for styling only (not displayed),
  # and is necessary because of the environment flextable uses (cant reference outside objects)
  scores_df <- rbind(maintenance_df, transparency_df, documentation_df) %>%
    dplyr::mutate(
      Result = map_answer(.data$Result),
      Category = purrr::map_chr(Category, ~ get_overall_labels(formatted_pkg_scores, .x))
    )

  # Group by category
  scores_df <- as_grouped_data(scores_df, groups = "Category")

  category_scores_flextable <-
    flextable_formatted(
      scores_df,
      hide_grouplabel = TRUE
    ) %>%
    bg(bg = "#ffffff", part = "all") %>%
    align(align = "center", part = "all") %>%
    color(color = "black", part = "body") %>%
    color(color = "darkgreen", j = 2, i = ~ Result == "Yes") %>%
    color(color = "darkred", j = 2, i = ~ Result == "No") %>%
    align(i = ~ !is.na(Category), align = "center") %>%
    bold(i = ~ !is.na(Category)) %>%
    set_caption("Package Details")

  return(category_scores_flextable)
}


#' Format testing scores
#'
#' @param testing_df dataframe returned by `formatted_pkg_scores$formatted$scores$testing`
#'
#' @returns a formatted flextable object
#'
#' @keywords internal
format_testing_scores <- function(testing_df){
  testing_df <- testing_df %>%
    dplyr::mutate(
      Risk = map_risk(.data$Result, risk_breaks, desc = TRUE),
      Result = dplyr::case_when(
        Criteria == "covr" ~ paste0(Result*100, "%"),
        Result %in% c(0, 0.5, 1) ~ map_answer(.data$Result)
      ),
      Criteria = ifelse(Criteria == "check", "R CMD CHECK passing", "Coverage")
    )

  testing_scores_flextable <-
    flextable_formatted(
      testing_df,
      as_flextable = FALSE,
      col_keys = c("Criteria", "Result")
    ) %>%
    bg(bg = "#ffffff", part = "all") %>%
    align(align = "center", part = "all") %>%
    color(color = "black", part = "body") %>%
    color(color = "darkgreen", j = 2, i = ~ Risk == "Low Risk") %>%
    color(color = "orange", j = 2, i = ~ Risk == "Medium Risk") %>%
    color(color = "darkred", j = 2, i = ~ Risk == "High Risk") %>%
    set_caption(paste("Testing:", unique(testing_df$Risk)))

  return(testing_scores_flextable)
}



#' Format system info and metadata
#'
#' @param metadata_list metadata list returned by `formatted_pkg_scores$metadata`
#'
#' @returns a formatted flextable object
#'
#' @keywords internal
format_metadata <- function(metadata_list){
  # Create system info table
  executor_tbl <- data.frame(executor = metadata_list$executor)
  data_tbl <- data.frame(date = metadata_list$date)
  env_vars_tbl <- as.data.frame(t(unlist(metadata_list$info$env_vars)))
  system_info_tbl <- as.data.frame(t(unlist(metadata_list$info$sys)))


  all_info_tbl <- cbind(data_tbl, executor_tbl, system_info_tbl, env_vars_tbl)
  all_info_tbl <- data.frame(Category = stringr::str_to_title(names(all_info_tbl)),
                             Value = unlist(all_info_tbl))

  # Create flextable
  system_info_flextable <-
    flextable_formatted(
      all_info_tbl,
      show_coltype = FALSE
    ) %>%
    bg(bg = "#ffffff", part = "all") %>%
    align(align = "center", part = "all") %>%
    color(color = "black", part = "body") %>%
    set_header_labels(Category = "Category", Value = "Value") %>%
    set_caption("System Information")

  return(system_info_flextable)
}
