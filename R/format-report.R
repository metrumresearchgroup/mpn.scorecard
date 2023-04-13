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
#' @param digits numeric. Number of digits to round to. If `NULL`, and `as_flextable = TRUE`, flextable will round to one digit.
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
                                digits = NULL,
                                ...){

  # If flextable object already, just apply formatting
  if(!inherits(tab, "flextable")){
    if(isTRUE(as_flextable)){
      tab_out <- tab %>% flextable::as_flextable(...) #%>%
      # flextable::theme_vanilla()
    }else{
      tab_out <- tab %>% flextable::flextable(...)
    }
  }else{
    tab_out <- tab
  }


  tab_out <- tab_out %>% flextable::theme_booktabs()

  if(!is.null(digits)){
    checkmate::assert_numeric(digits)
    tab_out <- tab_out %>% colformat_double(digits = digits)
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
#' @param digits number of digits to round scores to.
#'
#' @returns a formatted flextable object
#'
#' @keywords internal
format_overall_scores <- function(formatted_pkg_scores, digits = 2){

  # Create overall table
  overall_tbl <- formatted_pkg_scores$formatted$overall %>%
    mutate(
      Risk = factor(.data$Risk, levels = RISK_LEVELS),
      Category = stringr::str_to_title(.data$Category)
    ) %>% as.data.frame()

  # Base table
  overall_flextable <- flextable_formatted(overall_tbl, show_coltype = FALSE, digits = digits)

  # Add colors and styling
  overall_flextable <- overall_flextable %>%
    flextable::bg(bg = "#ffffff", part = "all") %>%
    flextable::color(color = "black", part = "body") %>%
    # Risk Styling
    flextable::color(color = "darkgreen", j = 3, i = ~ `Risk` == 'Low Risk') %>%
    flextable::color(color = "orange", j = 3, i = ~ `Risk` == 'Medium Risk') %>%
    flextable::color(color = "darkred", j = 3, i = ~ `Risk` == 'High Risk') %>%
    # Error Styling
    flextable::bold( j = 2, i = ~ `Risk` == 'Blocking') %>%
    flextable::color(color = "red", j = 3, i = ~ `Risk` == 'Blocking') %>%
    flextable::bold( j = 2, i = ~ `Risk` == 'NA - unexpected') %>%
    # Header/Caption
    flextable::set_header_labels(Category = "Category", Risk = "Risk Level") %>%
    flextable::set_caption("Package Risk Metrics Summary") %>%
    flextable::align(align = "center", part = "all")

  # Add minibars
  overall_flextable <- add_score_minibar(overall_flextable, column_index = 2, risk_col = "Risk", digits = digits)

  # Add hline before overall section and bold it (should use dplyr::lag() I would think, but I guess flextable is weird)
  overall_flextable <- overall_flextable %>%
    flextable::hline(i = ~ dplyr::lead(`Category` == 'Overall'),  border = officer::fp_border(width = 1))  %>%
    flextable::bold(j = 1:3, i = ~ `Category` == 'Overall')

  return(overall_flextable)
}


#' Retrieve and format category labels
#'
#' @param formatted_pkg_scores list containing all scores
#' @param category one of `c('testing', 'documentation', 'maintenance', 'transparency', 'overall')`
#' @param risk_only Logical (T/F). If `TRUE`, return just the overall risk
#'
#' @returns a character string in the format of <category>: <risk>
#'
#' @importFrom dplyr mutate
#'
#' @keywords internal
get_overall_labels <- function(formatted_pkg_scores, category, risk_only = FALSE){
  overall_df <- formatted_pkg_scores$formatted$overall
  checkmate::assert_true(category %in% unique(overall_df$Category))
  overall_df <- overall_df %>% mutate(
    category_label = paste0(stringr::str_to_title(.data$Category), ": ", .data$Risk)
  )
  if(isTRUE(risk_only)){
    cat_label <- overall_df$Risk[grep(category, overall_df$Category)]
  }else{
    cat_label <- overall_df$category_label[grep(category, overall_df$Category)]
  }

  return(cat_label)
}


#' Format Package Details
#'
#' Formats Documentation, Maintenance and Transparency scores
#'
#' @param formatted_pkg_scores list containing all scores
#' @param color_headers Logical (T/F). If `TRUE`, color the headers based on risk level
#'
#' @returns a formatted flextable object
#'
#' @keywords internal
format_package_details <- function(formatted_pkg_scores, color_headers = TRUE){

  package_details_list <- formatted_pkg_scores$formatted$scores
  # Get the scores for each category
  # TODO: assign risk to each category label (i.e. Maintenance: Low Risk) Need to figure out weighting first


  scores_df <- purrr::imap(package_details_list, ~ {
    .x %>% mutate(
      Category = .y,
      risk_header = get_overall_labels(formatted_pkg_scores, .y)
    )
  }) %>% purrr::list_rbind() %>%
    mutate(Risk = factor(.data$Risk, levels = RISK_LEVELS))

  # Testing is a separate table (for now)
  scores_df <- scores_df %>% filter(Category != "testing")

  # Format Table
  scores_df <- scores_df %>% mutate(Category = risk_header) %>%
    dplyr::select(-c(risk_header, "Raw Score"))

  # Group by category
  scores_df_flex <- flextable::as_grouped_data(scores_df, groups = "Category")

  category_scores_flextable <-
    flextable_formatted(
      scores_df_flex,
      hide_grouplabel = TRUE
    ) %>%
    flextable::bg(bg = "#ffffff", part = "all") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::color(color = "black", part = "body") %>%
    # Individual Risk Colors
    flextable::color(color = "darkgreen", j = 3, i = ~ Result == "Yes") %>%
    flextable::color(color = "darkred", j = 3, i = ~ Result == "No") %>%
    # Alignment and caption
    flextable::align(i = ~ !is.na(Category), align = "center") %>%
    flextable::bold(i = ~ !is.na(Category)) %>%
    flextable::set_caption("Package Details")

  if(isTRUE(color_headers)){
    # Header Risk Colors
    category_scores_flextable <- category_scores_flextable %>%
      flextable::color(color = "darkgreen", j = 1, i = ~ grepl("Low Risk", Category)) %>%
      flextable::color(color = "orange", j = 1, i = ~ grepl("Medium Risk", Category)) %>%
      flextable::color(color = "darkred", j = 1, i = ~ grepl("High Risk", Category))
  }

  return(category_scores_flextable)
}


#' Format testing scores
#'
#' @param formatted_pkg_scores list containing all scores
#'
#' @returns a formatted flextable object
#'
#' @keywords internal
format_testing_scores <- function(formatted_pkg_scores){

  testing_df <- formatted_pkg_scores$formatted$scores$testing %>%
    mutate(Risk = factor(.data$Risk, levels = RISK_LEVELS))

  # Get overall risk for caption
  flex_caption <- get_flex_caption(formatted_pkg_scores, "testing")

  testing_scores_flextable <-
    flextable_formatted(
      testing_df,
      as_flextable = FALSE,
      col_keys = c("Criteria", "Result", "Risk")
    ) %>%
    flextable::bg(bg = "#ffffff", part = "all") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::color(color = "black", part = "body") %>%
    # Individual Risk Colors
    flextable::color(color = "darkgreen", j = 3, i = ~ Risk == "Low Risk") %>%
    flextable::color(color = "orange", j = 3, i = ~ Risk == "Medium Risk") %>%
    flextable::color(color = "darkred", j = 3, i = ~ Risk == "High Risk") %>%
    # Error Styling
    flextable::color(color = "red", j = 3, i = ~ `Risk` == 'Blocking') %>%
    flextable::bold( j = 3, i = ~ `Risk` == 'NA - unexpected') %>%
    flextable::set_caption(flex_caption)

  # testing_scores_flextable <- testing_scores_flextable %>%
  #   flextable::compose(
  #     j = 2, i = ~ Result == "Failed",
  #     value = flextable::as_paragraph(
  #       `Result`, " ", flextable::as_chunk("\u274c", props = flextable::fp_text_default(color = "red", font.size = 9))
  #     )
  #   )



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
    flextable::bg(bg = "#ffffff", part = "all") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::color(color = "black", part = "body") %>%
    flextable::set_header_labels(Category = "Category", Value = "Value") %>%
    flextable::set_caption("System Information")

  return(system_info_flextable)
}


#' Format vector of mitigation text
#'
#' @param mitigation_block character vector of mitigation text. Can include bullets
#'
#' @keywords internal
format_mitigation <- function(mitigation_block){
  header_str <- "\n## Mitigation\n\n"

  # If specified in bullet format, we need to make sure there is a new line after the header
  # to be formatted correctly
  starts_with_bullet <- function(str){
    grepl("^[[:space:]]*[-+*]", str)
  }

  if(is.null(mitigation_block)){
    cat(NULL)
  }else{
    if(length(mitigation_block) > 0){
      cat(header_str)
      for(i in seq_along(mitigation_block)){
        if(i > 1){
          if(starts_with_bullet(mitigation_block[[i]]) && !starts_with_bullet(mitigation_block[[i-1]]) && nzchar(mitigation_block[[i-1]])){
            cat("\n")
          }
        }
        cat(mitigation_block[[i]])
        cat("\n")
      }
    }
  }
}


#' Format summary of overall risks
#'
#' @param risk_summary_df summary dataframe containing overall scores, risks, and package info for each package
#' @param digits number of digits to round scores to
#'
#' @returns a formatted flextable object
#'
#' @keywords internal
format_score_summaries <- function(risk_summary_df, digits = 2){

  # Base table
  risk_summary_flex <- flextable_formatted(
    risk_summary_df, as_flextable = FALSE, digits = digits,
    col_keys = c("Package", "Version", "Weighted Score", "Overall Risk")
  )

  # Add colors and styling
  risk_summary_flex <- risk_summary_flex %>%
    flextable::bg(bg = "#ffffff", part = "all") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::color(color = "black", part = "body") %>%
    # Individual Risk Colors
    flextable::color(color = "darkgreen", j = 4, i = ~ `Overall Risk` == "Low Risk") %>%
    flextable::color(color = "orange", j = 4, i = ~ `Overall Risk` == "Medium Risk") %>%
    flextable::color(color = "darkred", j = 4, i = ~ `Overall Risk` == "High Risk")

  # Add minibars
  risk_summary_flex <- add_score_minibar(risk_summary_flex, column_index = 3, risk_col = "Overall Risk", digits = digits)

  return(risk_summary_flex)
}





#' Create a flextable caption colored by risk
#'
#' @inheritParams get_overall_labels
#'
#' @returns a flextable caption
#'
#' @keywords internal
get_flex_caption <- function(formatted_pkg_scores, category){
  overall_df <- formatted_pkg_scores$formatted$overall
  checkmate::assert_true(category %in% unique(overall_df$Category))

  tot_risk <- get_overall_labels(formatted_pkg_scores, category, risk_only = TRUE)
  cap_color <- dplyr::case_when(
    tot_risk == "Low Risk" ~ "darkgreen",
    tot_risk == "Medium Risk" ~ "orange",
    tot_risk == "High Risk" ~ "darkred",
    TRUE ~ "black"
  )
  flex_caption <- flextable::as_paragraph(
    flextable::as_chunk(
      get_overall_labels(formatted_pkg_scores, category),
      props = flextable::fp_text_default(color = cap_color)
    )
  )
  return(flex_caption)
}


#' Add flextable minibar to column
#'
#' @param flextable_df a flextable object
#' @param column_index Index of column to apply the minibar to
#' @param risk_col The name of the column specifying the risks
#' @param digits number of digits to round scores to.
#'
#' @details
#' a minibar will replace the entire column of whichever specified (via `column_index`). In other words, it doesn't
#' append the minibar to a column. Specifying an index that **doesn't** correspond to the weighted scores, would overwrite that column.
#'
#' `digits` must be specified, because the default behavior is rounding the column to one decimal
#'
#' @keywords internal
add_score_minibar <- function(flextable_df, column_index = 2, risk_col = "Risk", digits = 2){

  # Formatter function for rounding (this is required, otherwise it will round to one decimal)
  formatter_fn <- function(x){format(round(x, digits), nsmall = digits)}

  base_table <- flextable_df$body$dataset

  risk_types <- c("Low Risk", "Medium Risk", "High Risk")
  risk_locs <- purrr::map(risk_types, ~{
    locs <- which(base_table[[risk_col]]==.x)
    if(rlang::is_empty(locs)) locs <- NULL
    locs
  }) %>% stats::setNames(risk_types)


  if(!is.null(risk_locs$`Low Risk`)){
    flextable_df <- flextable_df %>%
      flextable::compose(
        j = column_index, i = risk_locs$`Low Risk`,
        value = flextable::as_paragraph(
          flextable::as_chunk(
            `Weighted Score`, props = flextable::fp_text_default(color = "black"),
            formatter = formatter_fn
          ),
          " ",
          flextable::minibar(value = `Weighted Score`, max = 1, barcol = "darkgreen", bg = "transparent", height = .15)
        )
      )
  }

  if(!is.null(risk_locs$`Medium Risk`)){
    flextable_df <- flextable_df %>%
      flextable::compose(
        j = column_index, i = risk_locs$`Medium Risk`,
        value = flextable::as_paragraph(
          flextable::as_chunk(
            `Weighted Score`, props = flextable::fp_text_default(color = "black"),
            formatter = formatter_fn
          ),
          " ",
          flextable::minibar(value = `Weighted Score`, max = 1, barcol = "orange", bg = "transparent", height = .15)
        )
      )
  }

  if(!is.null(risk_locs$`High Risk`)){
    flextable_df <- flextable_df %>%
      flextable::compose(
        j = column_index, i = risk_locs$`High Risk`,
        value = flextable::as_paragraph(
          flextable::as_chunk(
            `Weighted Score`, props = flextable::fp_text_default(color = "black"),
            formatter = formatter_fn
          ),
          " ",
          flextable::minibar(value = `Weighted Score`, max = 1, barcol = "darkred", bg = "transparent", height = .15)
        )
      )
  }

  return(flextable_df)
}

