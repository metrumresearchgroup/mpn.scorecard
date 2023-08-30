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
#' @param font_size font size of the table.
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
                                font_size = 10,
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
    tab_out <- tab_out %>% flextable::colformat_double(digits = digits)
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
  overall_tbl <- formatted_pkg_scores$formatted$overall_scores %>%
    mutate(
      risk = factor(.data$risk, levels = RISK_LEVELS),
      category = stringr::str_to_title(.data$category)
    ) %>% as.data.frame() %>% format_colnames_to_title()


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
    flextable::color(color = "red", j = 3, i = ~ `Risk` == 'NA - unexpected') %>%
    flextable::bold(j = 3, i = ~ `Risk` == 'NA - unexpected') %>%
    # Header/Caption
    flextable::set_header_labels(Category = "Category", Risk = "Risk Level") %>%
    flextable::set_caption("Package Risk Metrics Summary") %>%
    flextable::align(align = "center", part = "all")

  # Add minibars
  overall_flextable <- add_score_minibar(overall_flextable, risk_col = "Risk", score_col = "Category Score", digits = digits)

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
  overall_df <- formatted_pkg_scores$formatted$overall_scores
  checkmate::assert_true(category %in% unique(overall_df$category))
  overall_df <- overall_df %>% mutate(
    category_label = paste0(stringr::str_to_title(.data$category), ": ", .data$risk)
  )
  if(isTRUE(risk_only)){
    cat_label <- overall_df$risk[grep(category, overall_df$category)]
  }else{
    cat_label <- overall_df$category_label[grep(category, overall_df$category)]
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

  package_details_list <- formatted_pkg_scores$formatted$category_scores
  # Get the scores for each category
  # TODO: assign risk to each category label (i.e. Maintenance: Low Risk) Need to figure out weighting first


  scores_df <- purrr::imap(package_details_list, ~ {
    .x %>% mutate(
      category = .y,
      risk_header = get_overall_labels(formatted_pkg_scores, .y)
    )
  }) %>% purrr::list_rbind() %>%
    mutate(
      risk = factor(.data$risk, levels = RISK_LEVELS),
      criteria = gsub("_", " ", .data$criteria) %>% stringr::str_to_title() %>% gsub("Url", "URL", .)
    )

  # Testing is a separate table (for now)
  scores_df <- scores_df %>% dplyr::filter(.data$category != "testing")

  # Format Table
  scores_df <- scores_df %>% mutate(category = .data$risk_header) %>%
    dplyr::select(-c("risk_header", "score")) %>%
    format_colnames_to_title()

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

  # Get overall risk for caption
  flex_caption <- get_flex_caption(formatted_pkg_scores, "testing")

  testing_df <- formatted_pkg_scores$formatted$category_scores$testing %>%
    mutate(risk = factor(.data$risk, levels = RISK_LEVELS)) %>%
    format_colnames_to_title()

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
    flextable::color(color = "red", j = 2, i = ~ `Result` == 'Failed') %>%
    flextable::bold( j = 3, i = ~ `Risk` == 'NA - unexpected') %>%
    flextable::color(color = "red", j = 3, i = ~ `Risk` == 'NA - unexpected') %>%
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

  # Format Table
  risk_summary_df <- risk_summary_df %>% format_colnames_to_title()
  # Base table
  risk_summary_flex <- flextable_formatted(
    risk_summary_df, as_flextable = FALSE, digits = digits,
    col_keys = c("Package", "Version", "Overall Score", "Overall Risk", "Mitigation"),
    pg_width = 6.5
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
  risk_summary_flex <- add_score_minibar(risk_summary_flex, risk_col = "Overall Risk", score_col = "Overall Score", digits = digits)

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
  overall_df <- formatted_pkg_scores$formatted$overall_scores
  checkmate::assert_true(category %in% unique(overall_df$category))

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
#' @param score_col The name of the column specifying the scores. Used to calculate
#' @param risk_col The name of the column specifying the risks
#' @param column_index Index of column to apply the minibar to. Traditionally, this should map to the index of `score_col`, but doesn't need to.
#'        If `NULL`, will map to the index of `score_col`.
#' @param color_text Logical (T/F). If `FALSE` color the text in `column_index` black. Otherwise use risk colors (green, orange, red)
#' @param digits number of digits to round scores to.
#'
#' @details
#' a minibar will replace the entire column of whichever specified (via `column_index`). In other words, it doesn't
#' append the minibar to a column. Specifying an index that **doesn't** correspond to the weighted scores, would overwrite that column.
#'
#' `digits` must be specified, because the default behavior is rounding the column to one decimal
#'
#' @importFrom rlang sym !!
#'
#' @keywords internal
add_score_minibar <- function(flextable_df,
                              score_col = "Weighted Score",
                              risk_col = "Risk",
                              column_index = NULL,
                              color_text = FALSE,
                              digits = 2){

  base_table <- flextable_df$body$dataset

  checkmate::assert_true(risk_col %in% names(base_table))
  checkmate::assert_true(score_col %in% names(base_table))

  # Get risk locations
  risk_types <- c("Low Risk", "Medium Risk", "High Risk")
  risk_locs <- purrr::map(risk_types, ~{
    locs <- which(base_table[[risk_col]]==.x)
    if(rlang::is_empty(locs)) locs <- NULL
    locs
  }) %>% stats::setNames(risk_types)

  # Handle column index
  if(is.null(column_index)){
    column_index <- grep(score_col, names(base_table))
  }

  # Handling of text to display next to minibar
  text_display <- names(base_table)[column_index]

  if(isTRUE(color_text)){
    text_colors <- c("darkgreen", "orange", "darkred") %>% stats::setNames(risk_types)
  }else{
    text_colors <- rep("black", 3) %>% stats::setNames(risk_types)
  }

  # Add minibars
  if(!is.null(risk_locs$`Low Risk`)){
    flextable_df <- flextable_df %>%
      flextable::compose(
        j = column_index, i = risk_locs$`Low Risk`,
        value = flextable::as_paragraph(
          flextable::as_chunk(
            !!sym(text_display), props = flextable::fp_text_default(color = text_colors[["Low Risk"]]),
            digits = digits
          ),
          " ",
          flextable::minibar(value = !!sym(score_col), max = 1, barcol = "darkgreen", bg = "transparent", height = .15)
        )
      )
  }

  if(!is.null(risk_locs$`Medium Risk`)){
    flextable_df <- flextable_df %>%
      flextable::compose(
        j = column_index, i = risk_locs$`Medium Risk`,
        value = flextable::as_paragraph(
          flextable::as_chunk(
            !!sym(text_display), props = flextable::fp_text_default(color = text_colors[["Medium Risk"]]),
            digits = digits
          ),
          " ",
          flextable::minibar(value = !!sym(score_col), max = 1, barcol = "orange", bg = "transparent", height = .15)
        )
      )
  }

  if(!is.null(risk_locs$`High Risk`)){
    flextable_df <- flextable_df %>%
      flextable::compose(
        j = column_index, i = risk_locs$`High Risk`,
        value = flextable::as_paragraph(
          flextable::as_chunk(
            !!sym(text_display), props = flextable::fp_text_default(color = text_colors[["High Risk"]]),
            digits = digits
          ),
          " ",
          flextable::minibar(value = !!sym(score_col), max = 1, barcol = "darkred", bg = "transparent", height = .15)
        )
      )
  }

  return(flextable_df)
}

#' Format column names to be user friendly
#'
#' Replaces underscores (`_`) with spaces and makes the first letter capitalized
#'
#' @param df a dataframe
#'
#' @keywords internal
format_colnames_to_title <- function(df){
  new_names <- gsub("_", " ", names(df)) %>% stringr::str_to_title()
  names(df) <- new_names
  return(df)
}


#' Format Traceability Matrix
#'
#' @param exports_df tibble. Output of [make_traceability_matrix()]
#' @param return_vals Logical (T/F). If `TRUE`, return the objects instead of printing them out for `rmarkdown`. Used for testing.
#'
#' @keywords internal
format_traceability_matrix <- function(exports_df, return_vals = FALSE){
  sub_header_str <- "\n# Traceability Matrix\n\n"
  if(is.null(exports_df)){
    if(isTRUE(return_vals)){
      return(NULL)
    }else{
      cat(NULL)
    }
  }else{
    ### Exported Functions ###
    # Unnest tests and testing directories
    exported_func_df <- exports_df %>%
      mutate(
        test_files = purrr::map_chr(.data$test_files, ~paste(.x, collapse = "\n")),
        test_dirs = purrr::map_chr(.data$test_dirs, ~paste(.x, collapse = "\n")),
      )

    # Get testing directories for caption
    test_dirs <- exported_func_df %>% dplyr::pull(test_dirs) %>% unique()
    test_dirs <- test_dirs[test_dirs != ""] %>% paste(collapse = ", ")

    # Remove testing directory column (not a column due to horizontal space limits)
    exported_func_df <- exported_func_df %>% dplyr::select(-"test_dirs")

    # Format Table
    exported_func_df <- exported_func_df %>%
      mutate(dplyr::across("exported_function":"code_file", ~ stringr::str_wrap(.x, width = 25, whitespace_only = FALSE)))
    exported_func_df <- exported_func_df %>% format_colnames_to_title()

    # Create flextable
    exported_func_flex <- flextable_formatted(exported_func_df, as_flextable = FALSE, pg_width = 7, font_size = 9) %>%
      flextable::set_caption("Traceability Matrix") %>%
      flextable::add_footer_row(
        values = flextable::as_paragraph(glue::glue("Testing directories: {test_dirs}")),
        colwidths = c(4)
      )

    boiler_plate_txt <- paste("This table links all package functionality to the documentation
    which describes that functionality, as well as the testing code which confirms the functionality
    works as described.") %>% strwrap(simplify = TRUE, width = 1000)

    if(isTRUE(return_vals)){
      return(exported_func_flex)
    }else{
      # Exported Function Documentation
      cat(sub_header_str)
      cat("\n")
      cat(boiler_plate_txt)
      cat("\n")
      cat(knitr::knit_print(exported_func_flex))
      cat("\n")
    }
  }
}

#' Format Appendix
#'
#' @param extra_notes_data named list. Output of [create_extra_notes()]
#' @param return_vals Logical (T/F). If `TRUE`, return the objects instead of printing them out for `rmarkdown`. Used for testing.
#'
#' @keywords internal
format_appendix <- function(extra_notes_data, return_vals = FALSE){
  sub_header_strs <- c("\n## R CMD Check\n\n", "\n## Test coverage\n\n")

    ### Covr Results ###
    # Format Table
    covr_results_df <- extra_notes_data$covr_results_df
    if (is.numeric(covr_results_df$test_coverage)) {
      covr_results_df <- covr_results_df %>%
        dplyr::mutate(test_coverage = paste0(.data$test_coverage, "%")) %>%
        format_colnames_to_title()

      # Create flextable
      covr_results_flex <- flextable_formatted(covr_results_df, as_flextable = FALSE, pg_width = 4) %>%
        flextable::set_caption("Test Coverage") %>%
        flextable::align(align = "right", part = "all", j=2) %>%
        flextable::add_footer_row(
          values = flextable::as_paragraph(paste(
            "Test coverage is calculated per script, rather than per function.",
            "See Traceability Matrix for function-to-test-file mapping."
            )),
          colwidths = c(2)
        )
    } else {
      covr_results_flex <- NULL
    }

    ### R CMD Check Results ###
    check_output <- extra_notes_data$check_output

    if(isTRUE(return_vals)){
      return(
        list(
          covr_results_flex = covr_results_flex,
          check_output = check_output
        )
      )
    }else{
      ### Print all Results ###
      # R CMD Check
      cat(sub_header_strs[1])
      cat_verbatim(check_output)
      cat("\\newpage")
      # Coverage
      cat(sub_header_strs[2])
      cat("\n")

      if (is.null(covr_results_flex)) {
        err_type <- covr_results_df$r_script
        if (identical(err_type, "File coverage failed")) {
          cat("\n\nCalculating code coverage failed with following error:\n\n")
          cat_verbatim(covr_results_df$test_coverage)
        } else if (identical(err_type, "No coverage results")) {
          cat(
            "\n\n", "Unable to calculate coverage: ",
            covr_results_df$test_coverage, "\n\n"
          )
        } else {
          stop("Unknown error type: ", err_type)
        }
      } else {
        cat(knitr::knit_print(covr_results_flex))
      }

      cat("\n")
    }
}

cat_verbatim <- function(s) {
  # At the markdown level, we need to make sure that nothing in s makes pandoc
  # end the raw block early.
  s <- gsub("(?m)(^[ \t]*```[ \t]*)$", "<SANITIZED>\\1", s, perl = TRUE)
  # At the LaTeX level, we need to escape any "\end{spverbatim}".
  s <- gsub(
    "\\end{spverbatim}",
    "<SANITIZED BACKSLASH>end{spverbatim}",
    s,
    fixed = TRUE
  )

  cat(
    # pandoc should automatically detect this snippet and treat it as raw LaTeX,
    # but mark it explicitly to avoid potential edge cases.
    "\n\n```{=latex}",
    "\\begin{small}",
    "\\begin{spverbatim}",
    s,
    "\\end{spverbatim}",
    "\\end{small}",
    "```\n\n",
    sep = "\n"
  )
}
