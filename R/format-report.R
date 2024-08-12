#' Autofit and format flextables
#'
#'
#' @param tab a dataframe or flextable object. Must be coercible to a dataframe
#' @param autofit logical (T/F). Whether or not to autofit the table.
#' @param pg_width width (in inches) of the table. Generally 1 inch less than the default word document (8 in.)
#' @param column_width named vector, where the column names are assigned to the desired _relative_ width.
#'        If specified, set these column widths before fitting to word document
#' @param doc_type Word, PDF, or HTML. Controls font size and autofit scaling.
#' @param digits numeric. Number of digits to round to. If `NULL`, and `as_flextable = TRUE`, flextable will round to one digit.
#' @param font_size font size of the table.
#' @param ... additional args to be passed to `as_flextable()` or `flextable()`.
#'   Which function, if any, is called depends on the type of `tab`. If `tab` is
#'   already a flextable, neither is called. If `tab` inherits from
#'   "grouped_data", `as_flextable()` is called. Otherwise `flextable()` is
#'   called.
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
                                digits = NULL,
                                font_size = 10,
                                ...){

  # If flextable object already, just apply formatting
  if(!inherits(tab, "flextable")){
    fn <- if (inherits(tab, "grouped_data")) {
      flextable::as_flextable
    } else {
      flextable::flextable
    }
    tab_out <- fn(tab, ...)
  }else{
    tab_out <- tab
  }


  tab_out <- tab_out %>% flextable::theme_booktabs()

  if(!is.null(digits)){
    checkmate::assert_numeric(digits)
    tab_out <- tab_out %>% flextable::colformat_double(digits = digits)
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
    tab_out <- autofit_flex(tab_out, pg_width, doc_type)
  }

  return(tab_out)
}


#' Autofit flextable to dimensions of the document
#' @inheritParams flextable_formatted
#' @noRd
autofit_flex <- function(tab, pg_width = 7, doc_type = "PDF"){
  tab <- tab %>% flextable::autofit()
  tab <- flextable::width(tab, width = dim(tab)$widths*pg_width /(flextable::flextable_dim(tab)$widths))

  # Word needs additional autofit formatting (HTML is ok with this, but PDF gets wacky)
  if(doc_type %in% c("Word", "HTML")){
    tab <- tab %>% flextable::set_table_properties(width = 1, layout = "autofit")
  }
  return(tab)
}


#' Helper to apply basic flextable styling (center align and background coloring)
#' @param tab a flextable
#' @noRd
basic_flex_styles <- function(tab){
  checkmate::assert_true(inherits(tab, "flextable"))
  tab %>%
    flextable::bg(bg = "#ffffff", part = "all") %>%
    flextable::align(align = "center", part = "all") %>%
    flextable::color(color = "black", part = "body")
}


#' Helper to apply basic flextable styling for the header
#' @param tab a flextable
#' @param header_bg,header_ft the background and font colors for the header.
#' @param bold Logical (T/F). If `TRUE`, bold the header.
#' @noRd
flex_header <- function(
    tab,
    header_bg = "#666cb2",
    header_ft = "white",
    bold = TRUE
){
  checkmate::assert_true(inherits(tab, "flextable"))

  h_nrow <- flextable::nrow_part(tab, "header")
  if(h_nrow > 0){
    tab <- flextable::bg(tab, bg = header_bg, part = "header") %>%
      flextable::color(color = header_ft, part = "header")
    if(isTRUE(bold)){
      tab <- flextable::bold(tab, bold = TRUE, part = "header")
    }
  }
  return(tab)
}


#' Helper to apply basic flextable styling for the header
#' @param tab a flextable
#' @param footer_bg,footer_ft the background and font colors for the footer.
#' @param bold Logical (T/F). If `TRUE`, bold the footer.
#' @noRd
flex_footer <- function(
    tab,
    footer_bg = "#666cb2",
    footer_ft = "white",
    bold = FALSE
){
  checkmate::assert_true(inherits(tab, "flextable"))

  f_nrow <- flextable::nrow_part(tab, "footer")
  if(f_nrow > 0){
    tab <- flextable::bg(tab, bg = footer_bg, part = "footer") %>%
      flextable::color(color = footer_ft, part = "footer")
    if(isTRUE(bold)){
      tab <- flextable::bold(tab, bold = TRUE, part = "footer")
    }
  }
  return(tab)
}


#' Helper to apply flextable styling for alternating row background color
#'
#' @param tab a flextable
#' @param odd_body_bg,even_body_bg the background color of odd and even rows for
#'   the main body of `tab`. Note that all font is black for the body.
#' @param border Logical (T/F). If `TRUE`, add horizontal borders and a vertical
#'   border after the first column.
#' @keywords internal
flex_stripe <- function(
    tab,
    odd_body_bg = "#EFEFEF",
    even_body_bg = "transparent",
    border = TRUE
){
  checkmate::assert_true(inherits(tab, "flextable"))
  b_nrow <- flextable::nrow_part(tab, "body")

  # Add stripe to the body of the table
  if (b_nrow > 0) {
    even <- seq_len(b_nrow) %% 2 == 0
    odd <- !even
    tab <- flextable::bg(tab, i = odd, bg = odd_body_bg, part = "body") %>%
      flextable::bg(i = even, bg = even_body_bg, part = "body")
  }

  # Optionally add borders
  if(isTRUE(border)){
    std_border <- officer::fp_border(color = "black", style = "triple")
    tab <- tab %>%
      flextable::hline(part = "all", border = std_border) %>%
      flextable::vline(part = "body", border = std_border, j = 1) %>%
      flextable::vline(part = "header", border = std_border, j = 1)
  }

  # Add cell padding (needed when adding borders and not center aligned)
  tab <- flextable::set_table_properties(tab, opts_pdf = list(tabcolsep = 4))

  return(tab)
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
  overall_flextable <- flextable_formatted(overall_tbl, digits = digits)

  # Add colors and styling
  overall_flextable <- overall_flextable %>%
    basic_flex_styles() %>%
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
    flextable::bold(j = 1:3, i = ~ `Category` == 'Overall') %>%
    flex_header()

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
    basic_flex_styles() %>%
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

  category_scores_flextable <- flex_header(category_scores_flextable)

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
      col_keys = c("Criteria", "Result", "Risk")
    ) %>%
    basic_flex_styles() %>%
    # Individual Risk Colors
    flextable::color(color = "darkgreen", j = 3, i = ~ Risk == "Low Risk") %>%
    flextable::color(color = "orange", j = 3, i = ~ Risk == "Medium Risk") %>%
    flextable::color(color = "darkred", j = 3, i = ~ Risk == "High Risk") %>%
    # Error Styling
    flextable::color(color = "red", j = 2, i = ~ `Result` == 'Failed') %>%
    flextable::bold( j = 3, i = ~ `Risk` == 'NA - unexpected') %>%
    flextable::color(color = "red", j = 3, i = ~ `Risk` == 'NA - unexpected') %>%
    flextable::set_caption(flex_caption)

  testing_scores_flextable <- flex_header(testing_scores_flextable)

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
  date <- metadata_list[["date"]]
  if (is.null(date)) {
    abort("`date` required in `metadata_list`")
  }
  executor <- metadata_list[["executor"]]
  if (is.null(executor)) {
    abort("`executor` required in `metadata_list`")
  }

  info <- metadata_list[["info"]]
  data <- c(date = date, executor = executor, info[["sys"]], info[["env_vars"]])

  all_info_tbl <- data.frame(
    Category = stringr::str_to_title(names(data)),
    Value = unname(unlist(data))
  )

  # Create flextable
  system_info_flextable <-
    flextable_formatted(all_info_tbl) %>%
    basic_flex_styles() %>%
    flextable::set_header_labels(Category = "Category", Value = "Value") %>%
    flextable::set_caption("Execution and Machine Information") %>%
    flex_header()

  return(system_info_flextable)
}

# `cat_dependency_versions` would be a more fitting name (given cat() and
# format()), but "format_" is used for consistency with other functions in this
# file.
format_dependency_versions <- function(df) {
  if (is.null(df)) {
    return(invisible(NULL))
  }

  out <- prepare_dependency_versions(df)
  if (inherits(out, "flextable")) {
    # Note: knit_print.flextable() does _not_ print to stdout.
    out <- knitr::knit_print(out)
  }
  cat("\n\n", out, "\n\n")
}

prepare_dependency_versions <- function(df) {
  if (is.null(df)) {
    return("Unable to calculate R dependency table due to failing `R CMD check`.")
  }

  checkmate::assert_data_frame(df)
  checkmate::assert_subset(c("package", "version"), names(df))

  if (nrow(df) == 0) {
    return("Package has no required dependencies.")
  }

  flextable_formatted(df) %>%
    flextable::set_caption("R Dependency Versions") %>%
    flextable::set_header_labels(package = "Package", version = "Version") %>%
    basic_flex_styles() %>% flex_header() %>% flex_stripe(border = FALSE)
}

#' Format vector of comments text
#'
#' @param comments_block character vector of comments text. Can include bullets
#'
#' @keywords internal
format_comments <- function(comments_block){
  header_str <- "\n## Comments\n\n"

  # If specified in bullet format, we need to make sure there is a new line after the header
  # to be formatted correctly
  starts_with_bullet <- function(str){
    grepl("^[[:space:]]*[-+*]", str)
  }

  if(is.null(comments_block)){
    cat(NULL)
  }else{
    if(length(comments_block) > 0){
      cat(header_str)
      for(i in seq_along(comments_block)){
        if(i > 1){
          if(starts_with_bullet(comments_block[[i]]) && !starts_with_bullet(comments_block[[i-1]]) && nzchar(comments_block[[i-1]])){
            cat("\n")
          }
        }
        cat(comments_block[[i]])
        cat("\n")
      }
    }
  }
}


#' Format summary of overall risks
#'
#' @param risk_summary_df summary dataframe containing overall scores, risks,
#'  and package info for each package
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
    risk_summary_df, digits = digits,
    col_keys = c("Package", "Version", "Overall Score", "Overall Risk", "Comments"),
    pg_width = 6.5
  )

  # Add colors and styling
  risk_summary_flex <- risk_summary_flex %>%
    basic_flex_styles() %>%
    # Individual Risk Colors
    flextable::color(color = "darkgreen", j = 4, i = ~ `Overall Risk` == "Low Risk") %>%
    flextable::color(color = "orange", j = 4, i = ~ `Overall Risk` == "Medium Risk") %>%
    flextable::color(color = "darkred", j = 4, i = ~ `Overall Risk` == "High Risk")

  # Add minibars
  risk_summary_flex <- add_score_minibar(
    risk_summary_flex, risk_col = "Overall Risk", score_col = "Overall Score",
    digits = digits
  )

  # Add stripe and header
  risk_summary_flex <- flex_header(risk_summary_flex) %>%
    flex_stripe(border = FALSE)

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
#' @param wrap_cols logical (T/F). If `TRUE`, wrap columns
#'
#' @keywords internal
format_traceability_matrix <- function(
    exports_df,
    wrap_cols = TRUE,
    scorecard_type = "R"
){
  checkmate::assert_logical(wrap_cols)

  if(!is.null(exports_df)){
    ### Exported Functions ###
    # Unnest tests and testing directories
    exported_func_df <- exports_df %>%
      mutate(
        test_files = purrr::map_chr(.data$test_files, ~paste(.x, collapse = "\n"))
      )

    # Get testing directories for caption
    if ("test_dirs" %in% names(exported_func_df)) {
      test_dirs <- exported_func_df %>% tidyr::unnest(test_dirs) %>% dplyr::pull(test_dirs) %>% unique()
      test_dirs <- test_dirs[test_dirs != ""] %>% paste(collapse = ", ")
      # Remove testing directory column (not a column due to horizontal space limits)
      exported_func_df <- exported_func_df %>% dplyr::select(-"test_dirs")
    } else {
      test_dirs <- NULL
    }

    if ("exported_function" %in% names(exported_func_df)) {
      # Align internal scoring with external format.
      exported_func_df <- dplyr::rename(exported_func_df,
        entrypoint = "exported_function"
      )
    }

    entry_name <- switch(scorecard_type,
      "R" = "Exported Function",
      "cli" = "Command",
      "Entry Point"
    )
    exported_func_df <- dplyr::rename(
      exported_func_df,
      !!entry_name := "entrypoint"
    )

    # Format Table
    if(isTRUE(wrap_cols)){
      exported_func_df <- exported_func_df %>%
        dplyr::mutate(
          dplyr::across(
            all_of(c(entry_name, "code_file", "documentation")),
            function(x) wrap_text(x, width = 24, indent = TRUE, strict = TRUE)
          ),
          # Tests can be longer due to page width (pg_width) settings (we make it wider)
          test_files = purrr::map_chr(.data$test_files, function(tests){
            wrap_text(tests, width = 40, strict = TRUE)
          })
        )
    }
    exported_func_df <- exported_func_df %>% format_colnames_to_title()

    # Create flextable
    exported_func_flex <- flextable_formatted(exported_func_df, pg_width = 7, font_size = 9) %>%
      flextable::set_caption("Traceability Matrix")

    if (!is.null(test_dirs)) {
      exported_func_flex <- flextable::add_footer_row(
        exported_func_flex,
        values = flextable::as_paragraph(glue::glue("Testing directories: {test_dirs}")),
        colwidths = c(4)
      )
    }

    # Add stripe and other formatting details
    exported_func_flex <- exported_func_flex %>%
      flex_header() %>% flex_footer(bold = TRUE) %>% flex_stripe() %>%
      autofit_flex(pg_width = 7)

    return(exported_func_flex)
  }
}

#' Print boiler plate text about the traceability matrix
#' @inheritParams format_traceability_matrix
#' @noRd
trace_matrix_notes <- function(exports_df){
  sub_header_str <- "\n# Traceability Matrix\n\n"
  boiler_plate_txt <- paste("This table links all package functionality to the documentation
    which describes that functionality, as well as the testing code which confirms the functionality
    works as described.") %>% strwrap(simplify = TRUE, width = 1000)

  if(!is.null(exports_df)){
    # Exported Function Documentation
    cat(sub_header_str)
    cat("\n")
    cat(boiler_plate_txt)
    cat("\n")
  }else{
    cat(NULL)
  }
}

#' Format Appendix
#'
#' @param extra_notes_data named list. Output of [create_extra_notes()]
#' @param return_vals Logical (T/F). If `TRUE`, return the objects instead of printing them out for `rmarkdown`. Used for testing.
#'
#' @keywords internal
format_appendix <- function(extra_notes_data, return_vals = FALSE, scorecard_type = "R") {
  check_title <- if (identical(scorecard_type, "R")) {
    "R CMD Check"
  } else {
    "Check output"
  }
  sub_header_strs <- c(paste0("\n## ", check_title, "\n\n"), "\n## Test coverage\n\n")

  ### Covr Results ###
  # Format Table
  cov_results_df <- extra_notes_data$cov_results_df
  if (is.numeric(cov_results_df$test_coverage)) {
    cov_results_df <- cov_results_df %>%
      dplyr::mutate(
        code_file = wrap_text(.data$code_file,
          width = 45, indent = TRUE, strict = TRUE
        ),
        test_coverage = sprintf("%.2f%%", .data$test_coverage)
      ) %>%
      format_colnames_to_title()

    # Create flextable and format
    cov_results_flex <- flextable_formatted(cov_results_df, pg_width = 4) %>%
      flextable::set_caption("Test Coverage") %>%
      flextable::align(align = "right", part = "all", j=2) %>%
      flextable::add_footer_row(
        values = flextable::as_paragraph(paste(
          "Test coverage is calculated per script, rather than per function.",
          "See Traceability Matrix for function-to-test-file mapping."
        )),
        colwidths = c(2)
      )
    cov_results_flex <- cov_results_flex %>% flex_header() %>%
      flex_footer(footer_bg = "transparent", footer_ft = "black") %>%
      flex_stripe(border = FALSE)
  } else {
    cov_results_flex <- NULL
  }

  ### R CMD Check Results ###
  check_output <- extra_notes_data$check_output

  if(isTRUE(return_vals)){
    return(
      list(
        cov_results_flex = cov_results_flex,
        check_output = check_output
      )
    )
  }else{
    ### Print all Results ###
    # R CMD Check
    cat(sub_header_strs[1])
    cat_verbatim(check_output)

    if (is.null(cov_results_df)) {
      # This is an externally scored package without coverage.
      return(invisible(NULL))
    }

    cat("\\newpage")
    # Coverage
    cat(sub_header_strs[2])
    cat("\n")

    if (is.null(cov_results_flex)) {
      err_type <- cov_results_df$code_file
      if (identical(err_type, "File coverage failed")) {
        cat("\n\nCalculating code coverage failed with following error:\n\n")
        cat_verbatim(cov_results_df$test_coverage)
      } else if (identical(err_type, "No coverage results")) {
        cat(
          "\n\n", "Unable to calculate coverage: ",
          cov_results_df$test_coverage, "\n\n"
        )
      } else {
        stop("Unknown error type: ", err_type)
      }
    } else if (nrow(cov_results_df) == 0) {
      cat("Per file test coverage not provided.")
    } else {
      cat(knitr::knit_print(cov_results_flex))
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
