#' Create extra notes summarizing the covr & rcmdcheck outputs, and documentation
#'
#' @inheritParams render_scorecard
#' @param pkg_tar_path path to a tarball
#'
#' @keywords internal
create_extra_notes <- function(results_dir, pkg_tar_path){
  covr_path <- get_result_path(results_dir, "covr.rds")
  check_path <- get_result_path(results_dir, "check.rds")

  # Format rcmdcheck
  check_results <- readRDS(check_path)

  # Format coverage
  covr_results <- readRDS(covr_path)
  covr_results_df <- covr_results$coverage$filecoverage %>% as.data.frame()
  covr_results_df <- covr_results_df %>%
    dplyr::mutate(r_script = row.names(covr_results_df)) %>%
    dplyr::select(r_script, test_coverage = ".")
  row.names(covr_results_df) <- NULL

  # Format documentation
  exports_df <- get_exports_documented(pkg_tar_path)

  # Join coverage and documentation
  covr_doc_df <- dplyr::full_join(covr_results_df, exports_df, by = dplyr::join_by(r_script))

  return(
    list(
      covr_doc_df = covr_doc_df,
      check_output = check_results$stdout
    )
  )
}


#' Get aliases of exported functions
#'
#' Returns the aliases of exported functions, the name of the `man` file, and the R script the function is contained in
#'
#' @inheritParams create_extra_notes
#'
#' @returns a tibble
#'
#' @keywords internal
get_exports_documented <- function(pkg_tar_path){

  # Unpack tarball
  pkg_source_path <- unpack_tarball(pkg_tar_path)
  on.exit(unlink(dirname(pkg_source_path), recursive = TRUE), add = TRUE)

  # Locate script for each export - aliases will be joined per script
  exports_df <- find_export_script(pkg_source_path)

  # some of this code was taken/inspired from riskmetric (finding aliases from Rd file)
  # see `riskmetric:::pkg_ref_cache.help_aliases.pkg_source` for overlap
  rd_files <- list.files(file.path(pkg_source_path, "man"), full.names = TRUE)
  rd_files <- rd_files[grep("\\.Rd$", rd_files)]
  aliases_df <- purrr::map_dfr(rd_files, function(rd_file.i) {
    rd_lines <- readLines(rd_file.i)

    # Get Rd file and aliases for exported functions
    aliases <- gsub("\\}", "", gsub("\\\\alias\\{", "",
                                    rd_lines[grep("^\\\\alias", rd_lines)]))
    man_name <- strsplit(strsplit(rd_file.i, "\\/man\\/")[[1]][2],
                         "\\.Rd")[[1]]

    # Get R script function is in
    r_script <- gsub("\\% Please edit documentation in", "",
                     rd_lines[grep("^\\% Please edit documentation in", rd_lines)]) %>%
      stringr::str_trim()

    tibble::tibble(
      r_script = r_script,
      man_name = man_name,
      alias = aliases
    )
  })

  # This will drop unexported aliases (such as an overall package help file)
  exports_doc_df <- exports_df %>% dplyr::left_join(aliases_df, by = "r_script") %>%
    dplyr::relocate("r_script")

  # Tabulate documentation percent per R script
  exports_doc_df <- exports_doc_df %>% dplyr::group_by(.data$r_script, .data$export) %>%
    dplyr::count(is_documented = export %in% alias) %>%
    dplyr::group_by(.data$r_script) %>% dplyr::summarise(documentation = 100*(sum(is_documented)/sum(n)))

  return(exports_doc_df)
}


#' Find the R script containing the export
#'
#' We need to know the script so we can determine documentation percentage per script
#'
#' @param pkg_source_path path to installed package
#'
#' @keywords internal
find_export_script <- function(pkg_source_path){

  # R files to search through
  r_files <- list.files(file.path(pkg_source_path, "R"), full.names = TRUE)
  r_files <- r_files[grep("\\.R$", r_files, ignore.case = TRUE)] # precautionary filter to only R files (unsure if this is needed)

  # All exports
  exports <- unname(unlist(pkgload::parse_ns_file(pkg_source_path)[c("exports","exportMethods")]))

  # nested function to check for the exported functions in a file
  find_exported_functions <- function(file_path, exports) {
    file_text <- readLines(file_path)
    result <- list()

    for (export in exports) {
      pattern <- paste0("^", export, "\\s*<-\\s*function\\s*\\(")
      matches <- grep(pattern, file_text, value = TRUE)
      if (length(matches) > 0) {
        result[[export]] <- file_path
      }
    }
    return(result)
  }

  # Find script per export
  export_lst <- purrr::map(r_files, find_exported_functions, exports = exports) %>%
    suppressWarnings() # suppress `incomplete final line` warnings
  export_lst <- purrr::reduce(export_lst, c)

  # convert to dataframe and format r_script column
  export_df <- export_lst %>% tibble::enframe(name = "export", value = "r_script") %>% tidyr::unnest(cols = c(r_script))  %>%
    dplyr::mutate(r_script = paste0("R/", basename(r_script)))

  return(export_df)
}

