#' Create extra notes summarizing the covr & rcmdcheck outputs, and documentation
#'
#' @inheritParams render_scorecard
#' @param pkg_tar_path path to a tarball
#'
#' @keywords internal
create_extra_notes <- function(
    results_dir,
    pkg_tar_path
){
  covr_path <- get_result_path(results_dir, "covr.rds")
  check_path <- get_result_path(results_dir, "check.rds")

  # Format rcmdcheck
  check_results <- readRDS(check_path)

  # Format coverage
  covr_results <- readRDS(covr_path)
  covr_results_df <- covr_results$coverage$filecoverage %>% as.data.frame()
  covr_results_df <- covr_results_df %>%
    mutate(r_script = row.names(covr_results_df)) %>%
    dplyr::select("r_script", "test_coverage" = ".")
  row.names(covr_results_df) <- NULL

  # Format documentation
  exports_df <- get_exports_documented(pkg_tar_path) %>%
    dplyr::select("exported_function" = "export", everything())



  return(
    list(
      exports_df = exports_df,
      covr_results_df = covr_results_df,
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
      documentation = paste0("man/", man_name,".Rd"),
      code_file = r_script,
      alias = aliases
    )
  })

  # This will drop unexported aliases (such as an overall package help file)
  exports_doc_df <- exports_df %>% dplyr::left_join(aliases_df, by = "code_file") %>%
    dplyr::relocate(c("export", "documentation"))

  # Tabulate documentation percent per R script - checks that exported functions are documented
  is_documented_df <- exports_doc_df %>% dplyr::group_by(.data$export, .data$documentation, .data$code_file) %>%
    dplyr::count(is_documented = .data$export %in% .data$alias) %>%
    dplyr::group_by(.data$code_file) %>% dplyr::summarise(documentation_perc = 100*(sum(.data$is_documented)/sum(.data$n)))

  exports_doc_df <- dplyr::full_join(exports_doc_df, is_documented_df, by = "code_file") %>% dplyr::select(-c("alias"))

  # We dont need documentation in report - but will use for this message
  # TODO: if no packages on the next MPN build trigger this (when extra_notes = TRUE) - we can remove this and some of the above code
  if(any(exports_doc_df$documentation_perc != 100)){
    docs_missing <- exports_doc_df %>% dplyr::filter(.data$documentation_perc != 100) %>% dplyr::pull("export")
    docs_missing <- paste(docs_missing, collapse = ", ")
    message(glue::glue("The following exported functions do not have documentation: {docs_missing}"))
  }

  exports_doc_df <- exports_doc_df %>% dplyr::select(-"documentation_perc")

  # This maps all functions to tests (not just exports) - this was intentional in case we eventually want all functions
  test_mapping_df <- map_tests_to_functions(pkg_source_path)

  func_tests_doc_df <- exports_doc_df %>% dplyr::left_join(test_mapping_df, by = c("export" = "pkg_function"))

  return(func_tests_doc_df)
}


#' Search for functions in a directory
#'
#' @param funcs a vector of function names to search for
#' @param search_dir a directory to search for the functions in
#' @param func_declaration Logical (T/F). If `TRUE`, only look for where the function is actually defined
#'
#' @keywords internal
find_function_files <- function(funcs, search_dir, func_declaration = TRUE){

  # R files to search through
  r_files <- list.files(search_dir, full.names = TRUE)
  r_files <- r_files[grep("\\.R$", r_files, ignore.case = TRUE)] # precautionary filter to only R files (unsure if this is needed)

  # nested function to check for the exported functions in a file
  find_functions <- function(file_path, funcs, func_declaration) {
    file_text <- readLines(file_path)

    result <- list()
    for(func in funcs){
      if(isTRUE(func_declaration)){
        pattern <- paste0("^", func, "\\s*<-\\s*function\\s*\\(")
      }else{
        pattern <- func
      }
      matches <- grep(pattern, file_text, value = TRUE)
      if(length(matches) > 0){
        result[[func]] <- file_path
      }
    }
    return(result)
  }

  # Find script per function
 func_lst <- purrr::map(r_files, find_functions, funcs = funcs, func_declaration = func_declaration) %>%
    suppressWarnings() # suppress `incomplete final line` warnings
 func_lst <- purrr::reduce(func_lst, c)
 return(func_lst)
}

#' Find the R script containing the export
#'
#' We need to know the script so we can determine documentation percentage per script
#'
#' @param pkg_source_path a file path pointing to an unpacked/untarred package directory
#'
#' @keywords internal
find_export_script <- function(pkg_source_path){

  # All exports
  exports <- unname(unlist(pkgload::parse_ns_file(pkg_source_path)[c("exports","exportMethods")]))

  # Search for scripts functions are defined in
  export_lst <- find_function_files(funcs = exports, search_dir = file.path(pkg_source_path, "R"))

  # convert to dataframe and format code_file column
  export_df <- export_lst %>% tibble::enframe(name = "export", value = "code_file") %>% tidyr::unnest(cols = "code_file") %>%
    mutate(code_file = paste0("R/", basename(.data$code_file)))

  return(export_df)
}



#' Get tests/testthat directory from package directory
#'
#' @param pkg_source_path a file path pointing to an unpacked/untarred package directory
#'
#' @keywords internal
get_testing_dir <- function(pkg_source_path){
  checkmate::assert_directory_exists(pkg_source_path)

  pkg_dir_ls <- list.dirs(pkg_source_path, recursive = FALSE) #fs::dir_ls(pkg_source_path)
  test_dir_outer <- pkg_dir_ls[grep("^(/[^/]+)+/tests$", pkg_dir_ls)]
  if(length(test_dir_outer) == 0){
    stop(glue::glue("no testing directory found at {pkg_source_path}"))
  }

  test_dir_ls <- fs::dir_ls(test_dir_outer) %>% as.character()
  test_dirs <- test_dir_ls[grep("^(/[^/]+)+/testthat$", test_dir_ls)]
  if(length(test_dirs) == 0){
    stop(glue::glue("no `testthat` directory found at {test_dir_outer}"))
  }

  # Look for cases of test_that & describe/it in other directories
  other_dirs <- pkg_dir_ls[-grep(test_dir_outer, pkg_dir_ls)]
  tests_df <- get_tests(pkg_source_path = pkg_source_path, test_dirs = other_dirs)

  if(!rlang::is_empty(tests_df)){
    other_test_dirs <- file.path(pkg_source_path, unique(tests_df$test_dir))
    test_dirs <- c(test_dirs, other_test_dirs)
  }


  return(test_dirs)
}

#' Tabulate all tests in a package directory
#'
#' @inheritParams get_testing_dir
#' @param test_dirs one or more directories containing test scripts
#'
#'
#' @returns a dataframe containing all test names and the test file they are associated with
#'
#' @keywords internal
get_tests <- function(
    pkg_source_path,
    test_dirs
){

  # Set up to map over multiple testing directories (if need be)
  test_names_df <- purrr::map_dfr(test_dirs, function(test_dir_x){
    test_files <- fs::dir_ls(test_dir_x, recurse = TRUE)
    test_scripts <- test_files[grep("\\.R$", test_files, ignore.case = TRUE)]
    test_script_names <- fs::path_rel(test_scripts, start = pkg_source_path)

    tests_names_lst <- purrr::map(test_scripts, ~{
      test_script <- readLines(.x) %>% suppressWarnings()  # suppress `incomplete final line` warnings

      # Find test_that calls and extract the names of the tests
      test_that_calls <- grep('(?:\\s|^)test_that\\("([^"]+)"', test_script)
      test_that_names <- test_script[test_that_calls]

      # Find it calls and extract the names of the tests
      it_calls <- grep('(?:\\s|^)it\\("([^"]+)"', test_script)
      it_names <- test_script[it_calls]

      # Combine the names of tests and count the number of tests in each file
      n_tests <- length(c(test_that_names[test_that_calls], it_names[it_calls]))
      test_names <- c(test_that_names, it_names)

      # Remove `test_that` and `it` calls and code
      stringr::str_extract_all(test_names, "\"([^\"]*)\"|'([^']*)'") %>%
        gsub("[\"',/{]", "", .)
    }) %>% stats::setNames(test_script_names)

    # Remove empty scripts (setup/non-test scripts)
    tests_names_lst <- Filter(Negate(rlang::is_empty), tests_names_lst)

    # Convert to dataframe and make test_dir a relative path
    test_names_tbl <- purrr::map2_df(tests_names_lst, names(tests_names_lst),
                                     ~ tibble(test_file = .y, test_name = .x))
    if(!rlang::is_empty(test_names_tbl)){
      test_names_tbl <- test_names_tbl %>%
        mutate(test_dir = dirname(.data$test_file), test_file = basename(.data$test_file))
    }

    test_names_tbl
  })

  test_names_df

  return(test_names_df)
}


#' map test files and directories to all functions
#'
#' @param pkg_source_path  a file path pointing to an unpacked/untarred package directory
#'
#' @keywords internal
map_tests_to_functions <- function(pkg_source_path){

  test_dirs <- get_testing_dir(pkg_source_path)

  # Get all functions (not just exported)
  r_files <- list.files(file.path(pkg_source_path, "R"), full.names = TRUE)
  pkg_functions <- purrr::map(r_files, function(r_file_i) {
    file_text <- readLines(r_file_i) %>% suppressWarnings()
    function_calls <- file_text[grepl("\\s*function\\s*\\(", file_text)]
    function_names <- gsub("^\\s*([[:alnum:]_\\.]+)\\s*(<\\-|=)\\s*function.*", "\\1", function_calls)
    function_names
  })
  pkg_functions <- purrr::reduce(pkg_functions, c)

  pkg_func_df <- purrr::map_dfr(test_dirs, function(test_dir_x){
    func_lst <- find_function_files(
      funcs = pkg_functions,
      search_dir = test_dir_x,
      func_declaration = FALSE
    )
    tibble::enframe(func_lst, name = "pkg_function", value = "test_file") %>% tidyr::unnest("test_file") %>%
      mutate(test_dir = fs::path_rel(test_dir_x, pkg_source_path), test_file = basename(.data$test_file))
  })


  # Nest test files
  func_test_df <- pkg_func_df %>% dplyr::group_by(.data$pkg_function) %>%
    dplyr::summarize(test_files = list(unique(.data$test_file)), test_dirs = list(unique(.data$test_dir))) %>%
    dplyr::ungroup()

  return(func_test_df)

}
