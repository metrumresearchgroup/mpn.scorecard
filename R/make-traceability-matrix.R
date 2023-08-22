
#' Create a Traceability Matrix
#'
#' Returns a table that links all exported functions and their aliases to their documentation (`man` files),
#' the R scripts containing them, and the test scripts that reference them.
#'
#' @param pkg_tar_path path to a tarball
#' @inheritParams create_extra_notes
#' @param verbose Logical (`TRUE`/`FALSE`). If `TRUE`, show any warnings/messages per function.
#'
#' @returns a tibble
#'
#' @export
make_traceability_matrix <- function(pkg_tar_path, results_dir = NULL, verbose = FALSE){

  # Unpack tarball
  pkg_source_path <- unpack_tarball(pkg_tar_path)
  on.exit(unlink(dirname(pkg_source_path), recursive = TRUE), add = TRUE)

  # Locate script for each export - aliases will be joined per script
  exports_df <- find_export_script(pkg_source_path)

  # some of this code was taken/inspired from riskmetric (finding aliases and man_name from Rd file)
  # see `riskmetric:::pkg_ref_cache.help_aliases.pkg_source` for overlap
  # TODO: though this code is mainly taken from riskmetric, it falls short for older packages where Rd files
  # dont necessarily reference the R script they were written in.
  rd_files <- list.files(file.path(pkg_source_path, "man"), full.names = TRUE)
  rd_files <- rd_files[grep("\\.Rd$", rd_files)]
  aliases_df <- purrr::map_dfr(rd_files, function(rd_file.i) {
    rd_lines <- readLines(rd_file.i) %>% suppressWarnings()

    # Get Rd file and aliases for exported functions
    aliases <- gsub("\\}", "", gsub("\\\\alias\\{", "",
                                    rd_lines[grep("^\\\\alias", rd_lines)]))
    man_name <- strsplit(rd_file.i, "\\/man\\/")[[1]][2]

    # Get R script function is in
    # Search man file first, look for name matching if not found
    # older packages or man files written by hand may not have the following lines
    r_script <- gsub("\\% Please edit documentation in", "",
                     rd_lines[grep("^\\% Please edit documentation in", rd_lines)]) %>%
      stringr::str_trim()

    # look for name matching if not found (i.e. search for my_function in my_function.R)
    if(rlang::is_empty(r_script)){
      r_script_search <- find_function_files(fs::path_ext_remove(man_name), file.path(pkg_source_path, "R")) %>%
        unlist()
      if(!is.null(r_script_search)){
        r_script <- file.path("R", basename(r_script_search))
      }
    }

    if(!rlang::is_empty(r_script)){
      if(grepl(",", r_script)){ # if multiple linked scripts, separate by comma
        r_script <- strsplit(r_script, ",")[[1]] %>% stringr::str_trim()
      }
    }else{
      # Triggered if documentation exists, but no referenced R script
      if(isTRUE(verbose)){
        message(glue::glue("In package `{basename(pkg_source_path)}`, could not find an R script associated with man file: {man_name}"))
      }
    }


    tidyr::crossing(
      documentation = paste0("man/", man_name),
      code_file = r_script,
      alias = aliases
    )
  })

  # This will drop unexported aliases (such as an overall package help file)
  if(!rlang::is_empty(aliases_df)){
    aliases_df <- aliases_df %>% dplyr::filter(.data$alias %in% exports_df$export) %>% mutate(export = .data$alias)
    exports_doc_df <- exports_df %>% dplyr::left_join(aliases_df, by = c("export", "code_file")) %>%
      dplyr::relocate(c("export", "documentation"))
  }else{
    exports_doc_df <- exports_df %>% mutate(documentation = NA_character_, alias = NA_character_)
    if(isTRUE(verbose)){
      message(glue::glue("No documentation was found in `man/` for package `{basename(pkg_source_path)}`"))
    }
  }


  # Tabulate documentation percent per R script - checks that exported functions are documented
  is_documented_df <- exports_doc_df %>% dplyr::group_by(.data$export, .data$documentation, .data$code_file) %>%
    dplyr::summarise(is_documented = .data$export %in% .data$alias, .groups = "keep")

  exports_doc_df <- dplyr::full_join(exports_doc_df, is_documented_df, by = c("export", "documentation", "code_file")) %>%
    dplyr::select(-c("alias"))

  # We dont need documentation in report - but will use for this message
  # TODO: This triggers messages for a lot of packages. Mostly because this dataframe includes all exports (such as exported data)
  # Would be nice to know if the export is a function or not, or refine which types should actually trigger messages
  if(any(exports_doc_df$is_documented == FALSE) && isTRUE(verbose)){
    docs_missing <- exports_doc_df %>% dplyr::filter(.data$is_documented == FALSE)
    exports_missing <- unique(docs_missing$export) %>% paste(collapse = "\n")
    code_files_missing <- unique(docs_missing$code_file) %>% paste(collapse = ", ")
    message(glue::glue("In package `{basename(pkg_source_path)}`, the R scripts ({code_files_missing}) are missing documentation for the following exports: \n{exports_missing}"))
  }

  # This maps all functions to tests (not just exports) - this was intentional in case we eventually want all functions
  test_mapping_df <- map_tests_to_functions(pkg_source_path)

  func_tests_doc_df <- exports_doc_df %>% dplyr::left_join(test_mapping_df, by = c("export" = "pkg_function")) %>%
    dplyr::select("exported_function" = "export", everything())

  func_tests_doc_df <- func_tests_doc_df %>% dplyr::select(-"is_documented")

  # write results to RDS
  if(!is.null(results_dir)){
    saveRDS(
      func_tests_doc_df,
      get_result_path(results_dir, "export_doc.rds")
    )
  }


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
    file_text <- readLines(file_path) %>% paste(collapse = "\n")

    result <- list()
    for(func in funcs){
      func_search <- paste0("\\Q", func, "\\E")
      if(isTRUE(func_declaration)){
        pattern <- paste0(paste0("^\\s*", func_search, "\\s*(<\\-|=)\\s*function\\s*.*"), "|",
                          paste0("^\\s*setGeneric\\s*\\(\\s*[\"|']", func_search, "[\"|'].*")
        )
      }else{
        pattern <- paste0("(?:\\(|\\s|^)(", func_search, "\\s*\\()")
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

  # Get exports
  exports <- get_exports(pkg_source_path)

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
    warning(glue::glue("no testing directory found at {pkg_source_path}"))
    return(NULL)
  }

  test_dir_ls <- fs::dir_ls(test_dir_outer) %>% as.character()
  test_dirs <- test_dir_ls[grep("^(/[^/]+)+/testthat$", test_dir_ls)]
  if(length(test_dirs) == 0){
    message(glue::glue("no `testthat` directory found at {test_dir_outer}"))
  }

  # Look for cases of test_that & describe/it in other directories
  other_dirs <- pkg_dir_ls[-grep(test_dir_outer, pkg_dir_ls)]
  tests_df <- get_tests(pkg_source_path = pkg_source_path, test_dirs = other_dirs)

  # Concatenate found test directories
  if(!rlang::is_empty(tests_df)){
    other_test_dirs <- file.path(pkg_source_path, unique(tests_df$test_dir))
    test_dirs <- c(test_dirs, other_test_dirs)
  }

  # If no sub directories are found, but test_dir_outer is not empty (e.g. MASS package)
  if(rlang::is_empty(test_dirs) && length(test_dir_outer) != 0){
    test_dirs <- test_dir_outer
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
      test_that_calls <- grep("(?:\\s|testthat::|^)test_that\\([\"|']([^\"]+)[\"|']", test_script)
      test_that_names <- test_script[test_that_calls]

      # Find it calls and extract the names of the tests
      it_calls <- grep("(?:\\s|testthat::|^)it\\([\"|']([^\"]+)[\"|']", test_script)
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
                                     ~ tibble::tibble(test_file = .y, test_name = .x))
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
#' @param pkg_source_path a file path pointing to an unpacked/untarred package directory
#'
#' @keywords internal
map_tests_to_functions <- function(pkg_source_path){

  test_dirs <- get_testing_dir(pkg_source_path)

  pkg_functions <- get_all_functions(pkg_source_path)

  if(!is.null(test_dirs)){
    pkg_func_df <- purrr::map_dfr(test_dirs, function(test_dir_x){
      func_lst <- find_function_files(
        funcs = pkg_functions,
        search_dir = test_dir_x,
        func_declaration = FALSE
      )
      if(rlang::is_empty(func_lst)) return(NULL)
      tibble::enframe(func_lst, name = "pkg_function", value = "test_file") %>% tidyr::unnest("test_file") %>%
        mutate(test_dir = fs::path_rel(test_dir_x, pkg_source_path), test_file = basename(.data$test_file))
    })
  }else{
    pkg_func_df <- tibble::tibble(
      pkg_function = pkg_functions,
      test_file = "",
      test_dir = "No tests found",
    )
  }

  # Nest test files
  func_test_df <- pkg_func_df %>% dplyr::group_by(.data$pkg_function) %>%
    dplyr::summarize(test_files = list(unique(.data$test_file)), test_dirs = list(unique(.data$test_dir))) %>%
    dplyr::ungroup()

  return(func_test_df)

}

#' list all package exports
#'
#' @inheritParams map_tests_to_functions
#'
#' @keywords internal
get_exports <- function(pkg_source_path){
  # Get exports
  exports <- unname(unlist(pkgload::parse_ns_file(pkg_source_path)[c("exports","exportMethods")]))

  # Remove specific symbols from exports
  exports <- filter_symbol_functions(exports)

  return(exports)
}


#' Remove specific symbols from vector of functions
#'
#' @inheritParams find_function_files
#'
#' @keywords internal
filter_symbol_functions <- function(funcs){
  ignore_functions <- c("\\%>\\%", "\\$", "\\[\\[", "\\[", "\\+")
  pattern <- paste0("^(", paste(ignore_functions, collapse = "|"), ")$")
  funcs_return <- grep(pattern, funcs, value = TRUE, invert = TRUE)
  return(funcs_return)
}

#' list all package functions
#'
#' @inheritParams map_tests_to_functions
#'
#' @keywords internal
get_all_functions <- function(pkg_source_path){

  # Get exports
  exports <- get_exports(pkg_source_path)

  # Get all defined functions (following syntax: func <- function(arg), or setGeneric("func"))
  r_files <- list.files(file.path(pkg_source_path, "R"), full.names = TRUE)
  pkg_functions <- purrr::map(r_files, function(r_file_i) {
    file_text <- readLines(r_file_i) %>% suppressWarnings()
    pattern <- paste0("^\\s*([[:alnum:]_\\.]+)\\s*(<\\-|=)\\s*function\\s*.*", "|",
                      "^\\s*setGeneric\\s*\\(\\s*[\"|']([[:alnum:]_\\.]+)[\"|'].*")
    function_calls <- file_text[grepl(pattern, file_text)]
    function_names <- gsub(pattern, "\\1\\3", function_calls)
    function_names
  })
  pkg_functions <- purrr::reduce(pkg_functions, c)

  all_functions <- c(pkg_functions, exports) %>% unique()

  # Remove specific symbols from functions/exports - done again here in case they are reintroduced
  all_functions <- filter_symbol_functions(all_functions)

  return(all_functions)
}

