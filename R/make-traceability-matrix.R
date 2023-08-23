
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

  # Locate script for each exported function
  exports_df <- find_export_script(pkg_source_path)

  # Map all Rd files to functions, then join back to exports
  exports_df <- map_functions_to_docs(exports_df, pkg_source_path, verbose)

  # This maps all functions to tests, then join back to exports
  exports_df <- map_tests_to_functions(exports_df, pkg_source_path)

  # write results to RDS
  exports_df <- dplyr::select(exports_df, "exported_function", everything())
  if(!is.null(results_dir)){
    saveRDS(
      exports_df,
      get_result_path(results_dir, "export_doc.rds")
    )
  }


  return(exports_df)
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

#' Get all exported functions and map them to R script where they are defined
#'
#' @param pkg_source_path a file path pointing to an unpacked/untarred package directory
#'
#' @value A data.frame with the columns `exported_function` and `code_file`.
#'
#' @keywords internal
find_export_script <- function(pkg_source_path){

  # Get exported functions
  exports <- get_exports(pkg_source_path)

  # Search for scripts functions are defined in
  export_lst <- find_function_files(funcs = exports, search_dir = file.path(pkg_source_path, "R"))

  # convert to dataframe and format code_file column
  export_df <- export_lst %>% tibble::enframe(name = "exported_function", value = "code_file") %>% tidyr::unnest(cols = "code_file") %>%
    mutate(code_file = paste0("R/", basename(.data$code_file)))

  return(export_df)
}

#' Map all Rd files to the functions they describe
#'
#' @param exports_df data.frame with a column, named `exported_function`,
#'   containing the names of all exported functions. Can also have other columns
#'   (which will be returned unmodified).
#' @inheritParams find_export_script
#' @inheritParams make_traceability_matrix
#'
#' @return Returns the data.frame passed to `exports_df`, with a `documentation`
#'   column appended. This column will contain the path to the `.Rd` files in
#'   `man/` that document the associated exported functions.
#'
#' @keywords internal
map_functions_to_docs <- function(exports_df, pkg_source_path, verbose) {

  rd_files <- list.files(file.path(pkg_source_path, "man"), full.names = TRUE)
  rd_files <- rd_files[grep("\\.Rd$", rd_files)]
  if (length(rd_files) == 0) {
    if(isTRUE(verbose)){
      message(glue::glue("No documentation was found in `man/` for package `{basename(pkg_source_path)}`"))
    }
    return(dplyr::mutate(exports_df, "documentation" = NA))
  }

  docs_df <- purrr::map_dfr(rd_files, function(rd_file.i) {
    rd_lines <- readLines(rd_file.i) %>% suppressWarnings()

    # Get Rd file and aliases for exported functions
    function_names_lines <- rd_lines[grep("(^\\\\alias)|(^\\\\name)", rd_lines)]
    function_names <- unique(gsub("\\}", "", gsub("((\\\\alias)|(\\\\name))\\{", "", function_names_lines)))

    man_name <- paste0("man/", basename(rd_file.i))

    data.frame(
      pkg_function = function_names,
      documentation = rep(man_name, length(function_names))
    )
  })

  # if any functions are aliased in more than 1 Rd file, collapse those Rd files to a single row
  # TODO: is this necessary? is it even possible to have this scenario without R CMD CHECK failing?
  docs_df <- docs_df %>%
    dplyr::group_by(.data$pkg_function) %>%
    dplyr::summarize(documentation = paste(unique(.data$documentation), collapse = ", ")) %>%
    dplyr::ungroup()

  # join back to filter to only exported functions
  exports_df <- dplyr::left_join(exports_df, docs_df, by = c("exported_function" = "pkg_function"))

  # message if any exported functions aren't documented
  if (any(is.na(exports_df$documentation)) && isTRUE(verbose)) {
    docs_missing <- exports_df %>% dplyr::filter(is.na(.data$documentation))
    exports_missing <- unique(docs_missing$exported_function) %>% paste(collapse = "\n")
    code_files_missing <- unique(docs_missing$code_file) %>% paste(collapse = ", ")
    message(glue::glue("In package `{basename(pkg_source_path)}`, the R scripts ({code_files_missing}) are missing documentation for the following exports: \n{exports_missing}"))
  }

  return(exports_df)
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


#' Map test files and directories to all functions
#'
#' @inheritParams map_functions_to_docs
#' @return Returns the data.frame passed to `exports_df`, with `test_files` and
#'   `test_dirs` columns appended. These columns will contain the paths to the
#'   test files that call the associated exported functions.
#'
#' @keywords internal
map_tests_to_functions <- function(exports_df, pkg_source_path){

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

  exports_df <- exports_df %>% dplyr::left_join(func_test_df, by = c("exported_function" = "pkg_function"))

  return(exports_df)

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

