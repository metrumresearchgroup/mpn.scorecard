
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

  # Check that package is valid
  check_trac_is_possible(pkg_source_path)

  # Get data.frame of exported functions
  exports_df <- get_exports(pkg_source_path)

  # Locate script for each exported function
  exports_df <- map_functions_to_scripts(exports_df, pkg_source_path, verbose)

  # Map all Rd files to functions, then join back to exports
  exports_df <- map_functions_to_docs(exports_df, pkg_source_path, verbose)

  # This maps all functions to tests, then join back to exports
  exports_df <- map_tests_to_functions(exports_df, pkg_source_path, verbose)

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


check_trac_is_possible <- function(pkg_source_path){
  r_dir <- file.path(pkg_source_path, "R")
  if(!fs::dir_exists(r_dir)){
    stop("an R directory is needed to create a traceability matrix")
  }
}


#' Get all exported functions and map them to R script where they are defined
#'
#' @param exports_df data.frame with a column, named `exported_function`,
#'   containing the names of all exported functions. Can also have other columns
#'   (which will be returned unmodified).
#' @param pkg_source_path a file path pointing to an unpacked/untarred package directory
#' @inheritParams make_traceability_matrix
#'
#' @return A data.frame with the columns `exported_function` and `code_file`.
#'
#' @keywords internal
map_functions_to_scripts <- function(exports_df, pkg_source_path, verbose){

  # Search for scripts functions are defined in
  funcs_df <- get_toplevel_assignments(pkg_source_path)

  if(nrow(funcs_df) == 0){
    # This shouldn't be triggered, and either indicates a bug in `get_toplevel_assignments`,
    # or an unexpected package setup that we may want to support.
    warning("No top level assignments were found in the R/ directory. Make sure this was expected.")
  }

  exports_df <- dplyr::left_join(exports_df, funcs_df, by = c("exported_function" = "func"))

  if (any(is.na(exports_df$code_file))) {
    if(isTRUE(verbose)) {
      missing_from_files <- exports_df$exported_function[is.na(exports_df$code_file)] %>%
        paste(collapse = "\n")
      message(glue::glue("The following exports were not found in R/ for {basename(pkg_source_path)}:\n{missing_from_files}\n\n"))
    }
  }

  return(exports_df)
}

#' Map all Rd files to the functions they describe
#'
#' @inheritParams map_functions_to_scripts
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
#' @inheritParams map_functions_to_scripts
#'
#' @keywords internal
get_testing_dir <- function(pkg_source_path, verbose){
  checkmate::assert_directory_exists(pkg_source_path)

  pkg_dir_ls <- list.dirs(pkg_source_path, recursive = FALSE)
  test_dir_outer <- pkg_dir_ls[grep("^(/[^/]+)+/tests$", pkg_dir_ls)]
  if(length(test_dir_outer) == 0){
    warning(glue::glue("no testing directory found at {pkg_source_path}"))
    return(NULL)
  }

  test_dir_ls <- fs::dir_ls(test_dir_outer) %>% as.character()
  test_dirs <- test_dir_ls[grep("^(/[^/]+)+/testthat$", test_dir_ls)]
  if(length(test_dirs) == 0 && isTRUE(verbose)){
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
#' @inheritParams map_functions_to_scripts
#' @return Returns the data.frame passed to `exports_df`, with `test_files` and
#'   `test_dirs` columns appended. These columns will contain the paths to the
#'   test files that call the associated exported functions.
#'
#' @keywords internal
map_tests_to_functions <- function(exports_df, pkg_source_path, verbose){

  # collect test directories and test files
  test_dirs <- get_testing_dir(pkg_source_path, verbose)
  if (is.null(test_dirs)) {
    # TODO: could skip this whole check if we refactor to pass in test_dirs instead of using get_testing_dir(),
    #   (instead just check if passed dirs exist and error if not?)
    if (isTRUE(verbose)) {
      message(glue::glue("No testing directories found in {pkg_source_path}"))
    }
    return(dplyr::mutate(exports_df, "test_files" = list(""), "test_dirs" = list("No tests found")))
  }

  test_files <- fs::dir_ls(test_dirs, glob = "*.R")
  if (length(test_files) == 0) {
    if (isTRUE(verbose)) {
      message(glue::glue("No test files found in {paste(test_dirs, collapse = ', ')} for {pkg_source_path}"))
    }
    return(dplyr::mutate(exports_df, "test_files" = list(""), "test_dirs" = list("No tests found")))
  }

  # map over test files and parse all functions called in those files
  func_test_df <- purrr::map_dfr(test_files, function(test_file.i) {
    parsed_df <- test_file.i %>%
      parse(keep.source = TRUE) %>%
      utils::getParseData() %>%
      tibble::as_tibble()

    # NOTE: this filters to "SYMBOL_FUNCTION_CALL", but also to "SYMBOL" to
    #   account for non-standard function calls (e.g. purrr, do.call, etc.).
    #   Unrelated symbols (and functions) are filtered out by the left_join below.
    uniq_funcs <- parsed_df %>%
      dplyr::filter(.data$token %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL")) %>%
      dplyr::pull("text") %>%
      unique()

    if (length(uniq_funcs) == 0) {
      # probably only possible if the file is basically empty
      return(data.frame(func = character(), test_file = character(), test_dir = character()))
    }

    test_dir.i <- fs::path_rel(dirname(test_file.i), pkg_source_path)
    return(data.frame(
      func = uniq_funcs,
      test_file = rep(basename(test_file.i), length(uniq_funcs)),
      test_dir = rep(test_dir.i, length(uniq_funcs))
    ))
  })

  # collapse to one row per unique function
  func_test_df <- func_test_df %>% dplyr::group_by(.data$func) %>%
    dplyr::summarize(test_files = list(unique(.data$test_file)), test_dirs = list(unique(.data$test_dir))) %>%
    dplyr::ungroup()

  # left_join to exports_df to filter back to only this package's exported functions
  exports_df <- dplyr::left_join(exports_df, func_test_df, by = c("exported_function" = "func"))

  # message if any exported functions aren't documented
  if (any(is.na(exports_df$test_files)) && isTRUE(verbose)) {
    docs_missing <- exports_df %>% dplyr::filter(is.na(.data$test_files))
    exports_missing <- unique(docs_missing$exported_function) %>% paste(collapse = "\n")
    message(glue::glue("In package `{basename(pkg_source_path)}`, the following exported functions were not found in any tests in {paste(test_dirs, collapse = ', ')}: \n{exports_missing}"))
  }

  return(exports_df)

}

#' list all package exports
#'
#' @inheritParams map_tests_to_functions
#'
#' @return data.frame, with one column `exported_function`, that can be passed
#'   to all downstream map_* helpers
#'
#' @keywords internal
get_exports <- function(pkg_source_path){
  # Get exports
  nsInfo <- pkgload::parse_ns_file(pkg_source_path)
  exports <- unname(unlist(nsInfo[c("exports","exportMethods")]))

  # Look for export patterns
  if(!rlang::is_empty(nsInfo$exportPatterns)){
    all_functions <- get_toplevel_assignments(pkg_source_path)$func
    for (p in nsInfo$exportPatterns) {
      exports <- c(all_functions[grep(pattern = p, all_functions)], exports)
    }
  }

  # Remove specific symbols from exports
  exports <- unique(exports)
  exports <- filter_symbol_functions(exports)

  if(rlang::is_empty(exports)){
    stop(glue::glue("No exports found in package {basename(pkg_source_path)}"))
  }

  return(tibble::tibble(exported_function = exports))
}


#' Remove specific symbols from vector of functions
#'
#' @param funcs vector of functions to filter
#'
#' @keywords internal
filter_symbol_functions <- function(funcs){
  ignore_patterns <- c("\\%>\\%", "\\$", "\\[\\[", "\\[", "\\+", "\\%", "<-")
  pattern <- paste0("(", paste(ignore_patterns, collapse = "|"), ")")
  funcs_return <- grep(pattern, funcs, value = TRUE, invert = TRUE)
  return(funcs_return)
}

#' list all top-level objects defined in the package code
#'
#' This is primarily for getting all _functions_, but it also returns top-level
#' declarations, regardless of type. This is intentional, because we also want
#' to capture any global variables or anything else that could be potentially
#' exported by the package.
#'
#' @inheritParams map_functions_to_scripts
#'
#' @return A data.frame with the columns `func` and `code_file` with a row for
#'   every top-level object defined in the package.
#'
#' @keywords internal
get_toplevel_assignments <- function(pkg_source_path){
  r_files <- list.files(
    file.path(pkg_source_path, "R"),
    full.names = TRUE, pattern = "\\.(?i)[rsq]$", recursive = TRUE
  )
  pkg_functions <- purrr::map_dfr(r_files, function(r_file_i) {
    exprs <- tryCatch(parse(r_file_i), error = identity)
    if (inherits(exprs, "error")) {
      warning("Failed to parse ", r_file_i, ": ", conditionMessage(exprs))
      return(tibble::tibble(func = character(), code_file = character()))
    }

    calls <- purrr::keep(as.list(exprs), function(e) {
      if (is.call(e)) {
        op <- as.character(e[[1]])
        return(length(op) == 1 && op %in% c("<-", "=", "setGeneric"))
      }
      return(FALSE)
    })
    lhs <- purrr::map(calls, function(e) {
      name <- as.character(e[[2]])
      if (length(name) == 1) {
        return(name)
      }
    })

    function_names <- unlist(lhs) %||% character()
    if (length(function_names) == 0 ) {
      return(tibble::tibble(func = character(), code_file = character()))
    }
    return(tibble::tibble(
      func = function_names,
      code_file = rep(fs::path_rel(r_file_i, pkg_source_path), length(function_names))
    ))
  })
  # TODO: do we need to check if there are any funcs defined in multiple files?

  return(pkg_functions)
}

