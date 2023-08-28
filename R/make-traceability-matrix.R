
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
#' @param pkg_source_path a file path pointing to an unpacked/untarred package directory
#'
#' @return A data.frame with the columns `exported_function` and `code_file`.
#'
#' @keywords internal
map_functions_to_scripts <- function(exports_df, pkg_source_path, verbose){

  # Search for scripts functions are defined in
  funcs_df <- get_all_functions(pkg_source_path)

  exports_df <- dplyr::left_join(exports_df, funcs_df, by = c("exported_function" = "func"))

  if (any(is.na(exports_df$code_file))) {
    if(isTRUE(verbose)) {
      missing_from_files <- exports_df$exported_function[is.na(exports_df$code_file)]
      message(glue::glue("The following exports were not found in R/ for {basename(pkg_source_path)}:\n{paste(missing_from_files, collapse = '\n')}"))
    }
  }

  return(exports_df)
}

#' Map all Rd files to the functions they describe
#'
#' @param exports_df data.frame with a column, named `exported_function`,
#'   containing the names of all exported functions. Can also have other columns
#'   (which will be returned unmodified).
#' @inheritParams map_functions_to_scripts
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
map_tests_to_functions <- function(exports_df, pkg_source_path, verbose){

  # collect test directories and test files
  test_dirs <- get_testing_dir(pkg_source_path)
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
    all_functions <- get_all_functions(pkg_source_path)$func
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



#' list all functions defined in the package code
#'
#' @inheritParams map_tests_to_functions
#'
#' @details
#' Inspired from pkgload::load_code
#'
#' @return A data.frame with the columns `func` and `code_file` with a row for
#'   every function defined in the package.
#'
#' @keywords internal
get_all_functions <- function(pkg_source_path){

  # Set up paths and encoding
  path <- pkgload::pkg_path(pkg_source_path)
  package <- pkgload::pkg_name(path)
  file_encoding <- pkgload::pkg_desc(path)$get("Encoding")
  path_r <- pkgload::package_file("R", path = path)
  r_files <- list.files(path_r, full.names = TRUE)

  # Set encoding to ASCII if it is not explicitly defined
  if (is.na(file_encoding)) {
    file_encoding <- "ASCII"
  }

  # Set up environment
  env <- create_pkg_env(pkg_source_path)
  on.exit(rm(list = ls(envir = env), envir = env))

  # Source functions in `env` and map to R script
  mapped_functions <- withr::with_dir(path, source_pkg_code(r_files, file_encoding, env))

  return(mapped_functions)
}


#' Create environment for sourcing package functions
#'
#' @inheritParams map_tests_to_functions
#'
#' @details
#' Inpsired from pkload:::create_ns_env
#'
#' @returns an environment
#' @keywords internal
create_pkg_env <- function(pkg_source_path){
  path <- pkgload::pkg_path(pkg_source_path)
  package <- pkgload::pkg_name(pkg_source_path)
  version <- pkgload::pkg_version(pkg_source_path)
  name <- paste(package,  version, "MPN_SCORECARD", sep = "_")

  env <- new.env(parent = .BaseNamespaceEnv, hash = TRUE)
  methods::setPackageName(name, env)
  return(env)
}

#' Source R files into environment with encoding
#'
#' @param files vector of files to source
#' @param file_encoding encoding to be assumed for input strings.
#' @param envir an environment to store the sourced functions
#'
#' @details
#' Inspired from pkgload internal functions: source_one, source_many and read_lines_enc
#'
#' @keywords internal
source_pkg_code <- function(files, file_encoding = "unknown", envir){
  stopifnot(is.environment(envir))

  purrr::map_dfr(files, function(file){
    stopifnot(file.exists(file))
    rlang::try_fetch({
      lines <- readLines(file, warn = FALSE, encoding = file_encoding)
      srcfile <- srcfilecopy(file, lines, file.info(file)[1, "mtime"],
                             isFile = TRUE)
      exprs <- parse(text = lines, n = -1, srcfile = srcfile)
      n <- length(exprs)
      if (n == 0L){
        return(tibble::tibble(func = character(), code_file = character()))
      }

      current_funcs <- ls(envir = envir)
      for (i in seq_len(n)) {
        tryCatch(eval(exprs[i], envir), error = function(e) e)
      }
      path <- file.path(basename(dirname(file)), basename(file))
      funcs_per_script <- setdiff(ls(envir = envir), current_funcs)

      if(length(funcs_per_script) == 0){
        return(tibble::tibble(func = character(), code_file = character()))
      }

      tibble::tibble(func = funcs_per_script, code_file = path)
    },
    error = function(cnd) {
      path <- file.path(basename(dirname(file)), basename(file))
      msg <- paste0("Failed to load {.file {path}}")
      cli::cli_abort(msg, parent = cnd)
    })
  })

}

