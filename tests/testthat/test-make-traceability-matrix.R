
describe("Traceability Matrix", {

  it("make_traceability_matrix - success integration test", {

    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file
    trac_mat <- make_traceability_matrix(pkg_tar_path = pkg_tar_x, result_dir_x)

    export_doc_path <- get_result_path(result_dir_x, "export_doc.rds")
    expect_true(fs::file_exists(export_doc_path))
    on.exit(fs::file_delete(export_doc_path), add = TRUE)

    # Confirm values - documentation
    expect_equal(unique(trac_mat$exported_function), "myfunction")
    expect_equal(unique(trac_mat$code_file), "R/myscript.R")
    expect_equal(unique(trac_mat$documentation), "man/myfunction.Rd")
    expect_equal(
      trac_mat %>% tidyr::unnest(test_files) %>% pull(test_files) %>% unique(),
      "test-myscript.R"
    )
  })


  it("make_traceability_matrix - missing documentation integration test", {
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_no_docs")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file

    expect_message(
      trac_mat <- make_traceability_matrix(pkg_tar_path = pkg_tar_x, result_dir_x, verbose = TRUE),
      as.character(glue::glue("No documentation was found in `man/` for package `{pkg_setup_select$pkg_name}`\n\n"))
    )
    export_doc_path <- get_result_path(result_dir_x, "export_doc.rds")
    expect_true(fs::file_exists(export_doc_path))
    on.exit(fs::file_delete(export_doc_path), add = TRUE)

    # Confirm values
    expect_equal(unique(trac_mat$exported_function), "myfunction")
    expect_equal(unique(trac_mat$code_file), "R/myscript.R")
    expect_true(is.na(unique(trac_mat$documentation)))
    expect_equal(
      trac_mat %>% tidyr::unnest(test_files) %>% pull(test_files) %>% unique(),
      "test-myscript.R"
    )
  })


  it("make_traceability_matrix - export patterns integration test", {
    # Normal behavior
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_export_patterns")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file

    trac_mat <- make_traceability_matrix(pkg_tar_path = pkg_tar_x, result_dir_x, verbose = TRUE)
    export_doc_path <- get_result_path(result_dir_x, "export_doc.rds")
    expect_true(fs::file_exists(export_doc_path))
    on.exit(fs::file_delete(export_doc_path), add = TRUE)

    # Confirm values
    expect_equal(unique(trac_mat$exported_function), "myfunction")
    expect_equal(unique(trac_mat$code_file), "R/myscript.R")
    expect_equal(unique(trac_mat$documentation), "man/myfunction.Rd")
    expect_equal(
      trac_mat %>% tidyr::unnest(test_files) %>% pull(test_files) %>% unique(),
      "test-myscript.R"
    )

    # Syntax error (cant find R script)
    # Add syntax error to new script
    r_script <- file.path(pkg_setup_select$pkg_dir, "R", "myscript_error.R")
    on.exit(fs::file_delete(r_script), add = TRUE)
    func_lines <- "myfunction2 <- function(x { x + 1"
    writeLines(func_lines, r_script)

    # these both generate this warning because this test hits the exportPattern
    # path in get_exports, which calls get_toplevel_assignments() (the source of this warning)
    expect_warning({
        exports_df <- get_exports(pkg_setup_select$pkg_dir)
        exports_df <- map_functions_to_scripts(exports_df, pkg_setup_select$pkg_dir)
      },
      "Failed to parse"
    )

    expect_equal(unique(exports_df$exported_function), "myfunction")
    expect_equal(unique(exports_df$code_file), "R/myscript.R")

  })


  it("make_traceability_matrix - cant link exports integration test", {
    # Bad package - no documentation (at all)
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "fail_func_syntax")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file

    # Check for two separate notes due to parsing error
    # - missing documentation
    # - can't find R script
    res <- testthat::evaluate_promise(
      trac_mat <- make_traceability_matrix(pkg_tar_path = pkg_tar_x, result_dir_x, verbose = TRUE)
    )
    expect_equal(
      res$messages,
      c(
        glue::glue("The following exports were not found in R/ for {pkg_setup_select$pkg_name}:\n{trac_mat$exported_function}\n\n\n"),
        glue::glue("No documentation was found in `man/` for package `{pkg_setup_select$pkg_name}`\n\n")
      )
    )
    expect_true(grepl("Failed to parse", res$warnings))

    export_doc_path <- get_result_path(result_dir_x, "export_doc.rds")
    expect_true(fs::file_exists(export_doc_path))
    on.exit(fs::file_delete(export_doc_path), add = TRUE)

    # Confirm values
    expect_equal(unique(trac_mat$exported_function), "myfunction")
    expect_true(is.na(unique(trac_mat$code_file)))
    expect_true(is.na(unique(trac_mat$documentation)))
    expect_equal(
      trac_mat %>% tidyr::unnest(test_files) %>% pull(test_files) %>% unique(),
      "test-myscript.R"
    )
  })


  it("make_traceability_matrix - no test suite integration test", {
    # No test suite
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_no_test_suite")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file

    expect_warning(
      trac_mat <- make_traceability_matrix(pkg_tar_path = pkg_tar_x, result_dir_x),
      "no testing directory found at"
    )

    export_doc_path <- get_result_path(result_dir_x, "export_doc.rds")
    on.exit(fs::file_delete(export_doc_path), add = TRUE)

    # Confirm values - empty strings will show up as empty cells when formatted with format_traceability_matrix
    expect_equal(
      trac_mat %>% tidyr::unnest(test_files) %>% pull(test_files) %>% unique(),
      ""
    )
    expect_equal(
      trac_mat %>% tidyr::unnest(test_dirs) %>% pull(test_dirs) %>% unique(),
      "No tests found"
    )
  })


  it("make_traceability_matrix - no R directory or exported functions integration test", {
    # No R directory
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_no_R_dir")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file

    expect_error(
      trac_mat <- make_traceability_matrix(pkg_tar_path = pkg_tar_x, result_dir_x),
      "an R directory is needed to create a traceability matrix"
    )

    # No R functions found
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_no_functions")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file

    expect_error(
      trac_mat <- make_traceability_matrix(pkg_tar_path = pkg_tar_x, result_dir_x),
      glue("No exports found in package {basename(pkg_setup_select$pkg_dir)}")
    )
  })


  it("get_exports", {
    # Test individual exports
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    exports_df <- get_exports(pkg_setup_select$pkg_dir)
    expect_equal(exports_df$exported_function, "myfunction")

    # Test export patterns
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_export_patterns")
    exports_df <- get_exports(pkg_setup_select$pkg_dir)
    expect_equal(exports_df$exported_function, "myfunction")
  })


  it("get_toplevel_assignments: identify functions and the script they're coded in", {
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    r_dir <- file.path(pkg_setup_select$pkg_dir, "R")

    func_lines1 <- c(
      "myfunc1 <- function(x) {x + 1}", # normal
      "myfunc2<-function(x) {x + 1}", # spacing
      "myfunc3 =function(x) {x + 1}", # equal sign
      "myfunc4 <-
      function(x) {x + 1}" # multi-line declaration
    )
    func_lines2 <- c(
      "setGeneric(\"myfunc5\", function(x) attributes(x))", # setGeneric
      "setGeneric('myfunc6', plot)", # different quotes, existing function
      "setGeneric ( 'myfunc7', function(x) mtcars)", # spacing
      "myfunc8 <- 'This is not a function, but should still be captured'" # non-function top-level assignment
    )
    func_names <- paste0("myfunc", 1:8)

    temp_file1 <- file.path(r_dir, "myscript1.R")
    fs::file_create(temp_file1); on.exit(fs::file_delete(temp_file1), add = TRUE)
    writeLines(func_lines1, temp_file1)

    temp_file2 <- file.path(r_dir, "myscript2.R")
    fs::file_create(temp_file2); on.exit(fs::file_delete(temp_file2), add = TRUE)
    writeLines(func_lines2, temp_file2)

    # Test get_toplevel_assignments - also contains the original `myfunction`
    funcs_found <- get_toplevel_assignments(pkg_setup_select$pkg_dir)
    expect_equal(
      funcs_found$func,
      c("myfunction", func_names)
    )

    # Confirm correct location
    expect_equal(basename(unique(funcs_found$code_file)), c("myscript.R","myscript1.R", "myscript2.R"))
  })


  it("map_functions_to_scripts", {
    # `map_functions_to_scripts` should work regardless of documentation presence (in `man/`)
    template_df <- tibble::tibble(exported_function = "myfunction", code_file = "R/myscript.R")

    # Documented correctly
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    exports_df <- get_exports(pkg_setup_select$pkg_dir)
    expect_equal(
      template_df,
      map_functions_to_scripts(exports_df, pkg_setup_select$pkg_dir, verbose = FALSE)
    )

    # Not documented, but still exported
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_no_docs")
    exports_df <- get_exports(pkg_setup_select$pkg_dir)
    expect_equal(
      template_df,
      map_functions_to_scripts(exports_df, pkg_setup_select$pkg_dir, verbose = FALSE)
    )
  })


  it("gathering of tests and mapping to functions", {

    # Tests `get_testing_dir`, `get_tests`, and `map_tests_df`

    # Test traditional `tests/testthat` location
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")

    testing_dirs <- get_testing_dir(pkg_setup_select$pkg_dir)
    expect_equal(
      fs::path_rel(testing_dirs, pkg_setup_select$pkg_dir),
      "tests/testthat"
    )
    tests_df <- get_tests(pkg_setup_select$pkg_dir, testing_dirs)
    expect_equal(tests_df$test_file, "test-myscript.R")
    expect_equal(tests_df$test_name, "this works")
    expect_equal(tests_df$test_dir, "tests/testthat")

    exports_df <- get_exports(pkg_setup_select$pkg_dir)
    map_tests_df <- map_tests_to_functions(exports_df, pkg_setup_select$pkg_dir, verbose = FALSE) %>%
      tidyr::unnest(c(test_files, test_dirs))
    expect_equal(map_tests_df$exported_function, "myfunction")
    expect_equal(map_tests_df$test_files, "test-myscript.R")
    expect_equal(map_tests_df$test_dirs, "tests/testthat")

    # Test with multiple testing locations
    # copy tests to other location within `pkg_setup_select$pkg_dir`
    test_files <- fs::dir_ls(testing_dirs)
    other_test_dir <- file.path(pkg_setup_select$pkg_dir, "inst", "other_tests")
    fs::dir_create(other_test_dir); on.exit(fs::dir_delete(dirname(other_test_dir)), add = TRUE)
    new_test_files <- fs::file_copy(test_files, other_test_dir)
    fs::file_move(new_test_files, file.path(other_test_dir, "test-new_tests.R"))

    testing_dirs_new <- get_testing_dir(pkg_setup_select$pkg_dir)
    expect_equal(
      fs::path_rel(testing_dirs_new, pkg_setup_select$pkg_dir),
      c("tests/testthat", "inst/other_tests")
    )
    tests_df <- get_tests(pkg_setup_select$pkg_dir, testing_dirs_new)
    expect_equal(tests_df$test_file, c("test-myscript.R", "test-new_tests.R"))
    expect_equal(tests_df$test_name, rep("this works", 2))
    expect_equal(tests_df$test_dir, c("tests/testthat", "inst/other_tests"))

    exports_df <- get_exports(pkg_setup_select$pkg_dir)
    map_tests_df <- map_tests_to_functions(exports_df, pkg_setup_select$pkg_dir, verbose = FALSE) %>%
      tidyr::unnest(c(test_files, test_dirs))
    expect_equal(map_tests_df$exported_function, rep("myfunction", 2))
    expect_equal(map_tests_df$test_files, c("test-myscript.R", "test-new_tests.R"))
    expect_equal(map_tests_df$test_dirs, c("tests/testthat", "inst/other_tests"))

  })


  it("correctly finding relevant tests per export", {
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    test_dir <- file.path(pkg_setup_select$pkg_dir, "tests", "testthat")

    # Examples that should -not- get picked up by map_tests_to_functions()
    test_lines1 <- c(
      "# comment about myfunction",
      "do.call('myfunction', list(1))", # remove if support for strings in this case is added later
      "print('calling my myfunction() is the best')",
      "myfunction2(1)"
    )

    func_name <- "myfunction"

    temp_file1 <- file.path(test_dir, "test-myscript-fake.R")
    fs::file_create(temp_file1); on.exit(fs::file_delete(temp_file1), add = TRUE)
    writeLines(test_lines1, temp_file1)

    # Search for test files - expect only original test
    exports_df <- get_exports(pkg_setup_select$pkg_dir)
    test_df <- map_tests_to_functions(exports_df, pkg_setup_select$pkg_dir, verbose = FALSE) %>%
      tidyr::unnest("test_files")
    expect_equal(unique(test_df$test_files), "test-myscript.R")
  })

})
