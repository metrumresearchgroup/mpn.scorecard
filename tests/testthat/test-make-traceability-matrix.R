
describe("creating extra notes", {

  it("make_traceability_matrix - success integration test", {

    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file
    trac_mat <- make_traceability_matrix(pkg_tar_path = pkg_tar_x, result_dir_x)

    export_doc_path <- get_result_path(result_dir_x, "export_doc.rds")
    expect_true(fs::file_exists(export_doc_path))
    on.exit(fs::file_delete(export_doc_path), add = TRUE)

    # Confirm values - documentation
    expect_equal(unique(trac_mat$documentation), "man/myfunction.Rd")
    expect_equal(
      trac_mat %>% tidyr::unnest(test_files) %>% pull(test_files) %>% unique(),
      "test-myscript.R"
    )
  })


  it("make_traceability_matrix - missing documentation", {
    # Bad package - no documentation (at all)
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "fail_func_syntax")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file

    # Check for two separate notes
    res <- testthat::evaluate_promise(
      trac_mat <- make_traceability_matrix(pkg_tar_path = pkg_tar_x, result_dir_x, verbose = TRUE)
    )
    expect_equal(
      res$messages,
      c(glue::glue("No documentation was found in `man/` for package `{pkg_setup_select$pkg_name}`\n\n"),
        glue::glue("In package `{pkg_setup_select$pkg_name}`, the R scripts (R/myscript.R) are missing documentation for the following exports: \nmyfunction\n\n"))
    )

    export_doc_path <- get_result_path(result_dir_x, "export_doc.rds")
    expect_true(fs::file_exists(export_doc_path))
    on.exit(fs::file_delete(export_doc_path), add = TRUE)

    # Confirm values - documentation
    expect_true(is.na(unique(trac_mat$documentation)))
    expect_equal(
      trac_mat %>% tidyr::unnest(test_files) %>% pull(test_files) %>% unique(),
      "test-myscript.R"
    )
  })


  it("make_traceability_matrix - no test suite", {
    # Bad package - no documentation (at all)
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_no_test_suite")
    result_dir_x <- pkg_setup_select$pkg_result_dir
    pkg_tar_x <- pkg_setup_select$tar_file

    # Check for two separate notes
    expect_warning(
      trac_mat <- make_traceability_matrix(pkg_tar_path = pkg_tar_x, result_dir_x),
      "no testing directory found at"
    )

    export_doc_path <- get_result_path(result_dir_x, "export_doc.rds")
    on.exit(fs::file_delete(export_doc_path), add = TRUE)

    # Confirm values
    expect_equal(
      trac_mat %>% tidyr::unnest(test_files) %>% pull(test_files) %>% unique(),
      ""
    )
    expect_equal(
      trac_mat %>% tidyr::unnest(test_dirs) %>% pull(test_dirs) %>% unique(),
      "No tests found"
    )
  })


  it("identify functions and the script they're coded in", {
    # `find_function_files` and `get_all_functions` follow slightly different forms of regex for finding functions and extracting their names
    # Both must be tested separately to ensure the regex matches up

    # Test `find_function_files`
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    r_dir <- file.path(pkg_setup_select$pkg_dir, "R")

    func_lines1 <- c(
      "myfunc1 <- function(x) {x + 1}",
      "myfunc2<-function(x) {x + 1}",
      "myfunc3 =function(x) {x + 1}"
    )
    func_lines2 <- c(
      "setGeneric(\"myfunc4\")",
      "setGeneric('myfunc5')",
      "setGeneric ( 'myfunc6' )"
    )
    func_names <- paste0("myfunc", 1:6)

    temp_file1 <- file.path(r_dir, "myscript1.R")
    fs::file_create(temp_file1); on.exit(fs::file_delete(temp_file1), add = TRUE)
    writeLines(func_lines1, temp_file1)

    temp_file2 <- file.path(r_dir, "myscript2.R")
    fs::file_create(temp_file2); on.exit(fs::file_delete(temp_file2), add = TRUE)
    writeLines(func_lines2, temp_file2)

    funcs_found <- find_function_files(func_names, search_dir = r_dir)

    # Confirm all function variants are found
    expect_equal(names(funcs_found), func_names)
    # Confirm correct location
    expect_equal(basename(unique(unlist(funcs_found))), c("myscript1.R", "myscript2.R"))

    # Test get_all_functions - also contains the original `myfunction`
    expect_equal(
      get_all_functions(pkg_setup_select$pkg_dir),
      c("myfunction", func_names)
    )
  })


  it("find_export_script", {
    # `find_export_script` should work regardless of documentation presence (in `man/`)
    template_df <- tibble::tibble(export = "myfunction", code_file = "R/myscript.R")

    # Documented correctly
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    expect_equal(
      template_df,
      find_export_script(pkg_setup_select$pkg_dir)
    )

    # Not documented, but still exported
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "fail_func_syntax")
    expect_equal(
      template_df,
      find_export_script(pkg_setup_select$pkg_dir)
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

    map_tests_df <- map_tests_to_functions(pkg_setup_select$pkg_dir) %>% tidyr::unnest(c(test_files, test_dirs))
    expect_equal(map_tests_df$pkg_function, "myfunction")
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

    map_tests_df <- map_tests_to_functions(pkg_setup_select$pkg_dir) %>% tidyr::unnest(c(test_files, test_dirs))
    expect_equal(map_tests_df$pkg_function, rep("myfunction", 2))
    expect_equal(map_tests_df$test_files, c("test-myscript.R", "test-new_tests.R"))
    expect_equal(map_tests_df$test_dirs, c("tests/testthat", "inst/other_tests"))

  })


  it("correctly finding relevant tests per export", {
    pkg_setup_select <- pkg_dirs$pkg_setups_df %>% dplyr::filter(pkg_type == "pass_success")
    test_dir <- file.path(pkg_setup_select$pkg_dir, "tests", "testthat")

    # Examples that should -not- get picked up by find_function_files
    test_lines1 <- c(
      "# comment about myfunction",
      "do.call(myfunction, list(1))", # remove if support for this is added later
      "myfunction2 <- myfunction",
      "myfunction2(1)"
    )

    func_name <- "myfunction"

    temp_file1 <- file.path(test_dir, "test-myscript-fake.R")
    fs::file_create(temp_file1); on.exit(fs::file_delete(temp_file1), add = TRUE)
    writeLines(test_lines1, temp_file1)

    # Search for test files - expect only original test

    # Test internal function
    test_files <- find_function_files("myfunction", search_dir = test_dir, func_declaration = FALSE)
    expect_equal(length(test_files$myfunction), 1)

    # Test overall function
    test_df <- map_tests_to_functions(pkg_setup_select$pkg_dir) %>% tidyr::unnest("test_files")
    expect_equal(unique(test_df$test_files), "test-myscript.R")
  })

})
