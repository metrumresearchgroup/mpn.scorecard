#' Rendering externally scored packages
#'
#' @description
#'
#' For R packages, mpn.scorecard handles both scoring and rendering. The
#' workflow is to first score the package with [score_pkg()], then to optionally
#' generate a traceability matrix with [make_traceability_matrix()], and finally
#' to render the scorecard with [render_scorecard()].
#'
#' [render_scorecard()] also supports rendering packages that are scored outside
#' of mpn.scorecard. The scorer is responsible for preparing a results directory
#' with the set of files described below.
#'
#' @details
#'
#' ## Input files
#'
#' The following input files define results for the scorecard to render. These
#' must reside in a directory named `<package>_<version>`, following the naming
#' of the output directory returned by [score_pkg()].
#'
#'  * `<package>_<version>.pkg.json`: This file provides general information
#'     about the package being scored.  It requires the following keys:
#'
#'     * `mpn_scorecard_format`: The version of the format in which these input
#'       files are specified. This should be "1.0".
#'
#'     * `pkg_name`, `pkg_version`: The name and version of the package.
#'
#'     * `scorecard_type`: The type of package. Two types are currently
#'       recognized and receive special handling: "R" and "cli". Everything else
#'       falls back to default handling.
#'
#'       If you're specifying "R" here, you should probably use [score_pkg()]
#'       instead.
#'
#'    Example:
#'
#'    ```
#'    {
#'      "mpn_scorecard_format": "1.0",
#'      "pkg_name": "foo",
#'      "pkg_version": "1.2.3",
#'      "scorecard_type": "cli"
#'    }
#'    ```
#'
#'  * `<package>_<version>.check.txt`: Output from the package check. This is
#'    included in the appendix verbatim.
#'
#'  * `<package>_<version>.coverage.json`: Code coverage percentages. The values
#'    will be rounded to two decimal places when rendering. This file is
#'    optional.
#'
#'    Example:
#'
#'     ```
#'     {
#'       "overall": 91.54265,
#'       "files": [
#'         {
#'           "file": "cmd/foo.go",
#'           "coverage": 98.7643
#'         },
#'         {
#'           "file": "cmd/bar.go",
#'           "coverage": 84.321
#'         }
#'       ]
#'     }
#'     ```
#'
#'  * `<package>_<version>.scores.json`: Scores for individual metrics grouped
#'    into four categories: "testing", "documentation", "maintenance", and
#'    "transparency". Each category must have a least one score.
#'
#'    For the testing category, both "check and "coverage" scores are required.
#'    "check" should be 1 if the tests passed and 0 if they failed. "coverage"
#'    should match the "overall" value from `<package>_<version>.coverage.json`,
#'    divided by 100.
#'
#'    Example:
#'
#'    ```
#'    {
#'      "testing": {
#'        "check": 1,
#'        "coverage": 0.9154265
#'      },
#'      "documentation": {
#'        "has_website": 1,
#'        "has_news": 1
#'      },
#'      "maintenance": {
#'        "has_maintainer": 1,
#'        "news_current": 1
#'      },
#'      "transparency": {
#'        "has_source_control": 1,
#'        "has_bug_reports_url": 1
#'      }
#'    }
#'    ```
#'
#'  * `<package>_<version>.metadata.json`: Information to include in the
#'    "System Info" table. The table will include the "date" and "executor"
#'    value, as well as any key-value pairs defined under "info.env_vars" and
#'    "info.sys". The "date" and "executor" keys are required.
#'
#'    Example:
#'
#'    ```
#'    {
#'      "date": "2024-08-01 08:19:12",
#'      "executor": "Bobert",
#'      "info": {
#'        "env_vars": {
#'          "METWORX_VERSION": "22.09"
#'        },
#'        "sys": {
#'          "sysname": "Linux",
#'          "machine": "x86_64"
#'        }
#'      }
#'    }
#'    ```
#'
#'  * `<package>_<version>.matrix.yaml`: A file defining entries to render as
#'    the traceability matrix table. The traceability matrix table is meant to
#'    map all user-facing entry points (e.g., exported functions or available
#'    commands for a command-line executable) to the relevant documentation and
#'    test files.
#'
#'    The file should consist of a sequence of entries with the following items:
#'
#'      * `entrypoint`: The name of the entry point.
#'
#'      * `code`: The path to where the entry point is defined.
#'
#'      * `doc`: The path to the entry point's main documentation.
#'
#'      * `tests`: A list of paths where the entry point is tested.
#'
#'    What the entry point is called in the table depends on `scorecard_type`.
#'    For "cli", the column name is "Command" and, for "R", it is
#'    "Exported Function". For all other types, it is "Entry Point".
#'
#'    This file is optional if the `add_traceability` argument of
#'    [render_scorecard()] is "auto" or `FALSE`.
#'
#'    Example:
#'
#'    ```
#'    - entrypoint: foo
#'      skip: true
#'
#'    - entrypoint: foo bar
#'      code: cmd/bar.go
#'      doc: docs/commands/foo_bar.md
#'      tests:
#'        - cmd/bar_test.go
#'        - integration/bar_test.go
#'    ```
#'
#' @name external_scores
#' @aliases render_external
NULL

get_render_params_external <- function(results_dir, risk_breaks, add_traceability) {
  pkg_scores <- build_pkg_scores(results_dir)

  if (identical(add_traceability, "auto")) {
    add_traceability <- file.exists(get_result_path(results_dir, "matrix.yaml"))
  }
  if (isTRUE(add_traceability)) {
    tmat <- read_traceability_matrix(results_dir)
  } else {
    tmat <- NULL
  }

  list(
    set_title = paste("Scorecard:", pkg_scores$pkg_name, pkg_scores$pkg_version),
    scorecard_footer = format_scorecard_version(
      scorecard_ver = utils::packageVersion("mpn.scorecard")
    ),
    pkg_scores = format_scores_for_render(pkg_scores, risk_breaks),
    comments_block = check_for_comments(results_dir),
    extra_notes_data = list(
      cov_results_df = read_coverage_results(results_dir),
      check_output = read_check_output(results_dir)
    ),
    exports_df = tmat
  )
}

build_pkg_scores <- function(results_dir) {
  pkg_json <- get_result_path(results_dir, "pkg.json")
  checkmate::assert_file_exists(pkg_json)
  meta_json <- get_result_path(results_dir, "metadata.json")
  checkmate::assert_file_exists(meta_json)
  scores_json <- get_result_path(results_dir, "scores.json")
  checkmate::assert_file_exists(scores_json)

  res <- c(
    jsonlite::read_json(pkg_json),
    scores = list(jsonlite::read_json(scores_json)),
    metadata = list(jsonlite::read_json(meta_json))
  )

  return(calc_overall_scores(res))
}

read_traceability_matrix <- function(results_dir) {
  fname <- get_result_path(results_dir, "matrix.yaml")
  checkmate::assert_file_exists(fname)

  entries <- yaml::read_yaml(fname)
  entries <- purrr::discard(entries, function(e) isTRUE(e[["skip"]]))
  tibble::tibble(
    entrypoint = purrr::map_chr(entries, "entrypoint"),
    code_file = purrr::map_chr(entries, "code"),
    documentation = purrr::map_chr(entries, "doc"),
    test_files = purrr::map(entries, function(x) {
      if (length(x[["tests"]])) x[["tests"]] else character()
    })
  )
}

read_check_output <- function(results_dir) {
  fname <- get_result_path(results_dir, "check.txt")
  checkmate::assert_file_exists(fname)
  return(readChar(fname, file.size(fname)))
}

read_coverage_results <- function(results_dir) {
  fname <- get_result_path(results_dir, "coverage.json")
  checkmate::assert_file_exists(fname)

  data <- jsonlite::read_json(fname)
  filecov <- data[["files"]]
  tibble::tibble(
    code_file = purrr::map_chr(filecov, "file"),
    test_coverage = purrr::map_dbl(filecov, "coverage")
  )
}
