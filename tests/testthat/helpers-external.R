#' Create a temporary directory with external results
#'
#' @param pattern,clean,.local_envir Arguments passed to
#'   `withr::local_tempdir()`.
local_create_external_results <- function(pattern = "mpn-scorecard-tests-",
                                          .local_envir = parent.frame(),
                                          clean = TRUE) {
  tdir <- withr::local_tempdir(
    pattern = pattern, .local_envir = .local_envir, clean = clean
  )

  pkg <- "foo"
  version <- "1.2.3"

  prefix <- paste0(pkg, "_", version)
  rdir <- file.path(tdir, prefix)
  fs::dir_create(rdir)

  stem <- file.path(tdir, prefix, prefix)

  jsonlite::write_json(
    list(
      mpn_scorecard_format = MPN_SCORECARD_FORMAT,
      pkg_name = pkg,
      pkg_version = version,
      scorecard_type = "cli"
    ),
    paste0(stem, ".pkg.json"),
    auto_unbox = TRUE
  )

  cat("check", "output", sep = "\n", file = paste0(stem, ".check.txt"))

  jsonlite::write_json(
    list(
      overall = 91.54265,
      files = list(
        list(
          file = "cmd/foo.go",
          coverage = 98.7643
        ),
        list(
          file = "cmd/bar.go",
          coverage = 84.321
        )
      )
    ),
    paste0(stem, ".coverage.json"),
    auto_unbox = TRUE
  )

  jsonlite::write_json(
    list(
      testing = list(
        check = 1,
        coverage = 0.9154265
      ),
      documentation = list(
        has_website = 1,
        has_news = 1
      ),
      maintenance = list(
        has_maintainer = 1,
        news_current = 1
      ),
      transparency = list(
        has_source_control = 1,
        has_bug_reports_url = 1
      )
    ),
    paste0(stem, ".scores.json"),
    auto_unbox = TRUE
  )

  jsonlite::write_json(
    list(
      date = "2024-08-01 08:19:12",
      executor = "Bobert",
      info = list(
        env_vars = list(
          METWORX_VERSION = "22.09"
        ),
        sys = list(
          sysname = "Linux",
          machine = "x86_64"
        )
      )
    ),
    paste0(stem, ".metadata.json"),
    auto_unbox = TRUE
  )

  yaml::write_yaml(
    list(
      list(
        entrypoint = "foo",
        skip = TRUE
      ),
      list(
        entrypoint = "foo bar",
        code = "cmd/bar.go",
        doc = "docs/commands/foo_bar.md",
        tests = c("cmd/bar_test.go", "integration/bar_test.go")
      ),
      list(
        entrypoint = "foo baz",
        code = "cmd/baz.go",
        doc = "docs/commands/foo_baz.md",
        tests = "cmd/baz_test.go"
      )
    ),
    paste0(stem, ".matrix.yaml")
  )

  return(rdir)
}
