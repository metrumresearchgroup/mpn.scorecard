
test_that("wrap_text() works", {

  # Get max number of characters per line (splits on '\n')
  max_line_char <- function(x){
    lines <- unlist(strsplit(x, "\n"))
    if(!length(lines)) return(0) else max(purrr::map_dbl(lines, nchar))
  }

  # Base Case
  str <- "man/check_nonmem_table_output.Rd"
  wrap_str <- wrap_text(str)
  expect_equal(wrap_str, "man/\ncheck_nonmem_table_\noutput.Rd")
  expect_equal(max_line_char(wrap_str), 19)

  # Indents
  expect_equal(
    wrap_text(str, indent = TRUE),
    "man/\n  check_nonmem_table_\n  output.Rd"
  )

  # Strict with no symbols
  hard_cutoff <- 26
  wrap_str <- wrap_text(str, strict = TRUE, wrap_sym = NULL, width = hard_cutoff)
  expect_equal(wrap_str, "man/check_nonmem_table_out\nput.Rd")
  expect_equal(max_line_char(wrap_str), hard_cutoff)

  # Adjust width
  str <- "test-modify-model-field.R\ntest-submit-models.R"
  wrap_str <- wrap_text(str, width = 30)
  expect_equal(wrap_str, "test-modify-model-field.R\ntest-submit-models.R")
  expect_equal(max_line_char(wrap_str), 25)
  wrap_str <- wrap_text(str, width = 24)
  expect_equal(wrap_str, "test-modify-model-\nfield.R\ntest-submit-models.R")
  expect_equal(max_line_char(wrap_str), 20)
})

test_that("wrap_text(): past offenders", {
  expect_identical(
    wrap_text("abcde", width = 2, wrap_sym = NULL, strict = TRUE),
    "ab\ncd\ne"
  )

  expect_identical(
    wrap_text("abcdefghij", width = 4, wrap_sym = NULL, strict = TRUE),
    "abcd\nefgh\nij"
  )

  expect_identical(
    wrap_text(
      "test-foo.R\ntest-foo_bar.R",
      width = 8, wrap_sym = NULL, strict = TRUE
    ),
    "test-foo\n.R\ntest-foo\n_bar.R"
  )

  expect_identical(
    wrap_text(
      "test-foo.R\ntest-foo_bar.R",
      width = 4, wrap_sym = NULL, strict = TRUE
    ),
    "test\n-foo\n.R\ntest\n-foo\n_bar\n.R"
  )

  expect_identical(
    wrap_text("foo/bar/baz", width = 4),
    "foo/\nbar/\nbaz"
  )
})
