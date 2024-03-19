
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
