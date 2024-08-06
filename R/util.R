#' Check if a path exists and delete the file
#' if overwrite is TRUE
#' @param path a file path to check if it exists
#' @param overwrite Logical (T/F). If `TRUE`, delete the file at the specified path
#'
#' @keywords internal
check_exists_and_overwrite <- function(path, overwrite) {
  checkmate::assert_string(path)
  checkmate::assert_logical(overwrite, len = 1)
  if (fs::file_exists(path)) {
    if (isTRUE(overwrite)) {
      fs::file_delete(path)
    } else {
      abort(glue::glue("{path} already exists. Pass overwrite = TRUE to overwrite it."))
    }
  }
}

#' Assign output file path for various outputs during scorecard rendering
#'
#' @param out_dir output directory for saving results and json
#' @param ext file name and extension. `"mitigation.txt"` is deprecated, but
#'   still supported.
#'
#' @details
#' The basename of `out_dir` should be the package name and version pasted together
#'
#'
#' @keywords internal
get_result_path <- function(
    out_dir,
    ext = c(
      "scorecard.json", "scorecard.pdf", "comments.txt", "mitigation.txt",
      # Internally scored
      "check.rds", "covr.rds", "export_doc.rds", "summary.pdf",
      # Externally scored
      "check.txt", "coverage.json", "matrix.yaml", "metadata.json", "pkg.json",
      "scores.json"
    )
){

  ext <- match.arg(ext)

  pkg_name <- basename(out_dir)

  file.path(out_dir, paste0(pkg_name,".",ext))
}

#' Read Description file and parse the package name and version
#'
#' @param pkg_source_path path to package source code (untarred)
#'
#' @keywords internal
get_pkg_desc <- function(pkg_source_path, fields = NULL){

  pkg_desc_path <- file.path(pkg_source_path, "DESCRIPTION")

  desc_file <- read.dcf(pkg_desc_path, fields = fields)[1L,]
  pkg_desc <- as.list(desc_file)

  return(pkg_desc)
}

#' Untar package and return installation directory
#'
#' @param pkg_tar path to tarball package
#' @param temp_file_name name of `tempfile`
#'
#' @keywords internal
unpack_tarball <- function(pkg_tar, temp_file_name = "SCORECARD_"){
  # Create temporary location for package installation
  temp_pkg_dir <- tempfile(temp_file_name)
  if (!dir.create(temp_pkg_dir)) stop("unable to create ", temp_pkg_dir)

  source_tar_dir <- file.path(temp_pkg_dir)

  # unpack tarball

  utils::untar(pkg_tar, exdir = source_tar_dir)

  # unpackacked package path
  pkg_source_path <- dir_ls(source_tar_dir)

  # Confirm tar is unpackacked in expected directory
  checkmate::assert_string(pkg_source_path)
  checkmate::assert_directory_exists(pkg_source_path)

  return(pkg_source_path)
}


#' Warn that a feature is deprecated
#'
#' @param version mpn.scorecard version when something became deprecated
#' @param what A string describing what is deprecated.
#' @param details Can either be a single string or a character vector, which
#'   will be converted to a bulleted list.
#'
#' @details
#' This function is different from lifecycle::deprecate_warn, as it does not
#' refer to the function that called it, nor care about the environment it was
#' called in. It simply triggers an inforamtive warning if the current
#' `mpn.scorecard` version is ahead of the `version` supplied.
#'
#' @noRd
deprecate_warning <- function(version, what, details = NULL){
  # Format package versions
  mpn_scorecard_ver <- package_version(utils::packageVersion("mpn.scorecard"))
  version <- package_version(version)

  if(mpn_scorecard_ver >= version){
    ver_text <- paste("mpn.scorecard", version)
    what_txt <- c("!" = paste(what, glue::glue("is deprecated as of {ver_text}")))
    details_txt <- if(!is.null(details)) c("i" = details) else ""

    rlang::warn(c(what_txt, details_txt))
  }
}



#' Wrap a vector of strings using specific characters
#'
#' @param str a string or vector of strings.
#' @param width Positive integer giving the target line width (in number of
#'  characters).
#' @param wrap_sym Characters to loop through and attempt to make new lines with.
#'  Note that the order you specify these values matters.
#' @param strict logical (T/F). If `FALSE`, will soft wrap based on the
#'  characters specified via `wrap_sym`. If `TRUE`, will first soft wrap, and then
#'  enforce the specified `width`.
#' @param indent logical (T/F). If `TRUE`, indent new lines by two spaces.
#'
#' @details
#' `stringr::str_wrap` is not strict with the width for word characters, so
#' a helper function was needed. Rather than being 100% strict at following the
#' cutoff width, we attempt to split at whitespace specific symbols commonly
#' used in function names and file paths. If this splitting is not sufficient,
#' we then perform a strict operation, whereby we cut the line off at exactly
#' that width.
#'
#' @keywords internal
wrap_text <- function(
    str,
    width = 23,
    wrap_sym = c("/", "_", "-"),
    strict = FALSE,
    indent = FALSE
){
  purrr::map_chr(
    str, wrapi_text,
    width = width, wrap_sym = wrap_sym, strict = strict, indent = indent
  )
}

#' Wrap a character string using specific characters
#' @inheritParams wrapi_text
#' @noRd
wrapi_text <- function(
    str,
    width = 23,
    wrap_sym = c("/", "_", "-"),
    strict = FALSE,
    indent = FALSE
){

  checkmate::assert_string(str, na.ok = TRUE)
  if (is.na(str)) {
    return(str)
  }

  str_new <- str

  # Get max number of characters per line (splits on '\n')
  max_line_char <- function(x){
    lines <- unlist(strsplit(x, "\n"))
    if(!length(lines)) return(0) else max(purrr::map_dbl(lines, nchar))
  }

  # Paste the max number of pieces together per line
  paste_pieces <- function(pieces, wrap_chr, width){
    newline_sep <- paste0(wrap_chr, "\n")

    cat_start <- function(start, add, sep){
      if(is.null(start)) add else paste(start, add, sep = sep)
    }

    start <- NULL
    shift <- 1
    npieces <- length(pieces)
    for (i in 2:npieces) {
      pieces_sep <- pieces[shift:i]
      add <- paste0(pieces_sep, collapse = wrap_chr)
      s <- cat_start(start = start, add = add, sep = wrap_chr)
      if(max_line_char(s) <= width){
        next
      }else{
        if (shift == i) {
          # `start` includes everything aside from the current piece.
          add <- paste0("\n", pieces[i])
        } else {
          pieces_start <- pieces[shift:(i - 1)]
          add <- paste(paste0(pieces_start, collapse = wrap_chr), pieces[i], sep = newline_sep)
        }

        if (i == npieces) {
          s <- cat_start(start = start, add = add, sep = wrap_chr)
        }else{
          start <- cat_start(start = start, add = add, sep = wrap_chr)
          shift <- i + 1
        }
      }
    }
    return(s)
  }

  # Loop through specified symbols and attempt to create breaks at their
  # occurrence. Will only happen if the line has more characters than the
  # specified width. A line is represented as: '\n...str...\n'
  if(!is.null(wrap_sym)){
    for(wrap.i in wrap_sym){
      if(grepl(wrap.i, str_new) && max_line_char(str_new) > width){
        pieces <- unlist(strsplit(str_new, wrap.i))
        str_new <- paste_pieces(pieces, wrap.i, width)
      }
    }
  }

  # Check if `wrap_sym` characters ensured required width
  if(max_line_char(str_new) > width && isTRUE(strict)){
    pieces <- unlist(strsplit(str_new, ""))
    str_new <- paste_pieces(pieces, wrap_chr = "", width)
  }

  # Indent new lines if only one line -initially- existed
  # This stops multi-lined strings (i.e. test scripts) from being intended
  if(isTRUE(indent)){
    str_new <- str_new %>% gsub("\n", "\n  ", .)
  }

  return(str_new)
}
