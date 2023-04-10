

# adapted from mrgvalprep::get_sys_info()
#' @importFrom rlang %||%
get_metadata <- function(executor = NULL, env_vars = "METWORX_VERSION") {
  checkmate::assert_string(executor, null.ok = TRUE)
  checkmate::assert_character(env_vars, null.ok = TRUE)

  res <- list(
    date = as.character(Sys.time()),
    executor = executor %||% Sys.getenv("USER"),
    info = list()
  )

  # TODO: add some way to hash package source code
  # maybe we insist on taking a tarball and we md5sum that?

  if (!is.null(env_vars)) {
    res[["info"]][["env_vars"]] <- list()
    for (.ev in env_vars) {
      res[["info"]][["env_vars"]][[.ev]] <- Sys.getenv(.ev)
    }
  }

  res[["info"]][["sys"]] <- as.list(Sys.info())[c("sysname", "version", "release", "machine")]

  return(res)
}
