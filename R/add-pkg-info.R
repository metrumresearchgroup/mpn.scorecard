# Pass in manual info from JSON
#
# These will be things that we know, but can't
# be verified by riskmetric. For example,
# bug report URLs that aren't in DESCRIPTION.

add_pkg_info <- function(scorelist, pkg_info) {
  # Do we just want to loop over pkg_info and
  # replace anything with the same key in scorelist?
  # That feels a little dangerous, but maybe.
  return(scorelist)
}
