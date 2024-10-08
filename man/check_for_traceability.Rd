% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render-scorecard.R
\name{check_for_traceability}
\alias{check_for_traceability}
\title{Look for Traceability matrix RDS file and return contents if is found}
\usage{
check_for_traceability(results_dir, add_traceability)
}
\arguments{
\item{results_dir}{Directory with scoring results. This is the path returned
by \code{\link[=score_pkg]{score_pkg()}}.}

\item{add_traceability}{Logical (T/F). If \code{TRUE}, append a table that links package functionality to the documentation and test files.
Defaults to "auto", which will include the matrix if found.}
}
\description{
Look for Traceability matrix RDS file and return contents if is found
}
\keyword{internal}
