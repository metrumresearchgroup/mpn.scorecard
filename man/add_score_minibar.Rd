% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format-report.R
\name{add_score_minibar}
\alias{add_score_minibar}
\title{Add flextable minibar to column}
\usage{
add_score_minibar(
  flextable_df,
  score_col = "Weighted Score",
  risk_col = "Risk",
  column_index = NULL,
  color_text = FALSE,
  digits = 2
)
}
\arguments{
\item{flextable_df}{a flextable object}

\item{score_col}{The name of the column specifying the scores. Used to calculate}

\item{risk_col}{The name of the column specifying the risks}

\item{column_index}{Index of column to apply the minibar to. Traditionally, this should map to the index of \code{score_col}, but doesn't need to.
If \code{NULL}, will map to the index of \code{score_col}.}

\item{color_text}{Logical (T/F). If \code{FALSE} color the text in \code{column_index} black. Otherwise use risk colors (green, orange, red)}

\item{digits}{number of digits to round scores to.}
}
\description{
Add flextable minibar to column
}
\details{
a minibar will replace the entire column of whichever specified (via \code{column_index}). In other words, it doesn't
append the minibar to a column. Specifying an index that \strong{doesn't} correspond to the weighted scores, would overwrite that column.

\code{digits} must be specified, because the default behavior is rounding the column to one decimal
}
\keyword{internal}
