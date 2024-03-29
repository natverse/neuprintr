% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch.R
\name{neuprint_list2df}
\alias{neuprint_list2df}
\title{Parse neuprint return list to a data frame}
\usage{
neuprint_list2df(
  x,
  cols = NULL,
  return_empty_df = FALSE,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  ...
)
}
\arguments{
\item{x}{A list returned by \code{\link{neuprint_fetch_custom}}}

\item{cols}{Character vector specifying which columns to include (by default
all of those named in \code{x}, see details).}

\item{return_empty_df}{Return a zero row data frame when there is no result.}

\item{check.names}{Whether to convert column names into R friendly form. This
is not necessary but would be the default for \code{\link{data.frame}} were
we not to set it ourselves. See \code{\link{as.data.frame}} for details.}

\item{stringsAsFactors}{Whether to return character vector columns as
factors. Note that the default of \code{FALSE} differs from
\code{\link{data.frame}} and friends.}

\item{...}{Additional arguments passed to \code{\link{as.data.frame}}}
}
\description{
Parse neuprint return list to a data frame
}
\details{
A low level function tailored to the standard neuprint list return
  format. Should handle those times when jsonlite's simplification doesn't
  work. The normal return value of \code{\link{neuprint_fetch_custom}} is a
  list formatted as follows: \itemize{

  \item{columns}{ List of column names}

  \item{data}{ Nested list of data, with each row formatted as a single
  sublist}

  \item{debug }{ Character vector containing query}}

  If \code{neuprint_list2df} receives such a list it will use the
  \code{columns} to define the names for a data.frame constructed from the
  \code{data} field.
}
