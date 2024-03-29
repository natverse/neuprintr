% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roi.R
\name{neuprint_ROI_mesh}
\alias{neuprint_ROI_mesh}
\title{Download a region of interest as a 3D mesh}
\usage{
neuprint_ROI_mesh(roi, dataset = NULL, conn = NULL, ...)
}
\arguments{
\item{roi}{region of interest for a dataset}

\item{dataset}{optional, a dataset you want to query. If \code{NULL}, the
default specified by your R environ file is used or, failing that the
current connection, is used. See \code{\link{neuprint_login}} for details.}

\item{conn}{optional, a neuprintr connection object, which also specifies the
neuPrint server. If NULL, the defaults set in your
\code{\link[=Startup]{.Rprofile}} or \code{\link[=Startup]{.Renviron}} are
used. See \code{\link{neuprint_login}} for details.}

\item{...}{methods passed to \code{\link{neuprint_login}}}
}
\value{
a \code{\link[rgl]{mesh3d}} object
}
\description{
Download a region of interest as a 3D mesh
}
\examples{
\donttest{
ALR=neuprint_ROI_mesh('AL(R)', dataset='hemibrain:v1.2.1')
}
\dontrun{
library(nat)
ALR
shade3d(ALR, col='grey')
}
}
\seealso{
\code{\link{neuprint_ROIs}}, \code{\link{neuprint_ROI_hierarchy}}
}
