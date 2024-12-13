% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orcabus.R
\name{orca_libid2workflows}
\alias{orca_libid2workflows}
\title{OrcaBus Get Workflows From LibraryId}
\usage{
orca_libid2workflows(
  libid,
  token,
  wf_name = NULL,
  page_size = 10,
  stage = "prod"
)
}
\arguments{
\item{libid}{libraryId.}

\item{token}{JWT.}

\item{wf_name}{Name of workflow to query.}

\item{page_size}{Maximum number of rows to return.}

\item{stage}{Environment where API is deployed (prod, stg or dev).}
}
\value{
Tibble with results.
}
\description{
Given a libraryId gets workflow details.
}
\examples{
\dontrun{
libid <- "L2401591" # wgs
libid <- "L2401074" # wts # nothing
libid <- "L2401577" # wts
libid <- "L2401558" # cttsov2
libid <- "L2401608" # wgs
libid <- "L2401603" # wgs
libid <- "L2401610" # oa
libid <- "L2401445" # cttsov2
wf_name <- NULL
token <- orca_jwt() |> jwt_validate()
d <- orca_libid2workflows(libid = libid, token = token, wf_name = wf_name, page_size = 20)
}
}
