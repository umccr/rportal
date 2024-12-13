% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/orcabus.R
\name{orca_wfrid2state}
\alias{orca_wfrid2state}
\title{OrcaBus WFR ID To State}
\usage{
orca_wfrid2state(wfrid, token, stage = "prod")
}
\arguments{
\item{wfrid}{Workflow run ID.}

\item{token}{JWT to use for authentication.}

\item{stage}{Environment where API is deployed (prod, stg or dev).}
}
\value{
Tibble with results.
}
\description{
Given a workflow run ID, provides the state.
}
\examples{
\dontrun{
token <- orca_jwt() |> jwt_validate()
wfrid <- "wfr.01JCARAVTXKG5581SRA1HKBTD3"
orca_wfrid2state(wfrid = wfrid, token = token)
}

}
