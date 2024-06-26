% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{portaldb_query_table}
\alias{portaldb_query_table}
\title{PortalDB Query Table}
\usage{
portaldb_query_table(query = NULL, table = "data_portal_workflow")
}
\arguments{
\item{query}{SQL query string.}

\item{table}{Table from \code{data_portal} DB (def: "data_portal_workflow").}
}
\value{
Tibble with results from query.
}
\description{
Query the given PortalDB table.
Note this is simply a convenience function that prepends
\verb{SELECT * FROM "data_portal"."data_portal"."<table>" } to any \code{query} you provide.
See examples.
}
\examples{
\dontrun{
prid <- shQuote("202403274bf3ad80")
query <- glue("WHERE \"portal_run_id\" IN ({prid});")
portaldb_query_table(query = query, table = "data_portal_workflow")
}
}
