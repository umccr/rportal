% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_icav1.R
\name{meta_wgs_alignment_qc}
\alias{meta_wgs_alignment_qc}
\title{Metadata for wgs_alignment_qc workflow}
\usage{
meta_wgs_alignment_qc(pmeta, status = "Succeeded")
}
\arguments{
\item{pmeta}{Path to portal workflows metadata table, or tibble with already parsed data.}

\item{status}{Workflow status to keep (default: Succeeded).}
}
\value{
A tibble with metadata per workflow run.
}
\description{
Metadata for wgs_alignment_qc workflow
}
\examples{
pmeta <- "extdata/portaldb_workflow_top4.rds" |>
  system.file(package = "rportal") |>
  readr::read_rds()
(m <- meta_wgs_alignment_qc(pmeta))
}
