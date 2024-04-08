# portal workflow meta subset
require(rportal)
require(here)
require(dplyr)
require(purrr)
require(readr)

# connect to umccr aws account
rportal::awsvault_profile("upro")

wfs <- c(
  "bcl_convert", "rnasum", "tso_ctdna_tumor_only",
  "umccrise", "wgs_alignment_qc", "wgs_tumor_normal", "wts_tumor_only",
  "wts_alignment_qc",
  "oncoanalyser_wgs", "oncoanalyser_wgts_existing_both",
  "oncoanalyser_wts", "sash", "star_alignment"
)

portaldb_query_workflow_type <- function(wf_type, limit = 4) {
  query <- glue(
    "WHERE \"type_name\" = '{wf_type}' AND \"end_status\" = 'Succeeded' ",
    "ORDER BY \"start\" DESC LIMIT {limit};"
  )
  rportal::portaldb_query_workflow(query)
}

# we are keeping it simple and just running the same query with a different type
d <- wfs |>
  purrr::map(\(x) portaldb_query_workflow_type(wf_type = x)) |>
  dplyr::bind_rows()
d |>
  # readr::write_rds(here::here("inst/extdata/portaldb_workflow_top4.rds"))
  readr::write_csv(here::here("inst/extdata/portaldb_workflow_top4.csv"))
