#' PortalDB Connect
#'
#' Establish the connection parameters for querying the Athena UMCCR databases
#' from the `orcahouse` or `data_portal` workgroup.
#'
#' @param workgroup UMCCR Athena workgroup for portal database.
#' @return Connection to DBMS
#'
#' @examples
#' \dontrun{
#' con <- portaldb_connect(workgroup = "orcahouse")
#' RAthena::list_work_groups(con) |> str()
#' }
#' @export
portaldb_connect <- function(workgroup = "orcahouse") {
  # make sure you have logged into AWS
  c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION") |>
    rportal::envvar_defined() |>
    stopifnot()
  # keep annoying reticulate prompt away
  Sys.setenv(RETICULATE_PYTHON = Sys.getenv("CONDA_PYTHON_EXE"))
  RAthena::RAthena_options(clear_s3_resource = FALSE)
  con <- DBI::dbConnect(
    RAthena::athena(),
    work_group = workgroup,
    rstudio_conn_tab = FALSE
  )
  return(con)
}

#' PortalDB Query
#'
#' Submit an Athena query.
#'
#' @param query SQL query string.
#' @param workgroup Athena workgroup for portal database.
#' @return Tibble with results from the provided query.
#'
#' @examples
#' \dontrun{
#' mart_db <- '"orcavault"."mart"'
#' q1 <- glue('SELECT * FROM {mart_db}."lims" LIMIT 2;')
#' portaldb_query(query = q1)
#' # find exact matches
#' prids <- c("20240327d595afa9", "202403274bf3ad80", "202504181b2efa22")
#' prids_quote <- paste(shQuote(prids), collapse = ", ")
#' q2 <- glue('SELECT * FROM {mart_db}."workflow" WHERE "portal_run_id" IN ({prids_quote});')
#' portaldb_query(q2)
#' }
#' @export
portaldb_query <- function(query = NULL, workgroup = "orcahouse") {
  assertthat::assert_that(!is.null(query))
  con <- portaldb_connect(workgroup = workgroup)
  cli::cli_alert_info("Running following SQL query:\n{query}")
  d <- RAthena::dbGetQuery(con, query) |>
    tibble::as_tibble()
  DBI::dbDisconnect(con)
  return(d)
}

#' PortalDB Query Mart
#'
#' Query the given PortalDB Mart table.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "orcavault"."mart"."<table>" ` to any `query` you provide.
#' See examples.
#'
#' @param table Table from `mart` DB.
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' prid <- shQuote("202504181b2efa22")
#' query <- glue("WHERE \"portal_run_id\" IN ({prid});")
#' portaldb_query_mart_table(query = query, table = "workflow")
#' }
#' @export
portaldb_query_mart_table <- function(query = NULL, table = "workflow") {
  assertthat::assert_that(!is.null(query))
  tab <- glue('"orcavault"."mart"."{table}"')
  q1 <- glue("SELECT * FROM {tab} {query}")
  portaldb_query(q1)
}

#' PortalDB Query Mart Workflow
#'
#' Queries the `workflow` table with the given query.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "orcavault"."mart"."workflow" ` to any
#' `query` you provide.
#' See examples.
#'
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' prid <- shQuote("2025060866284123")
#' query <- glue("WHERE \"portal_run_id\" IN ({prid});")
#' query <- glue("WHERE \"workflow_name\" = 'dragen-wgts-dna';")
#' query <- ";"
#' wfs <- portaldb_query_workflow(query)
#' }
#' @export
portaldb_query_workflow <- function(query = NULL) {
  portaldb_query_mart_table(query = query, table = "workflow")
}

#' PortalDB Query Mart Lims
#'
#' Queries the `lims` table with the given query.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "orcavault"."mart"."lims" ` to any
#' `query` you provide.
#' See examples.
#'
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' libids <- shQuote(paste(c("L2400340", "L2400256", "L2500469"), collapse = "|"))
#' query1 <- glue("WHERE REGEXP_LIKE(\"library_id\", {libids});")
#' portaldb_query_lims(query1)
#' sbjids <- paste(c("SBJ04470", "SBJ04487", "SBJ04488"), collapse = "|")
#' query2 <- glue(
#'   "WHERE REGEXP_LIKE(\"internal_subject_id\", '{sbjids}') AND \"type\" = 'WGS' ",
#'   "AND \"phenotype\" = 'tumor' ORDER BY \"internal_subject_id\" DESC;"
#' )
#' d <- portaldb_query_lims(query2)
#' # get tumor libids for each sbjid
#' d |> dplyr::select(internal_subject_id, library_id)
#' }
#' @export
portaldb_query_lims <- function(query = NULL) {
  portaldb_query_mart_table(query = query, table = "lims")
}

#' PortalDB Query fastq Table
#'
#' Queries the `fastq` table with the given query.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "orcavault"."mart"."fastq" ` to any
#' `query` you provide.
#' See examples.
#'
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' libids <- shQuote(paste(c("L2100192", "L2100191", "L2500469"), collapse = "|"))
#' query <- glue("WHERE REGEXP_LIKE(\"library_id\", {libids});")
#' res <- portaldb_query_fastq(query)
#' }
#' @export
portaldb_query_fastq <- function(query = NULL) {
  portaldb_query_mart_table(query = query, table = "fastq")
}

#' PortalDB Query fastq_history Table
#'
#' Queries the `fastq_history` table with the given query.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "orcavault"."mart"."fastq_history" ` to any
#' `query` you provide.
#' See examples.
#'
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' libids <- shQuote(paste(c("L2100192", "L2100191", "L2500469"), collapse = "|"))
#' query <- glue("WHERE REGEXP_LIKE(\"library_id\", {libids});")
#' res <- portaldb_query_fastqhistory(query)
#' }
#' @export
portaldb_query_fastqhistory <- function(query = NULL) {
  portaldb_query_mart_table(query = query, table = "fastq_history")
}

#' PortalDB Query curation_lims Table
#'
#' Queries the `curation_lims` table with the given query.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "orcavault"."mart"."curation_lims" ` to any
#' `query` you provide.
#' See examples.
#'
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' libids <- shQuote(paste(c("L2400340", "L2400256", "L2500469"), collapse = "|"))
#' query <- glue("WHERE REGEXP_LIKE(\"library_id\", {libids});")
#' portaldb_query_curationlims(query)
#' }
#' @export
portaldb_query_curationlims <- function(query = NULL) {
  portaldb_query_mart_table(query = query, table = "curation_lims")
}

#' PortalDB Query bam Table
#'
#' Queries the `bam` table with the given query.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "orcavault"."mart"."bam" ` to any
#' `query` you provide.
#' See examples.
#'
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' libids <- shQuote(paste(c("L2400340", "L2400256", "L2500469"), collapse = "|"))
#' query <- glue("WHERE REGEXP_LIKE(\"library_id\", {libids});")
#' portaldb_query_bam(query)
#' }
#' @export
portaldb_query_bam <- function(query = NULL) {
  portaldb_query_mart_table(query = query, table = "bam")
}
