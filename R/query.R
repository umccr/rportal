portaldb_connect <- function() {
  RAthena::RAthena_options(clear_s3_resource = FALSE)
  con <- DBI::dbConnect(
    RAthena::athena(),
    work_group = "data_portal",
    rstudio_conn_tab = FALSE
  )
  return(con)
}

#' PortalDB Query
#'
#' Query the `data_portal` UMCCR database.
#' To access a specific table from the database, use
#' `"data_portal"."data_portal"."table"` (see examples).
#'
#' @param query SQL query string.
#' @return Tibble with results from the provided query.
#'
#' @examples
#' \dontrun{
#' q1 <- 'SELECT * FROM "data_portal"."data_portal"."data_portal_limsrow" LIMIT 2;'
#' portaldb_query(q1)
#' # example of finding exact matches
#' prids <- c("20240327d595afa9", "202403274bf3ad80")
#' prids_quote <- paste(shQuote(prids), collapse = ", ")
#' wf_table <- '"data_portal"."data_portal"."data_portal_workflow"'
#' q2 <- glue('SELECT * FROM {wf_table} WHERE "portal_run_id" IN ({prids_quote});')
#' portaldb_query(q2)
#' }
#' @export
portaldb_query <- function(query = NULL) {
  assertthat::assert_that(!is.null(query))
  con <- portaldb_connect()
  cli::cli_alert_info("Running following SQL query:\n{query}")
  d <- RAthena::dbGetQuery(con, query) |>
    tibble::as_tibble()
  DBI::dbDisconnect(con)
  return(d)
}

#' PortalDB Query Table
#'
#' Query the given PortalDB table.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "data_portal"."data_portal"."<table>" ` to any `query` you provide.
#' See examples.
#'
#' @param table Table from `data_portal` DB (def: "data_portal_workflow").
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' prid <- shQuote("202403274bf3ad80")
#' query <- glue("WHERE \"portal_run_id\" IN ({prid});")
#' portaldb_query_table(query = query, table = "data_portal_workflow")
#' }
#' @export
portaldb_query_table <- function(query = NULL, table = "data_portal_workflow") {
  assertthat::assert_that(!is.null(query))
  wf_table <- glue('"data_portal"."data_portal"."{table}"')
  q1 <- glue("SELECT * FROM {wf_table} {query}")
  portaldb_query(q1)
}

#' PortalDB Query Workflow Table
#'
#' Queries the `data_portal_workflow` table with the given query.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "data_portal"."data_portal"."data_portal_workflow" ` to any
#' `query` you provide.
#' See examples.
#'
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' prid <- shQuote("202403274bf3ad80")
#' query <- glue("WHERE \"portal_run_id\" IN ({prid});")
#' portaldb_query_workflow(query)
#' }
#' @export
portaldb_query_workflow <- function(query = NULL) {
  portaldb_query_table(query = query, table = "data_portal_workflow")
}

#' PortalDB Query Limsrow Table
#'
#' Queries the `data_portal_limsrow` table with the given query.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "data_portal"."data_portal"."data_portal_limsrow" ` to any
#' `query` you provide.
#' See examples.
#'
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' libids <- shQuote(paste(c("L2400340", "L2400256"), collapse = "|"))
#' query <- glue("WHERE REGEXP_LIKE(\"library_id\", {libids});")
#' portaldb_query_limsrow(query)
#' }
#' @export
portaldb_query_limsrow <- function(query = NULL) {
  portaldb_query_table(query = query, table = "data_portal_limsrow")
}

#' PortalDB Query Fastqlistrow Table
#'
#' Queries the `data_portal_fastqlistrow` table with the given query.
#' Note this is simply a convenience function that prepends
#' `SELECT * FROM "data_portal"."data_portal"."data_portal_fastqlistrow" ` to any
#' `query` you provide.
#' See examples.
#'
#' @param query SQL query string.
#' @return Tibble with results from query.
#'
#' @examples
#' \dontrun{
#' libids <- shQuote(paste(c("L2100192"), collapse = "|"))
#' query <- glue("WHERE REGEXP_LIKE(\"library_id\", {libids});")
#' res <- portaldb_query_limsrow(query)
#' }
#' @export
portaldb_query_fastqlistrow <- function(query = NULL) {
  portaldb_query_table(query = query, table = "data_portal_fastqlistrow")
}
