#' OrcaBus Query URL
#'
#' @param url URL to query.
#' @param token JWT to use for authentication.
#'
#' @return JSON response.
#' @export
orca_query_url <- function(url, token = NULL) {
  assertthat::assert_that(!is.null(token))
  url <- utils::URLencode(url)
  req <- url |>
    httr2::request() |>
    httr2::req_auth_bearer_token(token = token) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_user_agent("umccr/rportal")
  resp <- req |>
    httr2::req_perform()
  resp |>
    httr2::resp_body_json()
}

#' OrcaBus cttsov2 PortalRunId To Output Directory
#'
#' Given a portalRunId from an OrcaBus cttso run, generate the sampleId and
#' root path to the output directory with the results.
#'
#' @param prid PortalRunId.
#' @param token JWT.
#'
#' @return List with the sampleId `prefix` and the `path` containing the Results
#' and Logs_Intermediates folders.
#' @examples
#' \dontrun{
#' token <- orca_jwt() |> jwt_validate()
#' # e.g. for a cttsov2 workflow
#' orca_workflow_payload("20240930fc23056d", token = token)
#'
#' sampleid <- resp[["data"]][["inputs"]][["sampleId"]]
#' path <- resp[["data"]][["outputs"]][["resultsDir"]] |> dirname()
#' list(
#'   path = path,
#'   prefix = sampleid
#' )
#' }
#'
#' @export
orca_workflow_payload <- function(prid, token) {
  prid2payload <- function(prid, token) {
    endpoint <- "https://workflow.prod.umccr.org/api/v1/workflowrun"
    url <- glue::glue("{endpoint}?portalRunId={prid}")
    resp <- orca_query_url(url, token)
    resp[["results"]][[1]][["currentState"]][["payload"]]
  }
  payload <- prid2payload(prid, token)
  endpoint <- "https://workflow.prod.umccr.org/api/v1/payload"
  url <- glue::glue("{endpoint}/{payload}")
  orca_query_url(url, token)
}


# urls <- list(
#   file = list(
#     all = "https://file.{ns}.umccr.org/api/v1/",
#     obj = "https://file.{ns}.umccr.org/api/v1/objects?page_size=10",
#     s3obj = "https://file.{ns}.umccr.org/api/v1/s3_objects?page_size=10"
#   ),
#   metadata = list(
#     all = "https://metadata.{ns}.umccr.org/api/v1",
#     # individual = "https://metadata.{ns}.umccr.org/api/v1/individual",
#     subject = "https://metadata.{ns}.umccr.org/api/v1/subject",
#     # sample = "https://metadata.{ns}.umccr.org/api/v1/sample",
#     library = "https://metadata.{ns}.umccr.org/api/v1/library?libraryId=L2400160",
#     specimen = "https://metadata.{ns}.umccr.org/api/v1/specimen"
#   ),
#   workflow = list(
#     all = "https://workflow.{ns}.umccr.org/api/v1/workflowrun",
#     wts = "https://workflow.{ns}.umccr.org/api/v1/workflowrun?workflow__workflowName=wts",
#     tn = "https://workflow.{ns}.umccr.org/api/v1/workflowrun?workflow__workflowName=tumor_normal",
#     cttso = "https://workflow.{ns}.umccr.org/api/v1/workflowrun?workflow__workflowName=cttsov2",
#   ),
#   sequence = list(
#     all =  "https://sequence.{ns}.umccr.org/srm/v1/",
#     seq =  "https://sequence.{ns}.umccr.org/srm/v1/sequence",
#     seq2 = "https://sequence.{ns}.umccr.org/srm/v1/sequence/1"
#   )
# )
# query(urls$workflow$all, ns = "prod")
# query(urls$metadata$all, ns = "prod")
# query(urls$mm$all, ns = "dev") |> str()
