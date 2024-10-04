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

#' OrcaBus PortalRunId To Workflow Payload
#'
#' Given an OrcaBus portalRunId, provides the workflow payload.
#'
#' @param prid portalRunId.
#' @param token JWT.
#' @param stage Environment where API is deployed (prod, stg or dev).
#'
#' @return List of workflow payload.
#' @examples
#' \dontrun{
#' token <- orca_jwt() |> jwt_validate()
#' # e.g. for a cttsov2 workflow
#' prid <- "20240930a40b3974"
#' p <- orca_prid2wfpayload(prid = prid, token = token)
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
orca_prid2wfpayload <- function(prid, token, stage = "prod") {
  assertthat::assert_that(stage %in% orca_stages())
  prid2payload <- function(prid, token) {
    ep <- glue::glue("https://workflow.{stage}.umccr.org/api/v1/workflowrun")
    url <- glue::glue("{ep}?portalRunId={prid}")
    resp <- orca_query_url(url, token)
    resp[["results"]][[1]][["currentState"]][["payload"]]
  }
  payload <- prid2payload(prid, token)
  ep <- glue::glue("https://workflow.{stage}.umccr.org/api/v1/payload")
  url <- glue::glue("{ep}/{payload}")
  orca_query_url(url, token)
}

#' OrcaBus Get Workflows From LibraryId
#'
#' Given a libraryId gets workflow details.
#'
#' @param libid libraryId.
#' @param token JWT.
#' @param stage Environment where API is deployed (prod, stg or dev).
#' @param wf_name Name of workflow to query.
#' @param page_size Maximum number of rows to return.
#'
#' @return Tibble with results.
#' @examples
#' \dontrun{
#' token <- orca_jwt() |> jwt_validate()
#' libid <- "L2401414"
#' wf_name <- "cttsov2"
#' d <- orca_libid2workflows(libid = libid, token = token, wf_name = wf_name)
#' }
#' @export
orca_libid2workflows <- function(libid, token, wf_name = NULL, page_size = 10, stage = "prod") {
  assertthat::assert_that(stage %in% orca_stages())
  wf_name_qstring <- ""
  if (!is.null(wf_name)) {
    wf_name_qstring <- glue::glue("&workflow__workflowName={wf_name}")
  }
  ep <- glue::glue("https://workflow.{stage}.umccr.org/api/v1/workflowrun/")
  url <- glue::glue("{ep}?libraries__libraryId={libid}&rowsPerPage={page_size}{wf_name_qstring}")
  x <- orca_query_url(url, token)
  res <- x[["results"]]
  d <- tibble::tibble(
    prid = res |> purrr::map_chr("portalRunId", .default = NA),
    wfr_name = res |> purrr::map_chr("workflowRunName", .default = NA),
    wf_id = res |> purrr::map_int(list("workflow", "id"), .default = NA),
    wf_name = res |> purrr::map_chr(list("workflow", "workflowName"), .default = NA),
    wf_version = res |> purrr::map_chr(list("workflow", "workflowVersion"), .default = NA),
    libraries = res |> purrr::map(\(y) y |>
      purrr::pluck("libraries") |>
      purrr::map_chr("libraryId")),
    currentStateStatus = res |> purrr::map_chr(list("currentState", "status"), .default = NA),
    currentStateTimestamp = res |> purrr::map_chr(list("currentState", "timestamp"), .default = NA)
  )
  d
}

#' OrcaBus List Workflow Runs
#'
#' @param wf_name Name of workflow. If NULL, returns all.
#' @param token JWT.
#' @param page_size Maximum number of rows to return.
#' @param stage Environment where API is deployed (prod, stg or dev).
#'
#' @examples
#' \dontrun{
#' token <- orca_jwt() |> jwt_validate()
#' wf_name <- NULL
#' wf_name <- "cttsov2"
#' orca_workflow_list(wf_name = wf_name, token = token)
#' }
#' @return Tibble with results.
#'
#' @export
orca_workflow_list <- function(wf_name = NULL, token, page_size = 10, stage = "prod") {
  assertthat::assert_that(stage %in% orca_stages())
  wf_name_qstring <- ""
  if (!is.null(wf_name)) {
    wf_name_qstring <- glue::glue("&workflow__workflowName={wf_name}")
  }
  ep <- glue::glue("https://workflow.{stage}.umccr.org/api/v1/workflowrun/")
  url <- glue::glue("{ep}?rowsPerPage={page_size}{wf_name_qstring}")
  x <- orca_query_url(url, token)
  res <- x[["results"]]
  d <- tibble::tibble(
    prid = res |> purrr::map_chr("portalRunId", .default = NA),
    wfr_name = res |> purrr::map_chr("workflowRunName", .default = NA),
    wf_id = res |> purrr::map_int(list("workflow", "id"), .default = NA),
    wf_name = res |> purrr::map_chr(list("workflow", "workflowName"), .default = NA),
    wf_version = res |> purrr::map_chr(list("workflow", "workflowVersion"), .default = NA),
    libraries = res |> purrr::map(\(y) y |>
      purrr::pluck("libraries") |>
      purrr::map_chr("libraryId")),
    currentStateStatus = res |> purrr::map_chr(list("currentState", "status"), .default = NA),
    currentStateTimestamp = res |> purrr::map_chr(list("currentState", "timestamp"), .default = NA)
  )
  return(d)
}

orca_stages <- function() {
  c("prod", "stg", "dev")
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
