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

#' OrcaBus PortalRunId To WFR ID
#'
#' Given an OrcaBus portalRunId, provides the workflow run ID.
#'
#' @param prid portalRunId.
#' @param token JWT to use for authentication.
#' @param stage Environment where API is deployed (prod, stg or dev).
#'
#' @return Workflow run ID.
#' @examples
#' \dontrun{
#' prid <- "20241110c01a1c76"
#' prid <- "202409303ed604f4"
#' token <- orca_jwt() |> jwt_validate()
#' wfrid <- orca_prid2wfrid(prid = prid, token = token)
#' }
#'
#' @export
orca_prid2wfrid <- function(prid, token, stage = "prod") {
  ep <- glue("https://workflow.{stage}.umccr.org/api/v1/workflowrun")
  url <- glue("{ep}?portalRunId={prid}")
  resp <- orca_query_url(url, token)
  assertthat::assert_that(length(resp[["results"]]) == 1)
  res <- resp[["results"]][[1]]
  assertthat::assert_that("orcabusId" %in% names(res))
  resp[["results"]][[1]][["orcabusId"]]
}

#' OrcaBus WFR ID To State
#'
#' Given a workflow run ID, provides the state.
#'
#' @param wfrid Workflow run ID.
#' @param token JWT to use for authentication.
#' @param stage Environment where API is deployed (prod, stg or dev).
#'
#' @return Tibble with results.
#' @examples
#' \dontrun{
#' token <- orca_jwt() |> jwt_validate()
#' wfrid <- "wfr.01JCARAVTXKG5581SRA1HKBTD3"
#' orca_wfrid2state(wfrid = wfrid, token = token)
#' }
#'
#' @export
orca_wfrid2state <- function(wfrid, token, stage = "prod") {
  wfrid <- sub("^wfr\\.", "", wfrid) # need to strip prefix
  ep <- glue("https://workflow.{stage}.umccr.org/api/v1/workflowrun/{wfrid}/state")
  orca_query_url(ep, token) |>
    dplyr::bind_rows()
}

#' OrcaBus WFR ID To Payload
#'
#' Given a workflow run ID, provides the payload.
#'
#' @param wfrid Workflow run ID.
#' @param token JWT to use for authentication.
#' @param stage Environment where API is deployed (prod, stg or dev).
#'
#' @return List of workflow payload.
#' @examples
#' \dontrun{
#' token <- orca_jwt() |> jwt_validate()
#' wfrid <- "wfr.01JCARAVTXKG5581SRA1HKBTD3"
#' wfrid <- "wfr.01JCA5DZFD0T4MFQX0HHEEFBCH" # wts
#' wfrid <- "wfr.01JBX361HKV0V9WS96RAFG135T" # cttsov2
#' wfrid <- "wfr.01JD20PQGQDAEAWD5S9E6M10J6" # cttsov2
#' wfrid <- "wfr.01JD20PQGQTY8SNSEZTZ8XF5N9" # wgs-tn
#' wfrid <- "wfr.01JE08ZV50519JTVH3Z4N8BQV2" # bsshfastqcopy
#' wfrid <- "wfr.01JDK5068AEV2RDNMH970B5W71" # bclconvert-interop-qc
#' wfrid <- "wfr.01JDE02YAQT441W1D26F0DXZ8J" # oncoanalyser-wgts-dna-rna
#' wfrid <- "wfr.01JDCCGRN6D34XCNE8N2XMQ91Q" # oncoanalyser-wgts-rna
#' wfrid <- "wfr.01JDBW5T3T1SDMCW5GN3SK10F8" # ora-compression
#' wfrid <- "wfr.01JDCPRG7DGH1KA9X90Y4YBYZX" # pieriandx
#' wfrid <- "wfr.01JDGB7G746GBXYPKNH7PRQ39S" # rnasum
#' p <- orca_wfrid2payload(wfrid = wfrid, token = token)
#' }
#'
#' @export
orca_wfrid2payload <- function(wfrid, token, stage = "prod") {
  states <- orca_wfrid2state(wfrid, token, stage)
  pld <- states |>
    dplyr::filter(.data$status == "SUCCEEDED") |>
    dplyr::pull("payload")
  msg <- glue("For {wfrid} we had {nrow(states)} states and {length(pld)} SUCCEEDED statuses.")
  assertthat::assert_that(length(pld) == 1, msg = msg)
  ep <- glue("https://workflow.{stage}.umccr.org/api/v1/payload/{pld}")
  orca_query_url(ep, token)
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
#' prid <- "20241110c01a1c76" # cttso
#' prid <- "2024111638b77605" # sash
#' prid <- "20241116dfee1aef" # oa-wgts-dna
#' prid <- "2024111514abc96a" # wgs-tn
#' prid <- "20241115cbfdaeea" # wgs-qc-rna
#' prid <- "202411154e2c74f3" # wgs-qc-dna
#' prid <- "202411231acb8163" # wgs-qc-dna
#' prid <- "2024111507e8ca78" # bclconvert
#' prid <- "202411152feba98c" # bclconvert-interopqc
#' token <- orca_jwt() |> jwt_validate()
#' p <- orca_prid2wfpayload(prid = prid, token = token)
#' }
#'
#' @export
orca_prid2wfpayload <- function(prid, token, stage = "prod") {
  assertthat::assert_that(stage %in% orca_stages())
  wfrid <- orca_prid2wfrid(prid = prid, token = token, stage = stage)
  pld <- orca_wfrid2payload(wfrid = wfrid, token = token, stage = stage)
  return(pld)
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
#' libid <- "L2401591" # wgs
#' libid <- "L2401074" # wts # nothing
#' libid <- "L2401577" # wts
#' libid <- "L2401558" # cttsov2
#' libid <- "L2401608" # wgs
#' libid <- "L2401603" # wgs
#' libid <- "L2401610" # oa
#' libid <- "L2401445" # cttsov2
#' wf_name <- NULL
#' token <- orca_jwt() |> jwt_validate()
#' d <- orca_libid2workflows(libid = libid, token = token, wf_name = wf_name, page_size = 20)
#' }
#' @export
orca_libid2workflows <- function(libid, token, wf_name = NULL, page_size = 10, stage = "prod") {
  assertthat::assert_that(stage %in% orca_stages())
  wf_name_qstring <- ""
  if (!is.null(wf_name)) {
    wf_name_qstring <- glue("&workflow__workflowName={wf_name}")
  }
  ep <- glue("https://workflow.{stage}.umccr.org/api/v1/workflowrun/")
  url <- glue("{ep}?libraries__libraryId={libid}&rowsPerPage={page_size}{wf_name_qstring}")
  x <- orca_query_url(url, token)
  res <- x[["results"]]
  d <- tibble::tibble(
    orcabusId = res |> purrr::map_chr("orcabusId", .default = NA),
    currentStateOrcabusId = res |> purrr::map_chr(list("currentState", "orcabusId"), .default = NA),
    currentStateStatus = res |> purrr::map_chr(list("currentState", "status"), .default = NA),
    currentStateTimestamp = res |> purrr::map_chr(list("currentState", "timestamp"), .default = NA),
    portalRunId = res |> purrr::map_chr("portalRunId", .default = NA),
    wfr_name = res |> purrr::map_chr("workflowRunName", .default = NA),
    wf_id = res |> purrr::map_chr(list("workflow", "orcabusId"), .default = NA),
    wf_name = res |> purrr::map_chr(list("workflow", "workflowName"), .default = NA),
    wf_version = res |> purrr::map_chr(list("workflow", "workflowVersion"), .default = NA),
    executionId = res |> purrr::map_chr("executionId", .default = NA),
    comment = res |> purrr::map_chr("comment", .default = NA),
    analysisRun = res |> purrr::map_chr("analysisRun", .default = NA)
  )
  d |>
    dplyr::select("portalRunId", "wf_name", "wf_version", "orcabusId", "currentStateStatus", dplyr::everything())
}

#' OrcaBus List Workflow Runs
#'
#' @param wf_name Name of workflow. If NULL, returns all.
#' @param token JWT.
#' @param page_size Maximum number of rows to return.
#' @param stage Environment where API is deployed (prod, stg or dev).
#' @param status Run status.
#'
#' @examples
#' \dontrun{
#' wf_name <- NULL
#' wf_name <- "umccrise"
#' token <- orca_jwt() |> jwt_validate()
#' wfs <- orca_workflow_list(wf_name = wf_name, token = token, page_size = 500)
#' }
#' @return Tibble with results.
#'
#' @export
orca_workflow_list <- function(wf_name = NULL, status = "SUCCEEDED", token, page_size = 10, stage = "prod") {
  assertthat::assert_that(stage %in% orca_stages())
  wf_name_qstring <- ""
  if (!is.null(wf_name)) {
    wf_name_qstring <- glue("&workflow__workflowName={wf_name}")
  }
  status_qstring <- ""
  if (!is.null(status)) {
    status_qstring <- glue("&status={status}")
  }
  ordering <- "-portal_run_id"
  ep <- glue("https://workflow.{stage}.umccr.org/api/v1/workflowrun/")
  url <- glue("{ep}?rowsPerPage={page_size}&ordering={ordering}{wf_name_qstring}{status_qstring}")
  cli::cli_alert_info("Query {url}")
  x <- orca_query_url(url, token)
  res <- x[["results"]]
  d <- tibble::tibble(
    orcabusId = res |> purrr::map_chr("orcabusId", .default = NA),
    portalRunId = res |> purrr::map_chr("portalRunId", .default = NA),
    executionId = res |> purrr::map_chr("executionId", .default = NA),
    workflowRunName = res |> purrr::map_chr("workflowRunName", .default = NA),
    comment = res |> purrr::map_chr("comment", .default = NA),
    analysisRun = res |> purrr::map_chr("analysisRun", .default = NA),
    workflowId = res |> purrr::map_chr(list("workflow", "orcabusId"), .default = NA),
    workflowName = res |> purrr::map_chr(list("workflow", "workflowName"), .default = NA),
    workflowVersion = res |> purrr::map_chr(list("workflow", "workflowVersion"), .default = NA),
    currentStateOrcabusId = res |> purrr::map_chr(list("currentState", "orcabusId"), .default = NA),
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
