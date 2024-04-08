meta_io_fromjson <- function(pmeta) {
  pmeta <- portal_meta_read(pmeta)
  pmeta |>
    dplyr::rowwise() |>
    dplyr::mutate(
      input = list(jsonlite::fromJSON(.data$input)),
      output = ifelse(!is.na(.data$output), list(jsonlite::fromJSON(.data$output)), NA)
    ) |>
    dplyr::ungroup()
}

meta_main_cols <- function() {
  c(
    "id", "wfr_name", "wfr_id", "version", "end_status", # "sequence_run", "batch_run",
    "start", "end", "portal_run_id"
  )
}

#' Read ICA Workflows Metadata via Portal API
#'
#' Reads ICA Workflows Metadata via Portal API using awscurl. See
#' https://github.com/okigan/awscurl for required `AWS_` environment variables.
#'
#' @param rows Number of rows to return.
#' @param params String containing additional params to pass to the `/workflows`
#' endpoint, e.g. `'&type_name=bclconvert'`.
#' @param pmeta Path to downloaded portal metadata file, or already parsed metadata tibble.
#' @param account UMCCR portal account (one of "prod", "dev", "stg").
#'
#' @return A tibble of the results from the given query.
#' @export
#'
#' @examples
#' \dontrun{
#' portal_meta_read(params = "&type_name=rnasum", rows = 4)
#' }
portal_meta_read <- function(pmeta = NULL, rows = 100, params = "", account = "prod") {
  assertthat::assert_that(account %in% c("prod", "dev", "stg"))
  au_tz <- "Australia/Melbourne"
  utc_tz <- "UTC"
  if (!is.null(pmeta)) {
    # if already parsed, just return it
    if (inherits(pmeta, "data.frame")) {
      assertthat::assert_that(
        all(c(
          "wfr_name", "type_name", "start", "end", "input", "output",
          "portal_run_id", "wfr_id", "wfl_id", "wfv_id", "version", "end_status"
        ) %in% colnames(pmeta))
      )
      return(pmeta)
    } else if (file.exists(pmeta)) {
      # local file downloaded via Portal
      # keep all character except start/end
      # and change to AEST timezone
      date_fmt_z <- "%Y-%m-%dT%H:%M:%SZ"
      ctypes <- readr::cols(
        .default = "c",
        start = readr::col_datetime(format = date_fmt_z),
        end = readr::col_datetime(format = date_fmt_z),
      )
      res <- pmeta |>
        readr::read_csv(col_types = ctypes) |>
        dplyr::mutate(
          start = lubridate::with_tz(.data$start, tz = au_tz),
          end = lubridate::with_tz(.data$end, tz = au_tz)
        )
      return(res)
    } else {
      stop("pmeta should be an already parsed dataframe or a local file.")
    }
  } # else pmeta is NULL, so read via portal API

  base_url <- glue("https://api.portal.{account}.umccr.org/iam")
  url1 <- utils::URLencode(glue("{base_url}/workflows?rowsPerPage={rows}{params}"))
  awscurl_cmd <- glue(
    "awscurl '{url1}' ",
    "--header 'Accept: application/json'"
  )
  message(glue("Running {awscurl_cmd}"))
  j <- system(awscurl_cmd, intern = TRUE)
  date_fmt <- "%Y-%m-%dT%H:%M:%S"
  d <- j |>
    jsonlite::fromJSON() |>
    purrr::pluck("results") |>
    tibble::as_tibble()
  d |>
    dplyr::mutate(
      start = as.POSIXct(.data$start, tz = utc_tz, format = date_fmt),
      end = as.POSIXct(.data$end, tz = utc_tz, format = date_fmt),
      start = lubridate::with_tz(.data$start, tz = au_tz),
      end = lubridate::with_tz(.data$end, tz = au_tz)
    )
}

#' Read ICA Workflows Metadata via Athena
#'
#' Reads the ICA Workflows Metadata for the given workflow run IDs.
#'
#' @param wfrids Character vector of wfr IDs to query.
#'
#' @return Tibble with a row per wfr ID.
#'
#' @examples
#' \dontrun{
#' wfrids <- c("wfr.1e764ca00e7a43a69e2424f250a34868")
#' portal_meta_read_athena(wfrids)
#' }
#' @export
portal_meta_read_athena <- function(wfrids = NULL) {
  assertthat::assert_that(!is.null(wfrids), all(grepl("^wfr\\.", wfrids)))
  RAthena::RAthena_options(clear_s3_resource = FALSE)
  con <- DBI::dbConnect(
    RAthena::athena(),
    work_group = "data_portal",
    rstudio_conn_tab = FALSE
  )
  wfrids_quote <- paste(shQuote(wfrids), collapse = ", ")
  q1 <- glue(
    'SELECT * FROM "data_portal"."data_portal"."data_portal_workflow" where wfr_id in ({wfrids_quote});'
  )
  RAthena::dbGetQuery(con, q1) |>
    tibble::as_tibble()
}


portal_meta_read_athena_tmp <- function(x = NULL) {
  assertthat::assert_that(!is.null(x))
  RAthena::RAthena_options(clear_s3_resource = FALSE)
  con <- DBI::dbConnect(
    RAthena::athena(),
    work_group = "data_portal",
    rstudio_conn_tab = FALSE
  )
  q1 <- glue(
    'SELECT * FROM "data_portal"."data_portal"."data_portal_workflow" {x}'
  )
  RAthena::dbGetQuery(con, q1) |>
    tibble::as_tibble()
}

#' Read Portal Limsrow Table
#'
#' Reads rows from the data portal's limsrow table, given a set of `LibraryID`s
#' to query.
#'
#' @param libids Character vector of LibraryID values to query for.
#'
#' @return Tibble with all rows from the data portal limsrow table where
#' there are hits with the `library_id` column.
#' @export
glims_portal_read <- function(libids) {
  assertthat::assert_that(!is.null(libids), all(grepl("^L", libids)))
  libids <- unique(libids)
  RAthena::RAthena_options(clear_s3_resource = FALSE)
  con <- DBI::dbConnect(
    RAthena::athena(),
    work_group = "data_portal",
    rstudio_conn_tab = FALSE
  )
  q_quote <- shQuote(paste(libids, collapse = "|"))
  q1 <- glue(
    'SELECT * FROM "data_portal"."data_portal"."data_portal_limsrow" where REGEXP_LIKE("library_id", {q_quote});'
  )
  d <- RAthena::dbGetQuery(con, q1) |>
    tibble::as_tibble()
  DBI::dbDisconnect(con)
  d
}

#' Read Google LIMS
#'
#' Reads UMCCR's Google LIMS spreadsheet.
#'
#' @return Tibble with all columns and rows from the Google LIMS spreadsheet.
#' @export
glims_excel_read <- function() {
  lims_key <- googledrive::drive_find("^Google LIMS$", shared_drive = "LIMS")$id
  lims <- lims_key |>
    googlesheets4::read_sheet("Sheet1", na = c(".", "", "-"), col_types = "c")
  lims |> readr::type_convert(col_types = readr::cols(.default = "c", Timestamp = "T"))
}
