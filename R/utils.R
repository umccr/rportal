meta_io_fromjson <- function(pmeta) {
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

#' @noRd
dummy1 <- function() {
  # Solves R CMD check: Namespaces in Imports field not imported from
  dracarys::gds_list_files_filter_relevant
  optparse::make_option
  fs::dir_create
}

#' Are AWS/ICA EnvVars Undefined?
#'
#' @return Tibble with undefined env vars.
#' @export
envvar_undefined <- function() {
  env <- dplyr::tibble(
    var = c(
      "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION", "ICA_ACCESS_TOKEN"
    )
  ) |>
    dplyr::mutate(
      value = Sys.getenv(.data$var),
      defined = nchar(.data$value) > 0,
    ) |>
    dplyr::filter(!.data$defined) |>
    dplyr::pull("var")
  env
}
