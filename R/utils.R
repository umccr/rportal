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

#' Are EnvVars Defined?
#'
#' @param vars Vector of env vars to check.
#'
#' @return Tibble with undefined env vars.
#'
#' @examples
#' envvar_defined("HOME")
#' @testexamples
#' expect_error(envvar_defined("FOOBAR"))
#' expect_true(envvar_defined("HOME"))
#' @export
envvar_defined <- function(vars) {
  env <- dplyr::tibble(
    var = vars
  ) |>
    dplyr::mutate(
      value = Sys.getenv(.data$var),
      defined = nchar(.data$value) > 0,
    ) |>
    dplyr::filter(!.data$defined)
  x <- paste(env[["var"]], collapse = ", ")
  msg <- glue::glue("Following required env variables not defined: {x}")
  assertthat::assert_that(nrow(env) == 0, msg = msg)
}
