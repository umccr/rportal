#' Metadata for tso_ctdna_tumor_only workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_tso_ctdna_tumor_only(pmeta))
#' @testexamples
#' expect_equal(length(unique(m$portal_run_id)), 4)
#' @export
meta_tso_ctdna_tumor_only <- function(pmeta, status = c("Succeeded")) {
  # retrieve workflow runs with the given type and status
  type <- "tso_ctdna_tumor_only"
  wf <- pmeta |>
    dplyr::filter(
      .data$type_name == type,
      .data$end_status %in% status
    )
  if (nrow(wf) == 0) {
    return(wf)
  }
  # grab libid/sampleid from the input meta, and outdir from the output meta
  d <- wf |>
    meta_io_fromjson() |>
    dplyr::mutate(
      # input
      sample_id = purrr::map_chr(.data$input, list("tso500_samples", "sample_id"), .default = NA),
      sample_name2 = purrr::map_chr(.data$input, list("tso500_samples", "sample_name"), .default = NA),
      # output
      gds_outdir = purrr::map_chr(.data$output, list("output_results_dir", "location"), .default = NA),
      libid1 = sub(".*_(L.*)", "\\1", .data$sample_id),
      rerun = grepl("rerun", .data$libid1),
      subjectid = sub("umccr__automated__tso_ctdna_tumor_only__(SBJ.*)__L.*", "\\1", .data$wfr_name),
      libid = sub("umccr__automated__tso_ctdna_tumor_only__SBJ.*__(L.*)__.*", "\\1", .data$wfr_name), # equal to libid1 wo _rerun
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "year", "durationMin",
      SubjectID = "subjectid",
      LibraryID = "libid",
      SampleID = "sample_name2",
      "gds_outdir",
      cttso_rerun = "rerun"
    )
}

#' Payload Tidy tso
#'
#' @param pld List with tso workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_cttsov2 <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqListRowIds into single string
  pdata[["tags"]][["fastqListRowIds"]] <- pdata[["tags"]][["fastqListRowIds"]] |>
    paste(collapse = ", ")
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # ignore verbose inputs$samplesheet
  inputs <- pdata[["inputs"]]
  inputs[["samplesheet"]] <- NULL
  inputs[["fastqListRows"]] <- inputs[["fastqListRows"]] |>
    purrr::map(tibble::as_tibble_row) |>
    dplyr::bind_rows()
  inputs[["fastqListRows"]] <- list(inputs[["fastqListRows"]])
  inputs <- inputs |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId")
  return(d)
}
