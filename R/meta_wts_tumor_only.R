#' Metadata for wts_tumor_only workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_wts_tumor_only(pmeta))
#' @testexamples
#' expect_equal(length(unique(m$portal_run_id)), 4)
#' expect_equal(length(unique(m$LibraryID)), 4)
#' @export
meta_wts_tumor_only <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "wts_tumor_only"
  wf <- pmeta |>
    dplyr::filter(
      .data$type_name == type,
      .data$end_status %in% status
    )
  if (nrow(wf) == 0) {
    return(wf)
  }
  d <- wf |>
    meta_io_fromjson() |>
    dplyr::mutate(
      # input
      rglb = purrr::map_chr(.data$input, \(x) unique(x[["fastq_list_rows"]][["rglb"]]) %||% NA),
      rgsm = purrr::map_chr(.data$input, \(x) unique(x[["fastq_list_rows"]][["rgsm"]]) %||% NA),
      lane = purrr::map_chr(.data$input, \(x) paste(x[["fastq_list_rows"]][["lane"]], collapse = ",")),
      lane = as.character(.data$lane),
      # output
      gds_outdir_dragen = purrr::map_chr(.data$output, list("dragen_transcriptome_output_directory", "location"), .default = NA),
      gds_outdir_multiqc = purrr::map_chr(.data$output, list("multiqc_output_directory", "location"), .default = NA),
      gds_outdir_arriba = purrr::map_chr(.data$output, list("arriba_output_directory", "location"), .default = NA),
      gds_outdir_qualimap = purrr::map_chr(.data$output, list("qualimap_output_directory", "location"), .default = NA),
      SubjectID = sub("umccr__.*__wts_tumor_only__(SBJ.*)__L.*", "\\1", .data$wfr_name),
      SubjectID = ifelse(
        !grepl("external_apgi", .data$wfr_name),
        .data$SubjectID,
        sub("umccr__external_apgi__wts_tumor_only__(.*)", "\\1", .data$wfr_name)
      ),
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "year", "durationMin",
      "SubjectID",
      LibraryID = "rglb",
      SampleID = "rgsm",
      Lane = "lane",
      "gds_outdir_dragen",
      "gds_outdir_multiqc",
      "gds_outdir_arriba",
      "gds_outdir_qualimap"
    )
}

#' Payload for WTS workflow
#'
#' @param pld List with WTS workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_wts <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqListRowIds into single string
  pdata[["tags"]][["tumorFastqListRowIds"]] <- pdata[["tags"]][["tumorFastqListRowIds"]] |>
    paste(collapse = ", ")
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  inputs <- pdata[["inputs"]] |>
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
