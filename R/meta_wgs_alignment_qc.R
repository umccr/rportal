#' Metadata for wgs_alignment_qc workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_wgs_alignment_qc(pmeta))
#' @testexamples
#' expect_equal("lane" %in% colnames(m), TRUE)
#' @export
meta_wgs_alignment_qc <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "wgs_alignment_qc"
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
      rgid = purrr::map_chr(.data$input, list("fastq_list_rows", "rgid")),
      rgsm = purrr::map_chr(.data$input, list("fastq_list_rows", "rgsm")),
      rglb = purrr::map_chr(.data$input, list("fastq_list_rows", "rglb")),
      lane = purrr::map_int(.data$input, list("fastq_list_rows", "lane")),
      lane = as.character(.data$lane),
      # read_1/read_2 are dfs
      fq1 = purrr::map_chr(.data$input, list("fastq_list_rows", "read_1", "location"), .default = NA),
      fq2 = purrr::map_chr(.data$input, list("fastq_list_rows", "read_2", "location"), .default = NA),
      # output
      gds_outdir_dragen = purrr::map_chr(.data$output, list("dragen_alignment_output_directory", "location"), .default = NA),
      gds_outdir_multiqc = purrr::map_chr(.data$output, list("multiqc_output_directory", "location"), .default = NA),
      SubjectID = sub("umccr__.*__wgs_alignment_qc__(SBJ.*)__L.*", "\\1", .data$wfr_name),
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    ) |>
    tidyr::separate_wider_delim(
      cols = "rgid", delim = ".",
      names = c("index1", "index2", "lane2", "illumina_id", "sample_lib_id")
    )

  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "year", "durationMin",
      "SubjectID",
      LibraryID = "rglb",
      SampleID = "rgsm",
      "lane",
      "index1",
      "index2",
      "illumina_id",
      "fq1",
      "fq2",
      "gds_outdir_dragen",
      "gds_outdir_multiqc",
    )
}

#' Payload Tidy wgtsqc
#'
#' @param pld List with wgtsqc workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_wgtsqc <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # take care of fastqListRows lists
  inputs <- pdata[["inputs"]]
  assertthat::assert_that(
    all(purrr::map_int(inputs[["fastqListRow"]], length) == 1),
    msg = "Input fastqListRow is supposed to have 1 value per element."
  )
  inputs[["fastqListRow"]] <- inputs[["fastqListRow"]] |>
    tibble::as_tibble_row() |>
    list()
  inputs <- inputs |>
    tibble::as_tibble_row() |>
    tidyr::unnest("fastqListRow") |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(
      orcabusId = id,
      input_lane = as.character(.data$input_lane)
    )
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
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}
