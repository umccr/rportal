#' Metadata for oncoanalyser_wgs workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_oncoanalyser_wgs(pmeta))
#' @testexamples
#' expect_equal(all(c("s3_outdir_oncoanalyser", "LibraryID_tumor", "gds_bam_tumor") %in% colnames(m)), TRUE)
#' @export
meta_oncoanalyser_wgs <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "oncoanalyser_wgs"
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
      SubjectID = purrr::map_chr(.data$input, "subject_id", .default = NA),
      SampleID_tumor = purrr::map_chr(.data$input, "tumor_wgs_sample_id", .default = NA),
      SampleID_normal = purrr::map_chr(.data$input, "normal_wgs_sample_id", .default = NA),
      LibraryID_tumor = purrr::map_chr(.data$input, "tumor_wgs_library_id", .default = NA),
      LibraryID_normal = purrr::map_chr(.data$input, "normal_wgs_library_id", .default = NA),
      gds_bam_tumor = purrr::map_chr(.data$input, "tumor_wgs_bam", .default = NA),
      gds_bam_normal = purrr::map_chr(.data$input, "normal_wgs_bam", .default = NA),
      # output
      s3_outdir_oncoanalyser = purrr::map_chr(.data$output, "output_directory", .default = NA),
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "year", "durationMin",
      "SubjectID",
      "LibraryID_tumor",
      "LibraryID_normal",
      "SampleID_tumor",
      "SampleID_normal",
      "s3_outdir_oncoanalyser",
      "gds_bam_tumor",
      "gds_bam_normal"
    )
}

#' Payload Tidy oncoanalyser-wgts-dna
#'
#' @param pld List with oncoanalyser-wgts-dna workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_oawgtsdna <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqListRowIds into single string
  pdata[["tags"]][["tumorFastqListRowIds"]] <- pdata[["tags"]][["tumorFastqListRowIds"]] |>
    paste(collapse = ", ")
  pdata[["tags"]][["normalFastqListRowIds"]] <- pdata[["tags"]][["normalFastqListRowIds"]] |>
    paste(collapse = ", ")
  tags <- pdata[["tags"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # remove trailing slashes from S3 directories
  inputs <- pdata[["inputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
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
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}
