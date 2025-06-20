#' Metadata for oncoanalyser_wgts_existing_both workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_oncoanalyser_wgts_existing_both(pmeta))
#' @testexamples
#' expect_equal(all(c("s3_outdir_oncoanalyser", "LibraryID_tumor_wts", "gds_bam_tumor_wgs") %in% colnames(m)), TRUE)
#' @export
meta_oncoanalyser_wgts_existing_both <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "oncoanalyser_wgts_existing_both"
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
      SampleID_tumor_wgs = purrr::map_chr(.data$input, "tumor_wgs_sample_id", .default = NA),
      SampleID_normal_wgs = purrr::map_chr(.data$input, "normal_wgs_sample_id", .default = NA),
      SampleID_tumor_wts = purrr::map_chr(.data$input, "tumor_wts_sample_id", .default = NA),
      LibraryID_tumor_wgs = purrr::map_chr(.data$input, "tumor_wgs_library_id", .default = NA),
      LibraryID_normal_wgs = purrr::map_chr(.data$input, "normal_wgs_library_id", .default = NA),
      LibraryID_tumor_wts = purrr::map_chr(.data$input, "tumor_wts_library_id", .default = NA),
      gds_bam_tumor_wgs = purrr::map_chr(.data$input, "tumor_wgs_bam", .default = NA),
      gds_bam_normal_wgs = purrr::map_chr(.data$input, "normal_wgs_bam", .default = NA),
      s3_bam_tumor_wts = purrr::map_chr(.data$input, "tumor_wts_bam", .default = NA),
      s3_indir_oncoanalyser_wgs = purrr::map_chr(.data$input, "existing_wgs_dir", .default = NA),
      s3_indir_oncoanalyser_wts = purrr::map_chr(.data$input, "existing_wts_dir", .default = NA),
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
      "LibraryID_tumor_wgs",
      "LibraryID_normal_wgs",
      "LibraryID_tumor_wts",
      "SampleID_tumor_wgs",
      "SampleID_normal_wgs",
      "SampleID_tumor_wts",
      "s3_outdir_oncoanalyser",
      "s3_indir_oncoanalyser_wgs",
      "s3_indir_oncoanalyser_wts",
      "gds_bam_tumor_wgs",
      "gds_bam_normal_wgs",
      "s3_bam_tumor_wts"
    )
}
