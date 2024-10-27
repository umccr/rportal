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
      durationMin = round(as.numeric(difftime(end, start, units = "mins")))
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
