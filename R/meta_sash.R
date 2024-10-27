#' Metadata for sash workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_sash(pmeta))
#' @testexamples
#' expect_equal(all(c("s3_indir_oncoanalyser", "LibraryID_tumor", "s3_outdir_sash") %in% colnames(m)), TRUE)
#' @export
meta_sash <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "sash"
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
      SampleID_tumor = purrr::map_chr(.data$input, "tumor_sample_id", .default = NA),
      SampleID_normal = purrr::map_chr(.data$input, "normal_sample_id", .default = NA),
      LibraryID_tumor = purrr::map_chr(.data$input, "tumor_library_id", .default = NA),
      LibraryID_normal = purrr::map_chr(.data$input, "normal_library_id", .default = NA),
      gds_indir_dragen_somatic = purrr::map_chr(.data$input, "dragen_somatic_dir", .default = NA),
      gds_indir_dragen_germline = purrr::map_chr(.data$input, "dragen_germline_dir", .default = NA),
      s3_indir_oncoanalyser = purrr::map_chr(.data$input, "oncoanalyser_dir", .default = NA),
      # output
      s3_outdir_sash = purrr::map_chr(.data$output, "output_directory", .default = NA),
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
      "s3_outdir_sash",
      "s3_indir_oncoanalyser",
      "gds_indir_dragen_somatic",
      "gds_indir_dragen_germline"
    )
}
