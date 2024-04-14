#' Metadata for oncoanalyser_wts workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_oncoanalyser_wts(pmeta))
#' @testexamples
#' expect_equal(all(c("s3_outdir_oncoanalyser", "LibraryID", "s3_bam") %in% colnames(m)), TRUE)
#' @export
meta_oncoanalyser_wts <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "oncoanalyser_wts"
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
      mode = purrr::map_chr(.data$input, "mode", .default = NA),
      SubjectID = purrr::map_chr(.data$input, "subject_id", .default = NA),
      SampleID = purrr::map_chr(.data$input, "tumor_wts_sample_id", .default = NA),
      LibraryID = purrr::map_chr(.data$input, "tumor_wts_library_id", .default = NA),
      s3_bam = purrr::map_chr(.data$input, "tumor_wts_bam", .default = NA),
      # output
      s3_outdir_oncoanalyser = purrr::map_chr(.data$output, "output_directory", .default = NA)
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "SubjectID",
      "LibraryID",
      "SampleID",
      "s3_bam",
      "s3_outdir_oncoanalyser",
    )
}
