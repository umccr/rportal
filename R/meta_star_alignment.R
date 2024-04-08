#' Metadata for star_alignment workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_star_alignment(pmeta))
#' @testexamples
#' expect_equal(all(c("s3_outdir_star", "LibraryID") %in% colnames(m)), TRUE)
#' @export
meta_star_alignment <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "star_alignment"
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
      SampleID = purrr::map_chr(.data$input, "sample_id", .default = NA),
      LibraryID = purrr::map_chr(.data$input, "library_id", .default = NA),
      gds_fq_fwd = purrr::map_chr(.data$input, "fastq_fwd", .default = NA),
      gds_fq_rev = purrr::map_chr(.data$input, "fastq_rev", .default = NA),
      # output
      s3_outdir_star = purrr::map_chr(.data$output, "output_directory", .default = NA)
    )
  d |>
    dplyr::select(
      meta_main_cols(),
      "SubjectID",
      "LibraryID",
      "SampleID",
      "s3_outdir_star",
      "gds_fq_fwd",
      "gds_fq_rev"
    )
}
