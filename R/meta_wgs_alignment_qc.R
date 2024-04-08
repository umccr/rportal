#' Metadata for wgs_alignment_qc workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- system.file("extdata/portal_meta_top4.csv", package = "rportal")
#' (m <- meta_wgs_alignment_qc(pmeta))
#' @testexamples
#' expect_equal("Lane" %in% colnames(m), TRUE)
#' @export
meta_wgs_alignment_qc <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "wgs_alignment_qc"
  wf <- portal_meta_read(pmeta) |>
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
      rglb = purrr::map_chr(.data$input, list("fastq_list_rows", "rglb")),
      rgsm = purrr::map_chr(.data$input, list("fastq_list_rows", "rgsm")),
      lane = purrr::map_int(.data$input, list("fastq_list_rows", "lane")),
      lane = as.character(.data$lane),
      # output
      gds_outdir_dragen = purrr::map_chr(.data$output, list("dragen_alignment_output_directory", "location"), .default = NA),
      gds_outdir_multiqc = purrr::map_chr(.data$output, list("multiqc_output_directory", "location"), .default = NA),
      SubjectID = sub("umccr__automated__wgs_alignment_qc__(SBJ.*)__L.*", "\\1", .data$wfr_name),
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "SubjectID",
      LibraryID = "rglb",
      SampleID = "rgsm",
      Lane = "lane",
      "gds_outdir_dragen",
      "gds_outdir_multiqc",
    )
}
