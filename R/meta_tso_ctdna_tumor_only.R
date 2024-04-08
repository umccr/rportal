#' Metadata for tso_ctdna_tumor_only workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- system.file("extdata/portal_meta_top4.csv", package = "rportal")
#' (m <- meta_tso_ctdna_tumor_only(pmeta))
#' @testexamples
#' expect_equal(length(unique(m$portal_run_id)), 2)
#' @export
meta_tso_ctdna_tumor_only <- function(pmeta, status = c("Succeeded")) {
  # retrieve workflow runs with the given type and status
  type <- "tso_ctdna_tumor_only"
  wf <- portal_meta_read(pmeta) |>
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
      libid = sub("umccr__automated__tso_ctdna_tumor_only__SBJ.*__(L.*)__.*", "\\1", .data$wfr_name) # equal to libid1 wo _rerun
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      SubjectID = "subjectid",
      LibraryID = "libid",
      SampleID = "sample_name2",
      "gds_outdir",
      cttso_rerun = "rerun"
    )
}
