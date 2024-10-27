#' Metadata for rnasum workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_rnasum(pmeta))
#' @testexamples
#' expect_equal(m$rnasum_dataset[1], "PANCAN")
#' expect_equal(basename(m$gds_outfile_rnasum_html[4]), "PRJ222637.RNAseq_report.html")
#' @export
meta_rnasum <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "rnasum"
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
      # renamed in v1.1.0
      gds_indir_dragen = purrr::map_chr(.data$input, list("dragen_transcriptome_directory", "location"), .default = NA),
      gds_indir_dragen = ifelse(
        is.na(.data$gds_indir_dragen),
        purrr::map_chr(.data$input, list("dragen_wts_dir", "location"), .default = NA),
        .data$gds_indir_dragen
      ),
      # renamed in v1.1.0
      gds_indir_umccrise = purrr::map_chr(.data$input, list("umccrise_directory", "location"), .default = NA),
      gds_indir_umccrise = ifelse(
        is.na(.data$gds_indir_umccrise),
        purrr::map_chr(.data$input, list("umccrise", "location"), .default = NA),
        .data$gds_indir_umccrise
      ),
      # renamed in v1.1.0
      gds_indir_arriba = purrr::map_chr(.data$input, list("arriba_directory", "location"), .default = NA),
      gds_indir_arriba = ifelse(
        is.na(.data$gds_indir_arriba),
        purrr::map_chr(.data$input, list("arriba_dir", "location"), .default = NA),
        .data$gds_indir_arriba
      ),
      rnasum_sample_name = purrr::map_chr(.data$input, "sample_name", .default = NA),
      rnasum_dataset = purrr::map_chr(.data$input, "dataset", .default = NA),
      rnasum_report_dir = purrr::map_chr(.data$input, "report_directory", .default = NA),
      # renamed in v1.1.0
      rnasum_report_dir = ifelse(
        is.na(.data$rnasum_report_dir),
        purrr::map_chr(.data$input, "report_dir", .default = NA),
        .data$rnasum_report_dir
      ),
      sbjid1 = sub("(SBJ.*)__L.*", "\\1", .data$rnasum_report_dir),
      libid1 = sub("(SBJ.*)__(L.*)", "\\2", .data$rnasum_report_dir),
      # output
      gds_outfile_rnasum_html = purrr::map_chr(.data$output, list("rnasum_html", "location"), .default = NA),
      gds_outdir_rnasum = purrr::map_chr(.data$output, list("rnasum_output_directory", "location"), .default = NA),
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(end, start, units = "mins")))
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      -dplyr::any_of(c("sequence_run", "batch_run")), # NA for rnasum
      "year", "durationMin",
      SubjectID = "sbjid1",
      LibraryID = "libid1",
      SampleID = "rnasum_sample_name",
      "rnasum_dataset",
      "gds_indir_dragen",
      "gds_indir_umccrise",
      "gds_indir_arriba",
      "gds_outdir_rnasum",
      "gds_outfile_rnasum_html"
    )
}
