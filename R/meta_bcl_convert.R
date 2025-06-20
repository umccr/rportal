#' Metadata for bcl_convert workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_bcl_convert(pmeta))
#' \dontrun{
#' q1 <- glue("WHERE \"type_name\" = 'bcl_convert' ORDER BY \"start\" DESC LIMIT 4;")
#' pmeta_raw <- portaldb_query_workflow(q1)
#' m2 <- meta_bcl_convert(pmeta)
#' }
#' @testexamples
#' expect_equal(sum(!is.na(m$topup_or_rerun)), 0)
#' expect_equal(length(unique(m$portal_run_id)), 4)
#' @export
meta_bcl_convert <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "bcl_convert"
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
      batch_name = purrr::map(.data$input, list("settings_by_samples", "batch_name")),
      samples = purrr::map(.data$input, list("settings_by_samples", "samples")),
      runfolder_name = purrr::map_chr(.data$input, "runfolder_name"),
      gds_indir_bcl = purrr::map_chr(.data$input, list("bcl_input_directory", "location"), .default = NA),
      # output
      gds_outdir_multiqc = purrr::map_chr(.data$output, list("bclconvert_multiqc_out", "location"), .default = NA),
      gds_outdir_multiqc_interop = purrr::map_chr(.data$output, list("interop_multiqc_out", "location"), .default = NA),
      gds_outdirs_fastq = purrr::map(.data$output, list("fastq_directories", "location"), .default = NA)
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      samples2 = list(purrr::set_names(.data$samples, .data$batch_name)),
      samples2 = list(.data$samples2 |> purrr::list_flatten() |> tibble::enframe(name = "batch_name", value = "sample"))
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c("samples", "batch_name")) |>
    tidyr::unnest("samples2") |>
    tidyr::unnest("sample")

  # need to take care of following patterns:
  # - sampleid_libid
  # - sampleid_libid_topup(2/3)
  # - sampleid_libid_rerun(2)
  # - sampleidA_sampleidB_libid_..
  # So we just say .*_(L.*) will be libid1, then split that based on topup/rerun
  d |>
    tidyr::separate_wider_regex("sample", c(sampleid = ".*", "_", libid1 = "L.*"), cols_remove = FALSE) |>
    tidyr::separate_wider_regex("libid1", c(libid2 = ".*", "_", topup_or_rerun = ".*"), cols_remove = FALSE, too_few = "align_start") |>
    dplyr::mutate(
      gds_outdir_reports = file.path(dirname(.data$gds_outdir_multiqc), .data$batch_name, "Reports"),
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    ) |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "year", "durationMin",
      -dplyr::any_of(c("batch_run")), # NA for bcl_convert
      SampleID = "sampleid",
      LibraryID = "libid2",
      "topup_or_rerun",
      "batch_name",
      "runfolder_name",
      "gds_outdirs_fastq",
      "gds_outdir_reports",
      "gds_outdir_multiqc",
      "gds_outdir_multiqc_interop",
      "gds_indir_bcl"
    )
}
