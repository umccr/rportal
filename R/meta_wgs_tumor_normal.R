#' Metadata for wgs_tumor_normal workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_wgs_tumor_normal(pmeta))
#' @testexamples
#' expect_equal("SubjectID" %in% colnames(m), TRUE)
#' @export
meta_wgs_tumor_normal <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "wgs_tumor_normal"
  wf <- pmeta |>
    dplyr::filter(
      .data$type_name == type,
      .data$end_status %in% status
    )
  extract_fqlist_el <- function(x, fq, y) {
    val <- x[[fq]][[y]]
    if (!is.null(val)) {
      return(paste(unique(val), collapse = ","))
    } else {
      return(NA)
    }
  }
  if (nrow(wf) == 0) {
    return(wf)
  }
  d <- wf |>
    meta_io_fromjson() |>
    dplyr::mutate(
      # input
      SampleID_normal = purrr::map_chr(.data$input, extract_fqlist_el, "fastq_list_rows", "rgsm"),
      SampleID_tumor = purrr::map_chr(.data$input, extract_fqlist_el, "tumor_fastq_list_rows", "rgsm"),
      LibraryID_normal = purrr::map_chr(.data$input, extract_fqlist_el, "fastq_list_rows", "rglb"),
      LibraryID_tumor = purrr::map_chr(.data$input, extract_fqlist_el, "tumor_fastq_list_rows", "rglb"),
      gds_infile_dragen_ref_tar = purrr::map_chr(.data$input, list("reference_tar", "location"), .default = NA),
      # output
      gds_outdir_dragen_germline = purrr::map_chr(.data$output, list("dragen_germline_output_directory", "location"), .default = NA), # NA in old runs
      gds_outdir_dragen_somatic = purrr::map_chr(.data$output, list("dragen_somatic_output_directory", "location"), .default = NA),
      gds_outdir_multiqc = purrr::map_chr(.data$output, list("multiqc_output_directory", "location"), .default = NA),
      gds_outfile_dragen_germline_snv_vcf = purrr::map_chr(.data$output, list("germline_snv_vcf_out", "location"), .default = NA), # NA in old runs
      gds_outfile_dragen_somatic_snv_vcf = purrr::map_chr(.data$output, list("somatic_snv_vcf_out", "location"), .default = NA),
      gds_outfile_dragen_somatic_snv_vcf_hardfilt = purrr::map_chr(.data$output, list("somatic_snv_vcf_hard_filtered_out", "location"), .default = NA),
      gds_outfile_dragen_somatic_sv_vcf = purrr::map_chr(.data$output, list("somatic_structural_vcf_out", "location"), .default = NA),
      SubjectID = sub("umccr__automated__wgs_tumor_normal__(SBJ.....)__L.*", "\\1", .data$wfr_name) # infer from wfr name
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      -dplyr::any_of(c("sequence_run", "batch_run")), # NA for wgs_tumor_normal
      "SubjectID",
      "LibraryID_tumor",
      "LibraryID_normal",
      "SampleID_tumor",
      "SampleID_normal",
      "gds_infile_dragen_ref_tar",
      "gds_outdir_dragen_somatic",
      "gds_outdir_dragen_germline",
      "gds_outdir_multiqc",
      "gds_outfile_dragen_germline_snv_vcf",
      "gds_outfile_dragen_somatic_snv_vcf",
      "gds_outfile_dragen_somatic_snv_vcf_hardfilt",
      "gds_outfile_dragen_somatic_sv_vcf"
    )
}
