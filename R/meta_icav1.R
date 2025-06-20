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
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
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
      "LibraryID",
      "SampleID",
      "s3_bam",
      "s3_outdir_oncoanalyser",
    )
}

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
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
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
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
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
      s3_outdir_star = purrr::map_chr(.data$output, "output_directory", .default = NA),
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "year", "durationMin",
      "SubjectID",
      "LibraryID",
      "SampleID",
      "s3_outdir_star",
      "gds_fq_fwd",
      "gds_fq_rev"
    )
}

#' Metadata for tso_ctdna_tumor_only workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_tso_ctdna_tumor_only(pmeta))
#' @testexamples
#' expect_equal(length(unique(m$portal_run_id)), 4)
#' @export
meta_tso_ctdna_tumor_only <- function(pmeta, status = c("Succeeded")) {
  # retrieve workflow runs with the given type and status
  type <- "tso_ctdna_tumor_only"
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
      sample_id = purrr::map_chr(.data$input, list("tso500_samples", "sample_id"), .default = NA),
      sample_name2 = purrr::map_chr(.data$input, list("tso500_samples", "sample_name"), .default = NA),
      # output
      gds_outdir = purrr::map_chr(.data$output, list("output_results_dir", "location"), .default = NA),
      libid1 = sub(".*_(L.*)", "\\1", .data$sample_id),
      rerun = grepl("rerun", .data$libid1),
      subjectid = sub("umccr__automated__tso_ctdna_tumor_only__(SBJ.*)__L.*", "\\1", .data$wfr_name),
      libid = sub("umccr__automated__tso_ctdna_tumor_only__SBJ.*__(L.*)__.*", "\\1", .data$wfr_name), # equal to libid1 wo _rerun
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "year", "durationMin",
      SubjectID = "subjectid",
      LibraryID = "libid",
      SampleID = "sample_name2",
      "gds_outdir",
      cttso_rerun = "rerun"
    )
}

#' Metadata for umccrise workflow
#'
#' @param pmeta tibble with raw PortalDB workflow metadata.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with tidy metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_umccrise(pmeta))
#' @testexamples
#' expect_equal(all(c("LibraryID_normal", "LibraryID_tumor") %in% colnames(m)), TRUE)
#' @export
meta_umccrise <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  # The input/output json objects changed from 2023-04-07, so need to handle those too
  type <- "umccrise"
  wf <- pmeta |>
    dplyr::filter(
      .data$type_name == type,
      .data$end_status %in% status
    )
  extract_fqlist_el <- function(x, y) {
    val <- x[["fastq_list_rows_germline"]][[y]]
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
      ## input first
      # new runs
      dragen_normal_sampleid2 = purrr::map_chr(.data$input, "dragen_normal_id", .default = NA),
      dragen_tumor_sampleid2 = purrr::map_chr(.data$input, "dragen_tumor_id", .default = NA),
      gds_indir_dragen_somatic = purrr::map_chr(.data$input, list("dragen_somatic_directory", "location"), .default = NA), # same for old
      gds_indir_dragen_germline2 = purrr::map_chr(.data$input, list("dragen_germline_directory", "location"), .default = NA),
      umccrise_outdir_name2 = purrr::map_chr(.data$input, "output_directory_name", .default = NA),
      umccrise_genomes_tar2 = purrr::map_chr(.data$input, list("genomes_tar", "location"), .default = NA),
      sbjid2 = purrr::map_chr(.data$input, "subject_identifier", .default = NA),
      dragen_tumor_libid2 = sub("(L.*)__(L.*)", "\\1", .data$umccrise_outdir_name2),
      dragen_normal_libid2 = sub("(L.*)__(L.*)", "\\2", .data$umccrise_outdir_name2),
      # old runs
      dragen_normal_sampleid1 = purrr::map_chr(.data$input, extract_fqlist_el, "rgsm"),
      dragen_normal_lane1 = purrr::map_chr(.data$input, extract_fqlist_el, "lane"),
      umccrise_outdir_name1 = purrr::map_chr(.data$input, "output_directory_umccrise", .default = NA),
      umccrise_genomes_tar1 = purrr::map_chr(.data$input, list("reference_tar_umccrise", "location"), .default = NA),
      sbjid1 = purrr::map_chr(.data$input, "subject_identifier_umccrise", .default = NA),
      dragen_tumor_libid1 = sub("(L.*)__(L.*)", "\\1", .data$umccrise_outdir_name1),
      dragen_normal_libid1 = sub("(L.*)__(L.*)", "\\2", .data$umccrise_outdir_name1),
      # merge new with old
      SubjectID = dplyr::if_else(is.na(.data$sbjid1), .data$sbjid2, .data$sbjid1),
      LibraryID_normal = dplyr::if_else(
        is.na(.data$dragen_normal_libid1), .data$dragen_normal_libid2, .data$dragen_normal_libid1
      ),
      LibraryID_tumor = dplyr::if_else(
        is.na(.data$dragen_tumor_libid1), .data$dragen_tumor_libid2, .data$dragen_tumor_libid1
      ),
      SampleID_normal = dplyr::if_else(
        is.na(.data$dragen_normal_sampleid1), .data$dragen_normal_sampleid2, .data$dragen_normal_sampleid1
      ),
      SampleID_tumor = .data$dragen_tumor_sampleid2, # NA for old runs
      Lane_normal = .data$dragen_normal_lane1, # NA for new runs
      gds_indir_dragen_germline = .data$gds_indir_dragen_germline2, # NA for old runs
      gds_infile_genomes_tar = dplyr::if_else(
        is.na(.data$umccrise_genomes_tar1), .data$umccrise_genomes_tar2, .data$umccrise_genomes_tar1
      ),
      ## output second
      gds_outdir_umccrise2 = purrr::map_chr(.data$output, list("output_directory", "location"), .default = NA),
      gds_outdir_umccrise1 = purrr::map_chr(.data$output, list("umccrise_output_directory", "location"), .default = NA),
      gds_outdir_umccrise = dplyr::if_else(
        is.na(.data$gds_outdir_umccrise1), .data$gds_outdir_umccrise2, .data$gds_outdir_umccrise1
      ),
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      -dplyr::any_of(c("sequence_run", "batch_run")), # NA for umccrise
      "year", "durationMin",
      "SubjectID",
      "LibraryID_tumor",
      "LibraryID_normal",
      "SampleID_normal",
      "SampleID_tumor",
      "gds_outdir_umccrise",
      "gds_indir_dragen_somatic",
      "gds_indir_dragen_germline",
      "gds_infile_genomes_tar"
    )
}

#' Metadata for wgs_alignment_qc workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_wgs_alignment_qc(pmeta))
#' @testexamples
#' expect_equal("lane" %in% colnames(m), TRUE)
#' @export
meta_wgs_alignment_qc <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "wgs_alignment_qc"
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
      rgid = purrr::map_chr(.data$input, list("fastq_list_rows", "rgid")),
      rgsm = purrr::map_chr(.data$input, list("fastq_list_rows", "rgsm")),
      rglb = purrr::map_chr(.data$input, list("fastq_list_rows", "rglb")),
      lane = purrr::map_int(.data$input, list("fastq_list_rows", "lane")),
      lane = as.character(.data$lane),
      # read_1/read_2 are dfs
      fq1 = purrr::map_chr(.data$input, list("fastq_list_rows", "read_1", "location"), .default = NA),
      fq2 = purrr::map_chr(.data$input, list("fastq_list_rows", "read_2", "location"), .default = NA),
      # output
      gds_outdir_dragen = purrr::map_chr(.data$output, list("dragen_alignment_output_directory", "location"), .default = NA),
      gds_outdir_multiqc = purrr::map_chr(.data$output, list("multiqc_output_directory", "location"), .default = NA),
      SubjectID = sub("umccr__.*__wgs_alignment_qc__(SBJ.*)__L.*", "\\1", .data$wfr_name),
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    ) |>
    tidyr::separate_wider_delim(
      cols = "rgid", delim = ".",
      names = c("index1", "index2", "lane2", "illumina_id", "sample_lib_id")
    )

  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "year", "durationMin",
      "SubjectID",
      LibraryID = "rglb",
      SampleID = "rgsm",
      "lane",
      "index1",
      "index2",
      "illumina_id",
      "fq1",
      "fq2",
      "gds_outdir_dragen",
      "gds_outdir_multiqc",
    )
}

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
  extract_fq_reads <- function(x, fq) {
    fq_df <- x[[fq]]
    if (is.null(fq_df)) {
      return(NA)
    }
    read1 <- fq_df[["read_1"]][["location"]]
    read2 <- fq_df[["read_2"]][["location"]]
    assertthat::assert_that(length(read1) == length(read2))
    tibble::tibble(read1 = read1, read2 = read2)
  }
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
      fastq_normal = purrr::map(.data$input, extract_fq_reads, "fastq_list_rows"),
      fastq_tumor = purrr::map(.data$input, extract_fq_reads, "tumor_fastq_list_rows"),
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
      SubjectID = sub("umccr__automated__wgs_tumor_normal__(SBJ.....)__L.*", "\\1", .data$wfr_name),
      SubjectID = ifelse(
        !grepl("external_apgi", .data$wfr_name),
        .data$SubjectID,
        sub("umccr__external_apgi__wgs_tumor_normal__(.*)", "\\1", .data$wfr_name)
      ),
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      -dplyr::any_of(c("sequence_run", "batch_run")), # NA for wgs_tumor_normal
      "year", "durationMin",
      "SubjectID",
      "LibraryID_tumor",
      "LibraryID_normal",
      "SampleID_tumor",
      "SampleID_normal",
      "fastq_tumor",
      "fastq_normal",
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

#' Metadata for wts_alignment_qc workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_wts_alignment_qc(pmeta))
#' @testexamples
#' expect_equal("lane" %in% colnames(m), TRUE)
#' @export
meta_wts_alignment_qc <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "wts_alignment_qc"
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
      rgid = purrr::map_chr(.data$input, list("fastq_list_rows", "rgid")),
      rgsm = purrr::map_chr(.data$input, list("fastq_list_rows", "rgsm")),
      rglb = purrr::map_chr(.data$input, list("fastq_list_rows", "rglb")),
      lane = purrr::map_int(.data$input, list("fastq_list_rows", "lane")),
      lane = as.character(.data$lane),
      # read_1/read_2 are dfs
      fq1 = purrr::map_chr(.data$input, list("fastq_list_rows", "read_1", "location"), .default = NA),
      fq2 = purrr::map_chr(.data$input, list("fastq_list_rows", "read_2", "location"), .default = NA),
      # output
      gds_outdir_dragen = purrr::map_chr(.data$output, list("dragen_alignment_output_directory", "location"), .default = NA),
      gds_outdir_multiqc = purrr::map_chr(.data$output, list("multiqc_output_directory", "location"), .default = NA),
      SubjectID = sub("umccr__.*__wts_alignment_qc__(SBJ.*)__L.*", "\\1", .data$wfr_name),
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    ) |>
    tidyr::separate_wider_delim(
      cols = "rgid", delim = ".",
      names = c("index1", "index2", "lane2", "illumina_id", "sample_lib_id")
    )

  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "year", "durationMin",
      "SubjectID",
      LibraryID = "rglb",
      SampleID = "rgsm",
      "lane",
      "index1",
      "index2",
      "illumina_id",
      "fq1",
      "fq2",
      "gds_outdir_dragen",
      "gds_outdir_multiqc",
    )
}

#' Metadata for wts_tumor_only workflow
#'
#' @param pmeta Path to portal workflows metadata table, or tibble with already parsed data.
#' @param status Workflow status to keep (default: Succeeded).
#'
#' @return A tibble with metadata per workflow run.
#' @examples
#' pmeta <- "extdata/portaldb_workflow_top4.rds" |>
#'   system.file(package = "rportal") |>
#'   readr::read_rds()
#' (m <- meta_wts_tumor_only(pmeta))
#' @testexamples
#' expect_equal(length(unique(m$portal_run_id)), 4)
#' expect_equal(length(unique(m$LibraryID)), 4)
#' @export
meta_wts_tumor_only <- function(pmeta, status = "Succeeded") {
  # retrieve workflow runs with the given type and status
  type <- "wts_tumor_only"
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
      rglb = purrr::map_chr(.data$input, \(x) unique(x[["fastq_list_rows"]][["rglb"]]) %||% NA),
      rgsm = purrr::map_chr(.data$input, \(x) unique(x[["fastq_list_rows"]][["rgsm"]]) %||% NA),
      lane = purrr::map_chr(.data$input, \(x) paste(x[["fastq_list_rows"]][["lane"]], collapse = ",")),
      lane = as.character(.data$lane),
      # output
      gds_outdir_dragen = purrr::map_chr(.data$output, list("dragen_transcriptome_output_directory", "location"), .default = NA),
      gds_outdir_multiqc = purrr::map_chr(.data$output, list("multiqc_output_directory", "location"), .default = NA),
      gds_outdir_arriba = purrr::map_chr(.data$output, list("arriba_output_directory", "location"), .default = NA),
      gds_outdir_qualimap = purrr::map_chr(.data$output, list("qualimap_output_directory", "location"), .default = NA),
      SubjectID = sub("umccr__.*__wts_tumor_only__(SBJ.*)__L.*", "\\1", .data$wfr_name),
      SubjectID = ifelse(
        !grepl("external_apgi", .data$wfr_name),
        .data$SubjectID,
        sub("umccr__external_apgi__wts_tumor_only__(.*)", "\\1", .data$wfr_name)
      ),
      # other
      year = as.character(lubridate::year(.data$start)),
      durationMin = round(as.numeric(difftime(.data$end, .data$start, units = "mins")))
    )
  d |>
    dplyr::select(
      dplyr::all_of(meta_main_cols()),
      "year", "durationMin",
      "SubjectID",
      LibraryID = "rglb",
      SampleID = "rgsm",
      Lane = "lane",
      "gds_outdir_dragen",
      "gds_outdir_multiqc",
      "gds_outdir_arriba",
      "gds_outdir_qualimap"
    )
}
