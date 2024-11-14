#' Datashare umccrise S3 Results
#'
#' @param libid LibraryID of WGS tumor.
#' @param token JWT.
#' @param wf_page_size Number of results to return when listing workflows this
#' libid is involved in.
#'
#' @return Tibble with presigned URLs.
#' @examples
#' \dontrun{
#' libid <- "L2401591"
#' libid <- "L2401596"
#' token <- orca_jwt() |> jwt_validate()
#' datashare_um_s3(libid, token)
#' }
#' @export
datashare_um_s3 <- function(libid, token, wf_page_size = 50) {
  umccrise_pld <- function(libid) {
    # get workflows run for libid
    wf <- orca_libid2workflows(
      libid = libid, token = token, wf_name = NULL, page_size = wf_page_size, stage = "prod"
    )
    wf_um_raw <- wf |>
      dplyr::filter(.data$wf_name == "umccrise", .data$currentStateStatus == "SUCCEEDED")
    n_um_runs <- nrow(wf_um_raw)
    if (n_um_runs == 0) {
      cli::cli_abort("No umccrise results found for {libid}")
    } else if (n_um_runs > 1) {
      wf_um_raw <- wf_um_raw |> dplyr::slice_head(n = 1)
      msg <- glue(
        "There are {n_um_runs} > 1 umccrise workflows run for ",
        "{libid};\n",
        "We use the latest run with portal_run_id=\"{wf_um_raw$portalRunId}\" ",
        "with a timestamp of ",
        "{wf_um_raw$currentStateTimestamp}."
      )
      cli::cli_alert_info(msg)
    }
    # now use wfrid to get the payload with umccrise io
    wf_um_raw$orcabusId |>
      orca_wfrid2payload(token = token) |>
      pld_umccrise()
  }
  umccrise_files <- dplyr::tribble(
    ~regex, ~fun,
    "multiqc_report\\.html$", "HTML_MultiQC",
    "somatic\\.pcgr\\.html$", "HTML_PCGR",
    "normal\\.cpsr\\.html$", "HTML_CPSR",
    "cancer_report\\.html$", "HTML_CanRep",
    "germline\\.predispose_genes\\.vcf\\.gz$", "VCF_Germline",
    "germline\\.predispose_genes\\.vcf\\.gz\\.tbi$", "VCFi_Germline",
    "somatic-PASS\\.vcf\\.gz$", "VCF_Somatic",
    "somatic-PASS\\.vcf\\.gz\\.tbi$", "VCFi_Somatic",
    "somatic\\.pcgr\\.snvs_indels\\.tiers\\.tsv$", "TSV_SmallVariantsSomatic",
    "manta\\.tsv$", "TSV_StructuralVariantsManta",
    "manta\\.vcf\\.gz$", "VCF_StructuralVariantsManta",
    "manta\\.vcf\\.gz.tbi$", "VCFi_StructuralVariantsManta",
    "purple\\.cnv\\.gene\\.tsv$", "TSV_CopyNumberVariantsPurpleGene",
    "purple\\.cnv\\.somatic\\.tsv$", "TSV_CopyNumberVariantsPurpleSegments"
  )
  tn_files <- dplyr::tribble(
    ~regex, ~fun,
    "tumor\\.bam$", "BAM_tumor",
    "tumor\\.bam\\.bai$", "BAMi_tumor",
    "tumor\\.bam\\.md5sum$", "BAMmd5sum_tumor",
    "normal\\.bam$", "BAM_normal",
    "normal\\.bam\\.bai$", "BAMi_normal",
    "normal\\.bam\\.md5sum$", "BAMmd5sum_normal",
  )
  p <- umccrise_pld(libid)
  um_dragen_input_s3 <- p[["input_dragenSomaticOutputUri"]]
  um_final_s3 <- p[["output_outputDirectoryUri"]]
  um_work_s3 <- file.path(dirname(um_final_s3), "work", basename(um_final_s3))
  assertthat::assert_that(all(
    grepl("^s3://", c(um_dragen_input_s3, um_final_s3, um_work_s3))
  ))
  libId_tumor <- libid
  libId_normal <- p[["normalLibraryId"]]
  subjectId <- p[["input_subjectId"]]
  amber_dir <- file.path(um_work_s3, "purple/amber")
  cobalt_dir <- file.path(um_work_s3, "purple/cobalt")
  sigs_dir <- file.path(um_final_s3, "cancer_report_tables/sigs")
  expiry <- 604800 # 7days * 24hrs * 60min * 60sec
  d_um_urls1 <- um_final_s3 |>
    dracarys::s3_list_files_filter_relevant(
      presign = TRUE, regexes = umccrise_files, max_objects = 1000, expiry_sec = expiry
    )
  d_um_urls_sigs <- sigs_dir |>
    dracarys::s3_list_files_filter_relevant(
      presign = TRUE, regexes = tibble::tibble(regex = ".*tsv\\.gz$", fun = "foo"),
      max_objects = 100, expiry_sec = expiry
    ) |>
    dplyr::mutate(type = "Signatures") |>
    dplyr::select("type", "bname", "size", "path", "presigned_url")
  d_um_urls_amber <- amber_dir |>
    dracarys::s3_list_files_filter_relevant(
      presign = TRUE, regexes = tibble::tibble(regex = "amber", fun = "foo"),
      max_objects = 100, expiry_sec = expiry
    ) |>
    dplyr::mutate(type = "AMBER") |>
    dplyr::select("type", "bname", "size", "path", "presigned_url")
  d_um_urls_cobalt <- cobalt_dir |>
    dracarys::s3_list_files_filter_relevant(
      presign = TRUE, regexes = tibble::tibble(regex = "cobalt", fun = "foo"),
      max_objects = 100, expiry_sec = expiry
    ) |>
    dplyr::mutate(type = "COBALT") |>
    dplyr::select("type", "bname", "size", "path", "presigned_url")
  d_um_urls2 <- um_dragen_input_s3 |>
    dracarys::s3_list_files_filter_relevant(
      presign = TRUE, regexes = tn_files, max_objects = 500, expiry_sec = expiry
    )
  if ((nrow(d_um_urls2) != nrow(tn_files)) | ((nrow(d_um_urls1) != nrow(umccrise_files)))) {
    # files were not found? might also have files with the same pattern matching,
    # though I have not encountered any such cases. Also: BAMs get deleted.
    cli::cli_alert_danger(
      text = c(
        "There was not a 1-1 match between files requested and files found. ",
        "Maybe BAMs have been deleted? ",
        "Contact the UMCCR bioinformatics team."
      )
    )
  }
  urls_all <- dplyr::bind_rows(d_um_urls1, d_um_urls2) |>
    dplyr::arrange(.data$type) |>
    dplyr::bind_rows(d_um_urls_amber, d_um_urls_cobalt, d_um_urls_sigs) |>
    dplyr::mutate(
      libid = libid,
      size = trimws(as.character(.data$size)),
      filesystem = "s3"
    ) |>
    dplyr::relocate("libid") |>
    dplyr::relocate("filesystem", .after = "lastmodified")
  urls_all
}

#' Datashare umccrise Results
#'
#' @param sid SubjectID.
#' @param lid LibraryID of WGS tumor.
#' @param token_ica ICA_ACCESS_TOKEN.
#'
#' @return Tibble with presigned URLs.
#' @examples
#' \dontrun{
#' sid <- "SBJ03144"
#' lid <- "L2301290"
#' datashare_um(sid, lid)
#' }
#' @export
datashare_um <- function(sid, lid, token_ica = Sys.getenv("ICA_ACCESS_TOKEN")) {
  sid_lid <- glue("{sid}__{lid}")
  umccrise_files <- dplyr::tribble(
    ~regex, ~fun,
    "multiqc_report\\.html$", "HTML_MultiQC",
    "somatic\\.pcgr\\.html$", "HTML_PCGR",
    "normal\\.cpsr\\.html$", "HTML_CPSR",
    "cancer_report\\.html$", "HTML_CanRep",
    "germline\\.predispose_genes\\.vcf\\.gz$", "VCF_Germline",
    "germline\\.predispose_genes\\.vcf\\.gz\\.tbi$", "VCFi_Germline",
    "somatic-PASS\\.vcf\\.gz$", "VCF_Somatic",
    "somatic-PASS\\.vcf\\.gz\\.tbi$", "VCFi_Somatic",
    "somatic\\.pcgr\\.snvs_indels\\.tiers\\.tsv$", "TSV_SmallVariantsSomatic",
    "manta\\.tsv$", "TSV_StructuralVariantsManta",
    "manta\\.vcf\\.gz$", "VCF_StructuralVariantsManta",
    "manta\\.vcf\\.gz.tbi$", "VCFi_StructuralVariantsManta",
    "purple\\.cnv\\.gene\\.tsv$", "TSV_CopyNumberVariantsPurpleGene",
    "purple\\.cnv\\.somatic\\.tsv$", "TSV_CopyNumberVariantsPurpleSegments"
  )

  tn_files <- dplyr::tribble(
    ~regex, ~fun,
    "tumor\\.bam$", "BAM_tumor",
    "tumor\\.bam\\.bai$", "BAMi_tumor",
    "normal\\.bam$", "BAM_normal",
    "normal\\.bam\\.bai$", "BAMi_normal",
  )
  query_um <- glue(
    "WHERE \"type_name\" = 'umccrise' AND  \"end_status\" = 'Succeeded' AND ",
    "REGEXP_LIKE(\"wfr_name\", 'umccr__automated__umccrise__{sid_lid}') ",
    "ORDER BY \"start\" DESC;"
  )
  d_um_raw <- portaldb_query_workflow(query_um)
  n_um_runs <- nrow(d_um_raw)
  if (n_um_runs == 0) {
    cli::cli_abort("No umccrise results found for {sid_lid}")
  } else if (n_um_runs > 1) {
    d_um_raw <- d_um_raw |> dplyr::slice_head(n = 1)
    msg <- glue(
      "There are {n_um_runs} > 1 umccrise workflows run for ",
      "{sid_lid};\n",
      "We use the latest run with portal_run_id=\"{d_um_raw$portal_run_id}\" ",
      "which ended at {d_um_raw$end}."
    )
    cli::cli_alert_info(msg)
  }
  d_um_tidy <- meta_umccrise(d_um_raw)
  um_dragen_input <- d_um_tidy[["gds_indir_dragen_somatic"]]
  stopifnot(!is.na(um_dragen_input))

  query_tn <- glue(
    "WHERE \"type_name\" = 'wgs_tumor_normal' AND  \"end_status\" = 'Succeeded' AND ",
    "REGEXP_LIKE(\"wfr_name\", 'umccr__automated__wgs_tumor_normal__{sid_lid}') ",
    "ORDER BY \"start\" DESC;"
  )
  d_tn_raw <- portaldb_query_workflow(query_tn)
  if (nrow(d_tn_raw) == 0) {
    cli::cli_abort("No wgs_tumor_normal results found for {sid_lid}")
  }
  d_tn_tidy <- meta_wgs_tumor_normal(d_tn_raw)
  n_tn_runs <- nrow(d_tn_tidy)
  if (n_tn_runs > 1) {
    if (um_dragen_input %in% d_tn_tidy[["gds_outdir_dragen_somatic"]]) {
      msg <- glue(
        "There are {n_tn_runs} > 1 wgs_tumor_normal workflows run for ",
        "{sid_lid}\n",
        "We use the run which output somatic results into the following location:\n{um_dragen_input}"
      )
      cli::cli_alert_info(msg)
    } else {
      msg <- glue(
        "No wgs_tumor_normal results found for {sid_lid} ",
        "with a gds_outdir_dragen_somatic of {um_dragen_input}"
      )
      cli::cli_abort(msg)
    }
  }
  d_tn_tidy <- d_tn_tidy |>
    dplyr::filter(.data$gds_outdir_dragen_somatic == um_dragen_input)
  stopifnot(nrow(d_tn_tidy) == 1)
  SampleID_tumor <- d_um_tidy[["SampleID_tumor"]]
  LibraryID_normal <- d_um_tidy[["LibraryID_normal"]]
  sbjid_sampid_dir <- glue("{sid}__{SampleID_tumor}")
  umccrise_dir <- file.path(d_um_tidy[["gds_outdir_umccrise"]], sbjid_sampid_dir)
  umccrise_work_dir <- file.path(d_um_tidy[["gds_outdir_umccrise"]], "work", sbjid_sampid_dir)
  amber_dir <- file.path(umccrise_work_dir, "purple/amber")
  cobalt_dir <- file.path(umccrise_work_dir, "purple/cobalt")
  sigs_dir <- file.path(umccrise_dir, "cancer_report_tables/sigs")
  d_um_urls1 <- umccrise_dir |>
    dracarys::gds_list_files_filter_relevant(
      token = token_ica, include_url = TRUE, page_size = 500, regexes = umccrise_files
    )
  d_um_urls_sigs <- sigs_dir |>
    dracarys::gds_list_files_dir(token = token_ica, include_url = TRUE, page_size = 100) |>
    dplyr::mutate(type = "Signatures") |>
    dplyr::select("type", "bname", "size", "file_id", "path", "presigned_url")
  d_um_urls_amber <- amber_dir |>
    dracarys::gds_list_files_dir(token = token_ica, include_url = TRUE, page_size = 100) |>
    dplyr::mutate(type = "AMBER") |>
    dplyr::select("type", "bname", "size", "file_id", "path", "presigned_url")
  d_um_urls_cobalt <- cobalt_dir |>
    dracarys::gds_list_files_dir(token = token_ica, include_url = TRUE, page_size = 100) |>
    dplyr::mutate(type = "COBALT") |>
    dplyr::select("type", "bname", "size", "file_id", "path", "presigned_url")
  d_um_urls2 <- d_um_tidy[["gds_indir_dragen_somatic"]] |>
    dracarys::gds_list_files_filter_relevant(
      token = token_ica, include_url = TRUE, page_size = 500, regexes = tn_files
    )
  fq_list <- d_tn_tidy |>
    dplyr::select("fastq_tumor", "fastq_normal") |>
    tidyr::pivot_longer(cols = c("fastq_tumor", "fastq_normal"), names_to = "fastq_tn") |>
    tidyr::unnest("value") |>
    dplyr::mutate(id = dplyr::row_number(), .by = "fastq_tn") |>
    tidyr::pivot_longer(cols = c("read1", "read2"), names_to = "read", values_to = "path") |>
    dplyr::mutate(
      fastq_id = glue("{.data$fastq_tn}_{.data$id}_{.data$read}"),
      fastq_dir = dirname(.data$path),
      path = basename(.data$path)
    ) |>
    dplyr::select(fun = "fastq_id", regex = "path", "fastq_dir") |>
    base::split(~fastq_dir)
  fq_urls <- NULL
  for (outdir in names(fq_list)) {
    fq_urls_tmp <- dracarys::gds_list_files_filter_relevant(
      gdsdir = outdir, token = token_ica,
      include_url = TRUE, page_size = 500, regexes = fq_list[[outdir]]
    )
    fq_urls <- dplyr::bind_rows(fq_urls, fq_urls_tmp)
  }
  if (nrow(fq_urls) == 0) {
    cli::cli_alert_danger(
      "No FASTQs were found for {sid_lid}"
    )
  }
  fq_urls <- fq_urls |>
    dplyr::mutate(type = sub("fastq", "FASTQ", .data$type))
  if ((nrow(d_um_urls2) != nrow(tn_files)) | ((nrow(d_um_urls1) != nrow(umccrise_files)))) {
    # files were not found? might also have files with the same pattern matching,
    # though I have not encountered any such cases. Also: BAMs get deleted.
    cli::cli_alert_danger(
      text = c(
        "There was not a 1-1 match between files requested and files found. ",
        "Maybe BAMs have been deleted? ",
        "Contact the UMCCR bioinformatics team."
      )
    )
  }
  urls_all <- dplyr::bind_rows(d_um_urls1, d_um_urls2, fq_urls) |>
    dplyr::arrange(.data$type) |>
    dplyr::bind_rows(d_um_urls_amber, d_um_urls_cobalt, d_um_urls_sigs) |>
    dplyr::mutate(
      sbjid_libid = glue("{sid_lid}"),
      path = sub("gds://", "", .data$path),
      size = trimws(as.character(.data$size))
    ) |>
    dplyr::relocate("sbjid_libid")
  urls_all
}

#' Datashare WTS Results
#'
#' @param sid SubjectID.
#' @param lid LibraryID of WTS tumor.
#' @param token_ica ICA_ACCESS_TOKEN.
#' @param wfrn_prefix ICA workflow run name prefix. Specify if you need something
#' other than the default 'umccr__automated__wts_tumor_only'.
#'
#' @return Tibble with presigned URLs.
#' @examples
#' \dontrun{
#' datashare_wts(sid = "SBJ05560", lid = "L2401254")
#' datashare_wts(sid = "SBJ05424", lid = "L2401135", wfrn_prefix = "umccr__atlas__wts_tumor_only")
#' }
#'
#' @export
datashare_wts <- function(sid, lid, wfrn_prefix = "umccr__automated__wts_tumor_only",
                          token_ica = Sys.getenv("ICA_ACCESS_TOKEN")) {
  sid_lid <- glue("{sid}__{lid}")
  wts_files <- dplyr::tribble(
    ~regex, ~fun,
    "\\.bam$", "BAM_WTS_tumor",
    "\\.bam\\.bai$", "BAMi_WTS_tumor",
    "fusion_candidates\\.final$", "TSV_WTS_FusionCandidatesDragen",
    "quant\\.genes\\.sf$", "TSV_WTS_QuantificationGenes",
    "quant\\.sf", "TSV_WTS_Quantification",
  )
  wts_arriba_files <- dplyr::tribble(
    ~regex, ~fun,
    "fusions\\.pdf$", "PDF_WTS_FusionsArriba",
    "fusions\\.tsv$", "TSV_WTS_FusionsArriba",
  )
  query_wts <- glue(
    "WHERE \"type_name\" = 'wts_tumor_only' AND  \"end_status\" = 'Succeeded' AND ",
    "REGEXP_LIKE(\"wfr_name\", '{wfrn_prefix}__{sid_lid}') ",
    "ORDER BY \"start\" DESC;"
  )
  d_wts_raw <- portaldb_query_workflow(query_wts)
  n_wts_runs <- nrow(d_wts_raw)
  if (n_wts_runs == 0) {
    cli::cli_abort("No WTS results found for {sid_lid}")
  } else if (n_wts_runs > 1) {
    d_wts_raw <- d_wts_raw |> dplyr::slice_head(n = 1)
    msg <- glue(
      "There are {n_wts_runs} > 1 WTS workflows run for ",
      "{sid_lid};\n",
      "We use the latest run with portal_run_id=\"{d_wts_raw$portal_run_id}\" ",
      "which ended at {d_wts_raw$end}."
    )
    cli::cli_alert_info(msg)
  }
  d_wts_tidy <- meta_wts_tumor_only(d_wts_raw)
  d_wts_urls1 <- d_wts_tidy[["gds_outdir_dragen"]] |>
    dracarys::gds_list_files_filter_relevant(
      token = token_ica, include_url = TRUE, page_size = 100, regexes = wts_files
    )
  d_wts_urls2 <- d_wts_tidy[["gds_outdir_arriba"]] |>
    dracarys::gds_list_files_filter_relevant(
      token = token_ica, include_url = TRUE, page_size = 10, regexes = wts_arriba_files
    )
  d_wts_urls <- dplyr::bind_rows(d_wts_urls1, d_wts_urls2) |>
    dplyr::arrange(.data$type) |>
    dplyr::mutate(
      sbjid_libid = sid_lid,
      path = sub("gds://", "", .data$path),
      size = trimws(as.character(.data$size))
    ) |>
    dplyr::relocate("sbjid_libid")
  d_wts_urls
}

#' Datashare WTS S3 Results
#'
#' @param libid LibraryID of WTS tumor.
#' @param wf_page_size Number of results to return when listing workflows this
#' libid is involved in.
#' @param token JWT.
#'
#' @return Tibble with presigned URLs.
#' @examples
#' \dontrun{
#' libid <- "L2401585"
#' token <- orca_jwt() |> jwt_validate()
#' datashare_wts_s3(libid, token)
#' }
#' @export
datashare_wts_s3 <- function(libid, token, wf_page_size = 50) {
  wts_pld <- function(libid) {
    # get workflows run for libid
    wf <- orca_libid2workflows(
      libid = libid, token = token, wf_name = NULL, page_size = wf_page_size, stage = "prod"
    )
    wf_raw <- wf |>
      dplyr::filter(.data$wf_name == "wts", .data$currentStateStatus == "SUCCEEDED")
    n_runs <- nrow(wf_raw)
    if (n_runs == 0) {
      cli::cli_abort("No WTS results found for {libid}")
    } else if (n_runs > 1) {
      wf_raw <- wf_raw |> dplyr::slice_head(n = 1)
      msg <- glue(
        "There are {n_runs} > 1 WTS workflows run for ",
        "{libid};\n",
        "We use the latest run with portal_run_id=\"{wf_raw$portalRunId}\" ",
        "with a timestamp of ",
        "{wf_raw$currentStateTimestamp}."
      )
      cli::cli_alert_info(msg)
    }
    # now use wfrid to get the payload with wts io
    p <- wf_raw$orcabusId |>
      orca_wfrid2payload(token = token) |>
      pld_wts()
  }
  wts_files <- dplyr::tribble(
    ~regex, ~fun,
    "\\.bam$", "BAM_WTS_tumor",
    "\\.bam\\.bai$", "BAMi_WTS_tumor",
    "\\.bam\\.md5sum$", "BAMmd5sum_WTS_tumor",
    "fusion_candidates\\.final$", "TSV_WTS_FusionCandidatesDragen",
    "quant\\.genes\\.sf$", "TSV_WTS_QuantificationGenes",
    "quant\\.sf", "TSV_WTS_Quantification",
  )
  wts_arriba_files <- dplyr::tribble(
    ~regex, ~fun,
    "fusions\\.pdf$", "PDF_WTS_FusionsArriba",
    "fusions\\.tsv$", "TSV_WTS_FusionsArriba",
  )
  p <- wts_pld(libid)
  expiry <- 604800 # 7days * 24hrs * 60min * 60sec
  d_wts_urls1 <- p[["output_dragenTranscriptomeOutputUri"]] |>
    dracarys::s3_list_files_filter_relevant(
      presign = TRUE, regexes = wts_files, max_objects = 500, expiry_sec = expiry
    )
  d_wts_urls2 <- p[["output_arribaOutputUri"]] |>
    dracarys::s3_list_files_filter_relevant(
      presign = TRUE, regexes = wts_arriba_files, max_objects = 500, expiry_sec = expiry
    )
  urls_all <- dplyr::bind_rows(d_wts_urls1, d_wts_urls2) |>
    dplyr::arrange(.data$type) |>
    dplyr::mutate(
      libid = libid,
      size = trimws(as.character(.data$size)),
      filesystem = "s3"
    ) |>
    dplyr::relocate("libid") |>
    dplyr::relocate("filesystem", .after = "lastmodified")
  return(urls_all)
}
