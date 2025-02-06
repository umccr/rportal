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
