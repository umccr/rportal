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

#' Payload for umccrise workflow
#'
#' @param pld List with umccrise workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_umccrise <- function(pld) {
  assertthat::assert_that(
    all(c("orcabusId", "payloadRefId", "version", "data") %in% names(pld))
  )
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  assertthat::assert_that(
    all(c("tags", "inputs", "outputs", "engineParameters") %in% names(pdata))
  )
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  inputs <- pdata[["inputs"]] |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)

  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId")
  return(d)
}
