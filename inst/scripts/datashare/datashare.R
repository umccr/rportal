#!/usr/bin/env Rscript

suppressMessages(library(optparse, include.only = "make_option"))
option_list <- list(
  optparse::make_option("--subject_id", type = "character", help = "Subject ID."),
  optparse::make_option("--library_id_tumor", type = "character", help = "Library ID of tumor."),
  optparse::make_option("--csv_output", type = "character", help = "CSV output path."),
  optparse::make_option("--append", action = "store_true", help = "Append to existing file (or write to new one if file does not exist -- caution: no column headers are written)."),
  optparse::make_option(c("--version", "-v"), action = "store_true", help = "Print rportal version and exit.")
)
parser <- optparse::OptionParser(option_list = option_list, formatter = optparse::TitledHelpFormatter)
opt <- optparse::parse_args(parser)

if (!is.null(opt[["version"]])) {
  cat(as.character(packageVersion("rportal")), "\n")
  quit("no", status = 0, runLast = FALSE)
}

# install following from UMCCR GitHub, rest are from CRAN
# devtools::install_github("umccr/rportal")
# devtools::install_github("umccr/dracarys")
suppressMessages(library(cli, include.only = "cli_alert_info"))
suppressMessages(library(dplyr))
suppressMessages(library(dracarys, include.only = "gds_files_list_filter_relevant"))
suppressMessages(library(fs, include.only = "dir_create"))
suppressMessages(library(glue, include.only = "glue"))
suppressMessages(library(readr, include.only = "write_csv"))
suppressMessages(library(rportal, include.only = "meta_umccrise"))
suppressMessages(library(tidyr, include.only = c("pivot_longer", "unnest")))

missing_flags <- NULL
for (flag in c("subject_id", "library_id_tumor", "csv_output")) {
  if (is.null(opt[[flag]])) {
    missing_flags <- c(missing_flags, flag)
  }
}
if (length(missing_flags) > 0) {
  tmp <- paste(missing_flags, collapse = ", ")
  cli::cli_abort("Missing required flags: {tmp}")
}

if (is.null(opt[["append"]])) {
  opt[["append"]] <- FALSE
}

## ---------------------------------------------------------------------------------------------------------------------------------
#| label: params_setup

SubjectID <- opt[["subject_id"]]
LibraryID_tumor <- opt[["library_id_tumor"]]
csv_output <- opt[["csv_output"]]
csv_append <- opt[["append"]]
fs::dir_create(dirname(csv_output))


## ---------------------------------------------------------------------------------------------------------------------------------
#| label: file_regexes

cli::cli_alert_info("Start datasharing for {SubjectID}__{LibraryID_tumor}")
umccrise_files <- tribble(
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

tn_files <- tribble(
  ~regex, ~fun,
  "tumor\\.bam$", "BAM_tumor",
  "tumor\\.bam\\.bai$", "BAMi_tumor",
  "normal\\.bam$", "BAM_normal",
  "normal\\.bam\\.bai$", "BAMi_normal",
)


## ---------------------------------------------------------------------------------------------------------------------------------
#| label: envvar_checker

envvar_undefined <- function() {
  env <- tibble(
    var = c(
      "AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION", "ICA_ACCESS_TOKEN"
    )
  ) |>
    mutate(
      value = Sys.getenv(.data$var),
      defined = nchar(.data$value) > 0,
    ) |>
    filter(!defined) |>
    pull("var")
  env
}
env_und <- envvar_undefined()
if (length(env_und) > 0) {
  e <- paste(env_und, collapse = ", ")
  cli::cli_abort("Following environment variables not defined: {e}")
}


## ---------------------------------------------------------------------------------------------------------------------------------
#| label: portaldb_wf_um
# invisible(capture.output(rportal::awsvault_profile("upro")))
token_ica <- Sys.getenv("ICA_ACCESS_TOKEN") |> dracarys::ica_token_validate()
# select all rows from the workflow table with the following conditions
query_um <- glue(
  "WHERE \"type_name\" = 'umccrise' AND  \"end_status\" = 'Succeeded' AND ",
  "REGEXP_LIKE(\"wfr_name\", 'umccr__automated__umccrise__{SubjectID}__{LibraryID_tumor}') ",
  "ORDER BY \"start\" DESC;"
)
d_um_raw <- rportal::portaldb_query_workflow(query_um)
n_um_runs <- nrow(d_um_raw)
if (n_um_runs == 0) {
  cli::cli_abort("ERROR: No umccrise results found for {SubjectID}__{LibraryID_tumor}")
} else if (n_um_runs > 1) {
  d_um_raw <- d_um_raw |> dplyr::slice_head(n = 1)
  msg <- glue(
    "There are {n_um_runs} > 1 umccrise workflows run for ",
    "{SubjectID}__{LibraryID_tumor};\n",
    "We use the latest run with portal_run_id=\"{d_um_raw$portal_run_id}\" ",
    "which ended at {d_um_raw$end}."
  )
  cli::cli_alert_info(msg)
}
d_um_tidy <- rportal::meta_umccrise(d_um_raw)
um_dragen_input <- d_um_tidy[["gds_indir_dragen_somatic"]]
stopifnot(!is.na(um_dragen_input))


## ---------------------------------------------------------------------------------------------------------------------------------
#| label: portaldb_wf_tn

query_tn <- glue(
  "WHERE \"type_name\" = 'wgs_tumor_normal' AND  \"end_status\" = 'Succeeded' AND ",
  "REGEXP_LIKE(\"wfr_name\", 'umccr__automated__wgs_tumor_normal__{SubjectID}__{LibraryID_tumor}') ",
  "ORDER BY \"start\" DESC;"
)
d_tn_raw <- rportal::portaldb_query_workflow(query_tn)
if (nrow(d_tn_raw) == 0) {
  cli::cli_abort("ERROR: No wgs_tumor_normal results found for {SubjectID}__{LibraryID_tumor}")
}
d_tn_tidy <- rportal::meta_wgs_tumor_normal(d_tn_raw)
n_tn_runs <- nrow(d_tn_tidy)
if (n_tn_runs > 1) {
  if (um_dragen_input %in% d_tn_tidy[["gds_outdir_dragen_somatic"]]) {
    msg <- glue(
      "There are {n_tn_runs} > 1 wgs_tumor_normal workflows run for ",
      "{SubjectID}__{LibraryID_tumor}\n",
      "We use the run which output somatic results into the following location:\n{um_dragen_input}"
    )
    cli::cli_alert_info(msg)
  } else {
    msg <- glue(
      "ERROR: No wgs_tumor_normal results found for {SubjectID}__{LibraryID_tumor} ",
      "with a gds_outdir_dragen_somatic of {um_dragen_input}"
    )
    cli::cli_abort(msg)
  }
}
d_tn_tidy <- d_tn_tidy |>
  dplyr::filter(.data$gds_outdir_dragen_somatic == um_dragen_input)
stopifnot(nrow(d_tn_tidy) == 1)


## ---------------------------------------------------------------------------------------------------------------------------------
#| label: extract_stuff

SampleID_tumor <- d_um_tidy[["SampleID_tumor"]]
LibraryID_normal <- d_um_tidy[["LibraryID_normal"]]
sbjid_sampid_dir <- glue("{SubjectID}__{SampleID_tumor}")
umccrise_dir <- file.path(d_um_tidy[["gds_outdir_umccrise"]], sbjid_sampid_dir)
umccrise_work_dir <- file.path(d_um_tidy[["gds_outdir_umccrise"]], "work", sbjid_sampid_dir)
amber_dir <- file.path(umccrise_work_dir, "purple/amber")
cobalt_dir <- file.path(umccrise_work_dir, "purple/cobalt")
sigs_dir <- file.path(umccrise_dir, "cancer_report_tables/sigs")
d_um_urls1 <- umccrise_dir |>
  dracarys::gds_files_list_filter_relevant(
    token = token_ica, include_url = TRUE, page_size = 500, regexes = umccrise_files
  )
d_um_urls_sigs <- sigs_dir |>
  dracarys::gds_files_list(token = token_ica, include_url = TRUE, page_size = 100) |>
  mutate(type = "Signatures") |>
  select("type", "bname", "size", "file_id", "path", "presigned_url")
d_um_urls_amber <- amber_dir |>
  dracarys::gds_files_list(token = token_ica, include_url = TRUE, page_size = 100) |>
  mutate(type = "AMBER") |>
  select("type", "bname", "size", "file_id", "path", "presigned_url")
d_um_urls_cobalt <- cobalt_dir |>
  dracarys::gds_files_list(token = token_ica, include_url = TRUE, page_size = 100) |>
  mutate(type = "COBALT") |>
  select("type", "bname", "size", "file_id", "path", "presigned_url")
d_um_urls2 <- d_um_tidy[["gds_indir_dragen_somatic"]] |>
  dracarys::gds_files_list_filter_relevant(
    token = token_ica, include_url = TRUE, page_size = 500, regexes = tn_files
  )
fq_list <- d_tn_tidy |>
  select("fastq_tumor", "fastq_normal") |>
  pivot_longer(cols = c("fastq_tumor", "fastq_normal"), names_to = "fastq_tn") |>
  unnest("value") |>
  mutate(id = row_number(), .by = "fastq_tn") |>
  pivot_longer(cols = c("read1", "read2"), names_to = "read", values_to = "path") |>
  mutate(
    fastq_id = glue("{.data$fastq_tn}_{.data$id}_{.data$read}"),
    fastq_dir = dirname(.data$path),
    path = basename(.data$path)
  ) |>
  select(fun = "fastq_id", regex = "path", "fastq_dir") |>
  base::split(~fastq_dir)
fq_urls <- NULL
for (outdir in names(fq_list)) {
  fq_urls_tmp <- dracarys::gds_files_list_filter_relevant(
    gdsdir = outdir, token = token_ica,
    include_url = TRUE, page_size = 500, regexes = fq_list[[outdir]]
  )
  fq_urls <- bind_rows(fq_urls, fq_urls_tmp)
}
if (nrow(fq_urls) == 0) {
  cli::cli_alert_danger(
    "No FASTQs were found for {SubjectID}__{LibraryID_tumor}"
  )
}
fq_urls <- fq_urls |>
  mutate(type = sub("fastq", "FASTQ", .data$type))
if ((nrow(d_um_urls2) != nrow(tn_files)) | ((nrow(d_um_urls1) != nrow(umccrise_files)))) {
  # files were not found? might also have files with the same pattern matching,
  # though I have not encountered any such cases.
  cli::cli_alert_danger(
    "There was not a 1-1 match between files requested and files found. ",
    "Contact the UMCCR bioinformatics team."
  )
}
urls_all <- bind_rows(d_um_urls1, d_um_urls2, fq_urls) |>
  arrange(type) |>
  bind_rows(d_um_urls_amber, d_um_urls_cobalt, d_um_urls_sigs) |>
  mutate(
    sbjid_libid = glue("{SubjectID}__{LibraryID_tumor}"),
    path = sub("gds://", "", .data$path),
    size = trimws(as.character(.data$size))
  ) |>
  relocate("sbjid_libid")

## ---------------------------------------------------------------------------------------------------------------------------------
#| label: results

cli::cli_alert_success("Writing {nrow(urls_all)} URL entries to {csv_output}")
urls_all |>
  readr::write_csv(csv_output, append = csv_append)
cli::cli_alert_success("Completed datasharing for {SubjectID}__{LibraryID_tumor}")
