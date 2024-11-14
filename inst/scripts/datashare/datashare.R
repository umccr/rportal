#!/usr/bin/env Rscript

suppressMessages(library(optparse, include.only = "make_option"))
option_list <- list(
  optparse::make_option("--subject_id", type = "character", help = "Subject ID.", default = ""),
  optparse::make_option("--library_id_tumor", type = "character", help = "Library ID of tumor."),
  optparse::make_option("--wts_wfrn_prefix",
    type = "character", help = "ICA Workflow Run Name prefix. Use if other than the default.",
    default = "umccr__automated__wts_tumor_only"
  ),
  optparse::make_option("--wts", action = "store_true", type = "character", help = "This is a WTS library."),
  optparse::make_option("--s3", action = "store_true", type = "character", help = "Results on S3, not GDS."),
  optparse::make_option("--csv_output", type = "character", help = "CSV output path."),
  optparse::make_option("--append", action = "store_true", help = "Append to existing file (or write to new one if file does not exist -- caution: no column headers are written)."),
  optparse::make_option(c("--version", "-v"), action = "store_true", help = "Print rportal version and exit.")
)
parser <- optparse::OptionParser(option_list = option_list, formatter = optparse::TitledHelpFormatter)
opt <- optparse::parse_args(parser)
# opt <- list(
#   subject_id = "SBJ05560",
#   library_id_tumor = "L2401254",
#   wts = TRUE,
#   s3 = TRUE,
#   csv_output = "FOO.csv",
#   append = TRUE,
#   # wts_wfrn_prefix = "umccr__automated__wts_tumor_only"
# )
# print(str(opt))

if (!is.null(opt[["version"]])) {
  cat(as.character(packageVersion("rportal")), "\n")
  quit("no", status = 0, runLast = FALSE)
}

# install following from UMCCR GitHub, rest are from CRAN
# devtools::install_github("umccr/rportal")
# devtools::install_github("umccr/dracarys")
suppressMessages(library(cli, include.only = "cli_alert_info"))
suppressMessages(library(dplyr))
suppressMessages(library(dracarys, include.only = "ica_token_validate"))
suppressMessages(library(fs, include.only = "dir_create"))
suppressMessages(library(glue, include.only = "glue"))
suppressMessages(library(readr, include.only = "write_csv"))
suppressMessages(library(rportal, include.only = "datashare_um"))
suppressMessages(library(tidyr, include.only = c("pivot_longer", "unnest")))

missing_flags <- NULL
for (flag in c("library_id_tumor", "csv_output")) {
  if (is.null(opt[[flag]])) {
    missing_flags <- c(missing_flags, flag)
  }
}
if (length(missing_flags) > 0) {
  tmp <- paste(missing_flags, collapse = ", ")
  cli::cli_abort("Missing required flags: {tmp}")
}

# logical flags
if (is.null(opt[["append"]])) {
  opt[["append"]] <- FALSE
}
if (is.null(opt[["wts"]])) {
  opt[["wts"]] <- FALSE
}
if (is.null(opt[["s3"]])) {
  opt[["s3"]] <- FALSE
}

SubjectID <- opt[["subject_id"]]
LibraryID_tumor <- opt[["library_id_tumor"]]
print_sbjlib <- ifelse(SubjectID == "", glue("{LibraryID_tumor}"), glue("{SubjectID}__{LibraryID_tumor}"))
csv_output <- opt[["csv_output"]]
csv_append <- opt[["append"]]
wts <- opt[["wts"]]
wts_wfrn_prefix <- opt[["wts_wfrn_prefix"]]
s3 <- opt[["s3"]]
fs::dir_create(dirname(csv_output))
cli::cli_alert_info("Start datasharing for {print_sbjlib}")

# make sure you have logged into AWS
c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION") |>
  rportal::envvar_defined() |>
  stopifnot()
token <- NULL
if (s3) {
  token <- rportal::orca_jwt() |> rportal::jwt_validate()
} else {
  rportal::envvar_defined("ICA_ACCESS_TOKEN") |> stopifnot()
  token <- Sys.getenv("ICA_ACCESS_TOKEN") |> dracarys::ica_token_validate()
}

if (s3) {
  cli::cli_alert_info("Results are on S3")
} else {
  cli::cli_alert_info("Results are on GDS")
}

if (s3) {
  if (wts) {
    urls <- rportal::datashare_wts_s3(libid = LibraryID_tumor, token = token)
  } else {
    urls <- rportal::datashare_um_s3(libid = LibraryID_tumor, token = token)
  }
} else {
  if (wts) {
    urls <- rportal::datashare_wts(sid = SubjectID, lid = LibraryID_tumor, wfrn_prefix = wts_wfrn_prefix, token_ica = token)
  } else {
    urls <- rportal::datashare_um(sid = SubjectID, lid = LibraryID_tumor, token_ica = token)
  }
}
cli::cli_alert_success("Writing {nrow(urls)} URL entries to {csv_output}")
urls |>
  readr::write_csv(csv_output, append = csv_append)
cli::cli_alert_success("Completed datasharing for {print_sbjlib}")
