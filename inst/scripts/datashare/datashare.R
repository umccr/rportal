#!/usr/bin/env Rscript

suppressMessages(library(optparse, include.only = "make_option"))
option_list <- list(
  optparse::make_option("--subject_id", type = "character", help = "Subject ID."),
  optparse::make_option("--library_id_tumor", type = "character", help = "Library ID of tumor."),
  optparse::make_option("--wts_wfrn_prefix", type = "character", help = "ICA Workflow Run Name prefix. Use if other than the default 'umccr__automated__wts_tumor_only'."),
  optparse::make_option("--wts", action = "store_true", type = "character", help = "This is a WTS library."),
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
#   csv_output = "FOO.csv",
#   append = TRUE,
#   wts_wfrn_prefix = "umccr__automated__wts_tumor_only"
# )

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
for (flag in c("subject_id", "library_id_tumor", "csv_output")) {
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

SubjectID <- opt[["subject_id"]]
LibraryID_tumor <- opt[["library_id_tumor"]]
csv_output <- opt[["csv_output"]]
csv_append <- opt[["append"]]
wts <- opt[["wts"]]
wts_wfrn_prefix <- opt[["wts_wfrn_prefix"]]
fs::dir_create(dirname(csv_output))
cli::cli_alert_info("Start datasharing for {SubjectID}__{LibraryID_tumor}")

# make sure you have logged into AWS and ICA
c("AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY", "AWS_REGION", "ICA_ACCESS_TOKEN") |>
  rportal::envvar_defined() |>
  stopifnot()
token_ica <- Sys.getenv("ICA_ACCESS_TOKEN") |> dracarys::ica_token_validate()

if (wts) {
  urls <- rportal::datashare_wts(sid = SubjectID, lid = LibraryID_tumor, wfrn_prefix = wts_wfrn_prefix, token_ica = token_ica)
} else {
  urls <- rportal::datashare_um(sid = SubjectID, lid = LibraryID_tumor, token_ica = token_ica)
}
cli::cli_alert_success("Writing {nrow(urls)} URL entries to {csv_output}")
urls |>
  readr::write_csv(csv_output, append = csv_append)
cli::cli_alert_success("Completed datasharing for {SubjectID}__{LibraryID_tumor}")
