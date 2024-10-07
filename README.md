
- [🔮 rportal - Interacting with the UMCCR Data
  Portal](#crystal_ball-rportal---interacting-with-the-umccr-data-portal)
  - [🍕 Installation](#pizza-installation)
  - [🌀 CLI](#cyclone-cli)
    - [Bioinformatics Data Sharing](#bioinformatics-data-sharing)
  - [🚕 Running](#taxi-running)

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![R-CMD-check](https://github.com/umccr/rportal/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/umccr/rportal/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# 🔮 rportal - Interacting with the UMCCR Data Portal

## 🍕 Installation

``` r
devtools::install_github("umccr/rportal")
# remotes::install_github("umccr/rportal")
```

## 🌀 CLI

{rportal} currently has one convenience wrapper for generating presigned
URLs for bioinformatics data (FASTQs, BAMs, VCFs and HTMLs/TSVs):

### Bioinformatics Data Sharing

A `datashare.R` command line interface is available for convenience. You
need to export the `rportal/inst/scripts/datashare/` directory to your
`PATH` in order to use `datashare.R`:

``` bash
datashare_cli=$(Rscript -e 'x = system.file("scripts/datashare", package = "rportal"); cat(x, "\n")' | xargs)
export PATH="${datashare_cli}:${PATH}"
```

    datashare.R --version
    0.1.2 

    #-----------------------------------#
    datashare.R --help
    Usage
    =====
     
    /Library/Frameworks/R.framework/Versions/4.4-arm64/Resources/library/rportal/scripts/datashare/datashare.R [options]


    Options
    =======
    --subject_id=SUBJECT_ID
            Subject ID.

    --library_id_tumor=LIBRARY_ID_TUMOR
            Library ID of tumor.

    --wts
            This is a WTS library.

    --csv_output=CSV_OUTPUT
            CSV output path.

    --append
            Append to existing file (or write to new one if file does not exist -- caution: no column headers are written).

    --version, -v
            Print rportal version and exit.

    --help, -h
            Show this help message and exit

## 🚕 Running

``` bash
datashare.R \
  --subject_id SBJ0XXXX \
  --library_id_tumor L230XXXX \
  --wts \
  --csv_output urls_SBJ0XXXX_L230XXXX.csv
```
