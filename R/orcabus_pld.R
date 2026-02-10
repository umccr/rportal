#' Payload Tidy bclconvert
#'
#' @param pld List with bclconvert workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_bclconvert <- function(pld) {
  assertthat::assert_that(
    all(c("orcabusId", "payloadRefId", "version", "data") %in% names(pld))
  )
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  pdata[["basespaceRunId"]] <- as.character(pdata[["basespaceRunId"]])
  # base64enc::base64decode(pdata$samplesheetB64gz) |>
  #   memDecompress("gzip", TRUE)
  d <- pdata |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id) |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy bclconvertinteropqc
#'
#' @param pld List with bclconvertinteropqc workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_bclconvertinteropqc <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  inputs <- pdata[["inputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy bsshfastqcopy
#'
#' @param pld List with bsshfastqcopy workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_bsshfastqcopy <- function(pld) {
  assertthat::assert_that(
    all(c("orcabusId", "payloadRefId", "version", "data") %in% names(pld))
  )
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  inputs <- pdata[["inputs"]] |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- inputs |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy tso
#'
#' @param pld List with cttsov2 workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_cttsov2 <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqListRowIds into single string
  pdata[["tags"]][["fastqListRowIds"]] <- pdata[["tags"]][["fastqListRowIds"]] |>
    paste(collapse = ", ")
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # ignore verbose inputs$samplesheet
  inputs <- pdata[["inputs"]]
  inputs[["samplesheet"]] <- NULL
  inputs[["fastqListRows"]] <- inputs[["fastqListRows"]] |>
    purrr::map(tibble::as_tibble_row) |>
    dplyr::bind_rows()
  inputs[["fastqListRows"]] <- list(inputs[["fastqListRows"]])
  inputs <- inputs |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy oncoanalyser-wgts-dna
#'
#' @param pld List with oncoanalyserwgtsdna workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_oawgtsdna <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqListRowIds into single string
  pdata[["tags"]][["fastqRgidList"]] <- pdata[["tags"]][["fastqRgidList"]] |>
    paste(collapse = ", ")
  if (!is.null(pdata[["tags"]][["tumorFastqRgidList"]])) {
    pdata[["tags"]][["tumorFastqRgidList"]] <- pdata[["tags"]][["tumorFastqRgidList"]] |>
      paste(collapse = ", ")
  }
  tags <- pdata[["tags"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  inputs <- pdata[["inputs"]]
  # handle genomes list
  inputs[["genomes"]] <- NULL
  inputs <- inputs |>
    # remove trailing slashes from S3 directories
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy oncoanalyser-wgts-dna-rna
#'
#' @param pld List with oncoanalyserwgtsdnarna workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_oawgtsdnarna <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # remove trailing slashes from S3 directories
  inputs <- pdata[["inputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy oncoanalyser-wgts-rna
#'
#' @param pld List with oncoanalyserwgtsrna workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_oawgtsrna <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # remove trailing slashes from S3 directories
  # take care of fastqListRows lists
  inputs <- pdata[["inputs"]]
  inputs[["tumorRnaFastqListRows"]] <- inputs[["tumorRnaFastqListRows"]] |>
    purrr::map(tibble::as_tibble_row) |>
    dplyr::bind_rows() |>
    list()
  inputs <- inputs |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy orca-compression
#'
#' @param pld List with orcacompression workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_oracompression <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # remove trailing slashes from S3 directories
  inputs <- pdata[["inputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy pieriandx
#'
#' @param pld List with pieriandx workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_pieriandx <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # just grab dataFiles + caseMetadata
  inputs <- pdata[["inputs"]]
  inputs_df <- inputs[["dataFiles"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  inputs_cm <- inputs[["caseMetadata"]] |>
    purrr::list_flatten() |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs_cm, by = "orcabusId") |>
    dplyr::left_join(inputs_df, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy rnasum
#'
#' @param pld List with rnasum workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_rnasum <- function(pld) {
  payload_okay(pld)
  id <- pld[["orcabusId"]]
  pdata <- pld[["data"]]
  pdata[["tags"]][["wgsTumorFastqListRowIds"]] <- pdata[["tags"]][["wgsTumorFastqListRowIds"]] |>
    paste(collapse = ", ")
  pdata[["tags"]][["wtsTumorFastqListRowIds"]] <- pdata[["tags"]][["wtsTumorFastqListRowIds"]] |>
    paste(collapse = ", ")
  pdata[["tags"]][["wgsNormalFastqListRowIds"]] <- pdata[["tags"]][["wgsNormalFastqListRowIds"]] |>
    paste(collapse = ", ")
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # remove trailing slashes from S3 directories
  inputs <- pdata[["inputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy sash
#'
#' @param pld List with sash workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_sash <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqRgidList into single string
  pdata[["tags"]][["fastqRgidList"]] <- pdata[["tags"]][["fastqRgidList"]] |>
    paste(collapse = ", ")
  if (!is.null(pdata[["tags"]][["tumorFastqRgidList"]])) {
    pdata[["tags"]][["tumorFastqRgidList"]] <- pdata[["tags"]][["tumorFastqRgidList"]] |>
      paste(collapse = ", ")
  }
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # remove trailing slashes from S3 directories
  inputs <- pdata[["inputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy tumor_normal
#'
#' @param pld List with tumornormal workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_tumornormal <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqListRowIds into single string
  pdata[["tags"]][["tumorFastqListRowIds"]] <- pdata[["tags"]][["tumorFastqListRowIds"]] |>
    paste(collapse = ", ")
  pdata[["tags"]][["normalFastqListRowIds"]] <- pdata[["tags"]][["normalFastqListRowIds"]] |>
    paste(collapse = ", ")
  tags <- pdata[["tags"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # remove trailing slashes from S3 directories
  # take care of fastqListRows lists
  inputs <- pdata[["inputs"]]
  inputs[["tumorFastqListRows"]] <- inputs[["tumorFastqListRows"]] |>
    purrr::map(tibble::as_tibble_row) |>
    dplyr::bind_rows()
  inputs[["fastqListRows"]] <- inputs[["fastqListRows"]] |>
    purrr::map(tibble::as_tibble_row) |>
    dplyr::bind_rows()
  inputs[["fastqListRowsTN"]] <- dplyr::bind_rows(
    list(
      normal = inputs[["fastqListRows"]],
      tumor = inputs[["tumorFastqListRows"]]
    ),
    .id = "phenotype"
  ) |>
    list()
  inputs[["fastqListRows"]] <- NULL
  inputs[["tumorFastqListRows"]] <- NULL
  inputs <- inputs |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy umccrise
#'
#' @param pld List with umccrise workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_umccrise <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqListRowIds into single string
  pdata[["tags"]][["tumorFastqListRowIds"]] <- pdata[["tags"]][["tumorFastqListRowIds"]] |>
    paste(collapse = ", ")
  pdata[["tags"]][["normalFastqListRowIds"]] <- pdata[["tags"]][["normalFastqListRowIds"]] |>
    paste(collapse = ", ")
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # remove trailing slashes from S3 directories
  inputs <- pdata[["inputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy wgtsqc
#'
#' @param pld List with wgtsqc workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_wgtsqc <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # take care of fastqListRows lists
  inputs <- pdata[["inputs"]]
  assertthat::assert_that(
    all(purrr::map_int(inputs[["fastqListRow"]], length) == 1),
    msg = "Input fastqListRow is supposed to have 1 value per element."
  )
  inputs[["fastqListRow"]] <- inputs[["fastqListRow"]] |>
    tibble::as_tibble_row() |>
    list()
  inputs <- inputs |>
    tibble::as_tibble_row() |>
    tidyr::unnest("fastqListRow") |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(
      orcabusId = id,
      input_lane = as.character(.data$input_lane)
    )
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy wts
#'
#' @param pld List with wts workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_wts <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqListRowIds into single string
  pdata[["tags"]][["tumorFastqListRowIds"]] <- pdata[["tags"]][["tumorFastqListRowIds"]] |>
    paste(collapse = ", ")
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # take care of fastqListRows lists
  inputs <- pdata[["inputs"]]
  inputs[["tumorFastqListRows"]] <- inputs[["tumorFastqListRows"]] |>
    purrr::map(tibble::as_tibble_row) |>
    dplyr::bind_rows() |>
    list()
  inputs <- inputs |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy dragenwgtsdna
#'
#' @param pld List with dragenwgtsdna workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_dragenwgtsdna <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqRgidList into single string
  pdata[["tags"]][["fastqRgidList"]] <- pdata[["tags"]][["fastqRgidList"]] |>
    paste(collapse = ", ")
  if (!is.null(pdata[["tags"]][["tumorFastqRgidList"]])) {
    pdata[["tags"]][["tumorFastqRgidList"]] <- pdata[["tags"]][["tumorFastqRgidList"]] |>
      paste(collapse = ", ")
  }
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  inputs <- pdata[["inputs"]]
  inputs[["reference"]] <- NULL
  # take care of fastqListRows lists
  inputs[["sequenceData"]][["fastqListRows"]] <-
    inputs[["sequenceData"]][["fastqListRows"]] |>
    purrr::map(tibble::as_tibble_row) |>
    dplyr::bind_rows() |>
    list()
  inputs[["fastqListRows"]] <- inputs[["sequenceData"]][["fastqListRows"]]
  if (!is.null(inputs[["tumorSequenceData"]][["fastqListRows"]])) {
    inputs[["tumorSequenceData"]][["fastqListRows"]] <-
      inputs[["tumorSequenceData"]][["fastqListRows"]] |>
      purrr::map(tibble::as_tibble_row) |>
      dplyr::bind_rows() |>
      list()
    inputs[["tumorFastqListRows"]] <- inputs[["tumorSequenceData"]][["fastqListRows"]]
  }
  inputs[["sequenceData"]] <- NULL
  inputs[["tumorSequenceData"]] <- NULL
  inputs[["somaticReference"]] <- NULL
  inputs[["somaticMsiOptions"]] <- NULL
  inputs[["somaticTmbOptions"]] <- NULL
  inputs[["targetedCallerOptions"]] <- NULL
  inputs[["alignmentOptions"]] <- NULL
  inputs[["somaticSvCallerOptions"]] <- NULL
  inputs[["snvVariantCallerOptions"]] <- NULL
  inputs[["somaticCnvCallerOptions"]] <- NULL
  inputs[["somaticNirvanaAnnotationOptions"]] <- NULL

  inputs <- inputs |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}

#' Payload Tidy dragenwgtsrna
#'
#' @param pld List with dragenwgtsrna workflow parameters.
#'
#' @return A tidy tibble.
#' @export
pld_dragenwgtsrna <- function(pld) {
  payload_okay(pld)
  pdata <- pld[["data"]]
  id <- pld[["orcabusId"]]
  # collapse FastqRgidList into single string
  pdata[["tags"]][["fastqRgidList"]] <- pdata[["tags"]][["fastqRgidList"]] |>
    paste(collapse = ", ")
  tags <- pdata[["tags"]] |>
    tibble::as_tibble_row() |>
    dplyr::mutate(orcabusId = id)
  # just grab only necessary elements, ignore rest
  inputs <- pdata[["inputs"]][c("sequenceData", "sampleName")]
  inputs[["sequenceData"]][["fastqListRows"]] <-
    inputs[["sequenceData"]][["fastqListRows"]] |>
    purrr::map(tibble::as_tibble_row) |>
    dplyr::bind_rows() |>
    list()
  inputs[["fastqListRows"]] <- inputs[["sequenceData"]][["fastqListRows"]]
  inputs[["sequenceData"]] <- NULL
  inputs <- inputs |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("input_{x}")) |>
    dplyr::mutate(orcabusId = id)
  outputs <- pdata[["outputs"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("output_{x}")) |>
    dplyr::mutate(orcabusId = id)
  engpar <- pdata[["engineParameters"]] |>
    purrr::map(\(x) x |> stringr::str_replace("/$", "")) |>
    tibble::as_tibble_row() |>
    rlang::set_names(\(x) glue("engparam_{x}")) |>
    dplyr::mutate(orcabusId = id)
  d <- tags |>
    dplyr::left_join(inputs, by = "orcabusId") |>
    dplyr::left_join(outputs, by = "orcabusId") |>
    dplyr::left_join(engpar, by = "orcabusId") |>
    dplyr::relocate("orcabusId")
  return(d)
}
