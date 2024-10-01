#' OrcaBus JWT Retrieve
#'
#' Retrieve OrcaBus JWT from Secrets Manager.
#'
#' @return JWT string.
#' @export
orca_jwt <- function() {
  sm_client <- paws::secretsmanager()
  resp <- sm_client$get_secret_value(SecretId = "orcabus/token-service-jwt")
  resp[["SecretString"]] |>
    jsonlite::fromJSON() |>
    purrr::pluck("id_token")
}
