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

#' JWT Expiration Date
#'
#' @param token Token to check.
#'
#' @return Timestamp with token expiry date.
#' @export
jwt_exp <- function(token) {
  l <- jose::jwt_split(token)
  structure(l$payload$exp, class = c("POSIXct", "POSIXt"))
}

#' JWT Validation
#'
#' Validates JWT by parsing it and checking its structure and expiration date.
#'
#' @param token JWT string.
#'
#' @return The input token if valid, else errors out.
#' @export
jwt_validate <- function(token) {
  # https://github.com/r-lib/jose/blob/429a46/R/jwt.R#L171
  .token_check_expiration_time <- function(payload) {
    if (length(payload$exp)) {
      stopifnot("exp claim is a number" = is.numeric(payload$exp))
      expdate <- structure(payload$exp, class = c("POSIXct", "POSIXt"))
      if (expdate < (Sys.time() - 60)) {
        stop(paste("Token has expired on", expdate), call. = FALSE)
      }
    }
    if (length(payload$nbf)) {
      stopifnot("nbf claim is a number" = is.numeric(payload$nbf))
      nbfdate <- structure(payload$nbf, class = c("POSIXct", "POSIXt"))
      if (nbfdate > (Sys.time() + 60)) {
        stop(paste("Token is not valid before", nbfdate), call. = FALSE)
      }
    }
  }
  # giving a friendlier error msg in case this isn't even valid jwt
  tmp <- strsplit(token, ".", fixed = TRUE)[[1]]
  msg <- "The input token is not a valid JWT"
  assertthat::assert_that(length(tmp) %in% c(2, 3), msg = msg)
  l <- jose::jwt_split(token)
  .token_check_expiration_time(l[["payload"]])
  token
}
