#' List tokens from a Mapbox account
#'
#' @param username The Mapbox username for which you'd like to list access tokens.
#' @param default If TRUE, will only include the default token for an account. If FALSE, will include all other tokens except for the default.  Defaults to NULL.
#' @param limit The maximum number of tokens to return. Defaults to NULL.
#' @param sortby How to sort the returned tokens; one of \code{"created"} or \code{"modified"}.
#' @param usage If \code{"pk"}, returns only public tokens; if \code{"sk"}, returns only secret tokens.  Defaults to \code{NULL}, which returns all tokens in the scope of the supplied access token.
#' @param access_token Your Mapbox access token.  If left blank, will first check to see if you have a secret token stored in .Renviron, then a public token.
#'
#' @return A tibble of information about tokens in your Mapbox account.
#' @export
list_tokens <- function(username,
                        default = NULL,
                        limit = NULL,
                        sortby = "created",
                        usage = NULL,
                        access_token = NULL) {

  if (is.null(access_token)) {
    # Use secret token first, then public token
    if (Sys.getenv("MAPBOX_SECRET_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")
    } else {
      if (Sys.getenv("MAPBOX_PUBLIC_TOKEN") != "") {
        access_token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")
      } else {
        stop("A Mapbox access token is required.  Please locate yours from your Mapbox account.", call. = FALSE)
      }

    }
  }

  base <- sprintf("https://api.mapbox.com/tokens/v2/%s", username)


  if (!is.null(default)) {
    if (default) {
      default <- 'true'
    } else {
      default <- 'false'
    }

  }

  request <- httr::GET(base, query = list(access_token = access_token,
                                          default = default,
                                          limit = limit,
                                          sortby = sortby,
                                          usage = usage
                                          ))

  if (request$status_code != 200) {
    pull <- fromJSON(content)
    stop(pull$message, call. = FALSE)
  }

  output <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON() %>%
    dplyr::as_tibble()

  return(output)

}
