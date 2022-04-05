#' Get information about a style from your Mapbox account
#'
#' @param style_id The style ID
#' @param username Your Mapbox username
#' @param access_token Your Mapbox public or secret access token; set with \code{mb_access_token()}
#'
#' @return A list of information about your selected style.
#' @export
get_style <- function(style_id,
                      username,
                      access_token = NULL) {
  access_token <- get_mb_access_token(access_token, default = "MAPBOX_SECRET_TOKEN")

  url <- sprintf("https://api.mapbox.com/styles/v1/%s/%s", username, style_id)

  request <- httr::GET(
    url = url,
    query = list(access_token = access_token)
  )

  content <- httr::content(request, as = "text")

  if (request$status_code != 200) {
    stop(print(content$message), call. = FALSE)
  }

  return(jsonlite::fromJSON(content))
}


#' List styles in your Mapbox account
#'
#' @param username Your Mapbox username
#' @param access_token Your Mapbox public or secret access token; set with \code{mb_access_token()}
#'
#' @return A data frame of information about styles in your Mapbox account
#' @export
list_styles <- function(username, access_token = NULL) {
  access_token <- get_mb_access_token(access_token, default = "MAPBOX_SECRET_TOKEN")

  url <- sprintf("https://api.mapbox.com/styles/v1/%s", username)

  request <- httr::GET(url, query = list(access_token = access_token))

  content <- httr::content(request, as = "text")

  if (request$status_code != 200) {
    stop(print(content$message), call. = FALSE)
  }

  return(jsonlite::fromJSON(content))
}
