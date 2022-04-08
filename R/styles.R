#' Get information about a style from your Mapbox account
#'
#' @param style_url Mapbox style URL
#' @param style_id A style ID
#' @param username A Mapbox username
#' @param access_token A Mapbox public or secret access token; set with \code{mb_access_token()}
#'
#' @return A list of information about your selected style.
#' @export
get_style <- function(style_id,
                      username,
                      style_url = NULL,
                      access_token = NULL) {
  access_token <- get_mb_access_token(access_token, default = "MAPBOX_SECRET_TOKEN")

  if (!is.null(style_url)) {
    username <- stringi::stri_extract(style_url, paste0("(?<=styles/).+(?=/)"))
    style_id <- stringi::stri_extract(style_url, paste0("(?<=", username, "/).+"))
  }

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
