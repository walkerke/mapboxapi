#' Geocode an address using the Mapbox Geocoding API (skeleton at the moment)
#'
#' @param query
#' @param limit
#' @param sf
#' @param access_token
#'
#' @return
#' @export
#'
#' @examples
mb_geocode <- function(query, limit = 1, sf = FALSE, access_token = NULL) {

  if (is.null(access_token)) {
    stop("A Mapbox access token is required.  Please locate yours from your Mapbox account.",
         call. = FALSE)
  }

  query <- curl::curl_escape(query)

  base <- paste0("https://api.mapbox.com/geocoding/v5/mapbox.places/",
                 query,
                 ".json")

  req <- httr::GET(base, query = list(
    access_token = access_token,
    limit = limit
  ))

  cont <- content(req, as = "text")

  if (sf) {
    return(read_sf(cont))
  } else {

    cont2 <- fromJSON(cont, flatten = TRUE)

    coords <- unlist(cont2$features$geometry.coordinates)

    return(coords)

  }


}
