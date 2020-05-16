#' Geocode an address using the Mapbox Geocoding API (skeleton at the moment)
#'
#' @param query The address query, formatted as a character string.
#' @param limit How many results to return; defaults to 1
#' @param sf If TRUE, returns the query result as an sf object.  Defaults to FALSE.
#' @param access_token The Mapbox access token (required); can be set with \code{mb_access_token}.
#'
#' @export
mb_geocode <- function(query, limit = 1, sf = FALSE, access_token = NULL) {

  if (is.null(access_token)) {
    # Use public token first, then secret token
    if (Sys.getenv("MAPBOX_PUBLIC_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")
    } else {
      if (Sys.getenv("MAPBOX_SECRET_TOKEN" != "")) {
        access_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")
      } else {
        stop("A Mapbox access token is required.  Please locate yours from your Mapbox account.", call. = FALSE)
      }
    }
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
