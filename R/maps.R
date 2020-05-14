#' Upload dataset to your Mapbox account
#'
#' @param input The path to the dataset to upload
#' @param username Your Mapbox username
#' @param access_token Your Mapbox access token; must have secret scope
#' @param tileset_name The name of the tileset in your Mapbox account
#' @param tileset_id The ID of the tileset in your Mapbox account
#'
#' @export
upload_tiles <- function(input,
                         username,
                         access_token = NULL,
                         tileset_id = NULL,
                         tileset_name = NULL) {

  if (is.null(access_token)) {

    if (Sys.getenv("MAPBOX_SECRET_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")
    } else {
      stop("A Mapbox secret access token is required.  Please locate yours from your Mapbox account.",
           call. = FALSE)
    }
  }

  # If tileset_id is NULL, use the basename of the input
  if (is.null(tileset_id)) {
    tileset_id <- basename(input)
  }

  # Get AWS credentials
  base1 <- sprintf("https://api.mapbox.com/uploads/v1/%s/credentials",
                   username)

  req <- httr::POST(base1, query = list(access_token = access_token))

  credentials <- httr::content(req, as = "text") %>%
    jsonlite::fromJSON()

  # Use these credentials to transfer to the staging bucket
  aws.s3::put_object(file = input,
             object = credentials$key,
             bucket = credentials$bucket,
             region = "us-east-1",
             key = credentials$accessKeyId,
             secret = credentials$secretAccessKey,
             session_token = credentials$sessionToken,
             check_region = FALSE)


  # Once done, generate the upload
  url <- sprintf("http://%s.s3.amazonaws.com/%s",
                 credentials$bucket,
                 credentials$key)

  if (!is.null(tileset_name)) {
    upload <- sprintf('{"url": "%s", "tileset": "%s.%s"}',
                      url, username, tileset_id)
  } else {
    upload <- sprintf('{"url": "%s", "tileset": "%s.%s", name: %s}',
                      url, username, tileset_id, tileset_name)
  }



  base2 <- sprintf('https://api.mapbox.com/uploads/v1/%s',
                   username)

  request <- httr::POST(base2,
                        httr::add_headers("Content-Type" = "application/json",
                                          "Cache-Control" = "no-cache"),
                        body = upload,
                        query = list(access_token = access_token))

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Upload failed: your error message is %s", response),
         call. = FALSE)
  }

  message(sprintf("Your upload ID is %s", response$id))

}


#' Check the status of a Mapbox upload
#'
#' @param username Your account's username
#' @param id The upload ID
#' @param access_token Your Mapbox access token
#'
#' @export
check_upload_status <- function(username,
                                id,
                                access_token = NULL) {

  if (is.null(access_token)) {

    if (Sys.getenv("MAPBOX_SECRET_TOKEN") != "") {
      access_token <- Sys.getenv("MAPBOX_SECRET_TOKEN")
    } else {
      stop("A Mapbox secret access token is required.  Please locate yours from your Mapbox account.",
           call. = FALSE)
    }
  }

  status <- GET(sprintf("https://api.mapbox.com/uploads/v1/%s/%s",
                        username, id),
                query = list(access_token = access_token))

  status %>%
    content(as = "text") %>%
    fromJSON()

}


#' Get information about features in a tileset using the Tilequery API
#'
#' @param location The location for which you'd like to query tiles, expressed as either a length-2 vector of longitude and latitude or an address you'd like to geocode.
#' @param tileset_id The tileset ID to query.
#' @param radius The radius around the point for which you'd like to query features.  For point-in-polygon queries (e.g. "what county is my point located in?") the default of 0 should be used.
#' @param limit How many features to return (defaults to 5). Can be an integer between 1 and 50.
#' @param dedupe Whether or not to return duplicate features as identified by their IDs.  The default, TRUE, will de-duplicate your dataset.
#' @param geometry The feature geometry type to query - can be \code{"point"}, \code{"linestring"}, or \code{"polygon"}. If left blank, all geometry types will be queried.
#' @param layers A vector of layer IDs you'd like to query (recommended); if left blank will query all layers, with the limitation that at most 50 features can be returned.
#' @param access_token Your Mapbox access token, which can be set with \code{mb_access_token()}.
#'
#' @return A data frame of information about the requested features.
#' @export
query_tiles <- function(location,
                        tileset_id,
                        radius = 0,
                        limit = 5,
                        dedupe = TRUE,
                        geometry = NULL,
                        layers = NULL,
                        access_token = NULL) {

  if (Sys.getenv("MAPBOX_ACCESS_TOKEN") != "") {
    access_token <- Sys.getenv("MAPBOX_ACCESS_TOKEN")
  } else {
    stop("A Mapbox access token is required.  Please locate yours from your Mapbox account.",
         call. = FALSE)
  }

  # If location is an address, geocode it
  if (length(location) == 1) {
    coords <- mb_geocode(location, access_token = access_token)
  } else if (length(location) == 2) {
    coords <- location
  } else {
    stop("The specified location must either be a coordinate pair or a valid address",
         call. = FALSE)
  }

  # Format the requested layers if included
  if (!is.null(layers)) {
    layers <- paste0(layers, collapse = ",")
  }

  # Format the dedupe param
  if (dedupe) {
    dedupe <- "true"
  } else {
    dedupe <- "false"
  }

  # Construct the JSON for the request
  base <- sprintf("https://api.mapbox.com/v4/%s/tilequery/%s,%s.json",
                  tileset_id, coords[1], coords[2])

  # Build the request
  query <- httr::GET(base, query = list(
    radius = radius,
    limit = limit,
    dedupe = dedupe,
    geometry = geometry,
    layers = layers,
    access_token = access_token
  ))

  content <- content(query, as = "text")

  if (query$status_code != 200) {
    pull <- fromJSON(content)
    stop(pull$message, call. = FALSE)
  }

  result <- jsonlite::fromJSON(content)

  return(result)

}


get_vector_tiles <- function(tileset_id,
                             location,
                             zoom,
                             output = "sf") {

}
