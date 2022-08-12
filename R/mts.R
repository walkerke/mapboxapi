make_newline_geojson <- function(data, file_loc) {

  # Credit to https://github.com/SymbolixAU/geojsonsf/issues/96
  # for example implementation
  data_gj <- geojsonsf::sf_geojson(data, atomise = TRUE)
  filecon <- file(file_loc)
  writeLines(data_gj, filecon, sep = "\n")
  close(filecon)

}

#' Create a Mapbox tileset source from a sf object using the Mapbox Tiling Service API
#'
#' The \code{mts_create_source()} function can be used to create, append to, or replace an existing tileset source.
#'
#' @param data An input simple features object
#' @param tileset_id The tileset ID. If the tileset ID already exists in your Mapbox account, this function will overwrite the existing source with a new source.
#' @param username Your Mapbox username
#' @param access_token Your Mapbox access token with secret scope. Install with \code{mb_access_token()} after you retrieve it from your Mapbox account.
#'
#' @return A list of the MTS API's responses, including the name of the tileset source in your Mapbox account.  You'll use this name to build a MTS recipe.
#' @export
mts_create_source <- function(data, tileset_id, username,
                              access_token = NULL) {

  # Grab & validate the access token
  # Secret scope is required
  access_token <- get_mb_access_token(access_token, secret_required = TRUE)

  # Make a temporary directory to store the newline-delimited GeoJSON
  tmp <- tempdir()

  # Write the dataset to the temporary directory
  output_name <- paste0(deparse(substitute(data)), ".geojson.ld")

  out_file <- file.path(tmp, output_name)

  make_newline_geojson(data, out_file)

  # Once the object is created, send it to the MTS API
  url <- sprintf("https://api.mapbox.com/tilesets/v1/sources/%s/%s",
                 username, tileset_id)

  request <- httr::POST(url = url,
                        body = list(file = httr::upload_file(out_file)),
                        query = list(access_token = access_token),
                        httr::add_headers("Content-Type: multipart/form-data"))

  # Capture the response from req and give back
  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Attempt failed: your error message is %s", response),
         call. = FALSE
    )
  }

  rlang::inform(c("v" = sprintf("Successfully created tileset source with the ID\n'%s'.",
                              response$id),
                "Use your source ID to build your tileset's recipe."))

  # Return the response as a list
  return(response)

}


#' List tileset sources in your Mapbox account
#'
#' @param username Your Mapbox username
#' @param access_token Your Mapbox access token with secret scope.
#'
#' @return A data frame containing information on your tileset sources.
#' @export
mts_list_sources <- function(username,
                             access_token = NULL) {

  access_token <- get_mb_access_token(access_token, secret_required = TRUE)



  request <- httr::GET(
    url = sprintf("https://api.mapbox.com/tilesets/v1/sources/%s", username),
    query = list(access_token = access_token)
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Request failed: your error message is %s", response),
         call. = FALSE
    )
  }

  return(response)

}


mts_make_recipe <- function(...) {

  # Create recipe skeleton
  # Make this easier in the future
  recipe <- list(
      version = 1,
      layers = list(...)
    )
  # Convert the recipe to JSON
  # recipe_json <- jsonlite::toJSON(recipe, auto_unbox = TRUE)

  return(recipe)

}

mts_validate_recipe <- function(recipe,
                                access_token = NULL) {

  access_token <- get_mb_access_token(access_token)

  request <- httr::PUT(
    url = "https://api.mapbox.com/tilesets/v1/validateRecipe",
    body = recipe,
    encode = "json",
    query = list(access_token = access_token),
    httr::content_type_json()
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Request failed: your error message is %s", response),
         call. = FALSE
    )
  }

  return(response)

}


mts_create_tileset <- function(recipe,
                               tileset_name,
                               username,
                               request_name = tileset_name,
                               access_token = NULL) {

  access_token <- get_mb_access_token(access_token, secret_required = TRUE)

  tileset <- sprintf("%s.%s", username, tileset_name)

  url <- sprintf("https://api.mapbox.com/tilesets/v1/%s", tileset)

  # Prepare the request body
  req_body <- list(
    recipe = recipe,
    name = request_name
  )

  request <- httr::POST(
    url = url,
    body = req_body,
    encode = "json",
    query = list(access_token = access_token),
    httr::content_type_json()
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Tileset creation failed: your error message is %s", response),
         call. = FALSE
    )
  }

  return(response)

}


mts_publish_tileset <- function(tileset_name,
                                username,
                                access_token = NULL) {

  access_token <- get_mb_access_token(access_token, secret_required = TRUE)

  tileset <- sprintf("%s.%s", username, tileset_name)

  url <- sprintf("https://api.mapbox.com/tilesets/v1/%s/publish",
                 tileset)

  request <- httr::POST(
    url = url,
    query = list(access_token = access_token)
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Request failed: your error message is %s", response),
         call. = FALSE
    )
  }

  return(response)

}

