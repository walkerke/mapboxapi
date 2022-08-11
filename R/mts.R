make_newline_geojson <- function(data, file_loc) {

  # Credit to https://github.com/SymbolixAU/geojsonsf/issues/96
  # for example implementation
  data_gj <- geojsonsf::sf_geojson(data, atomise = TRUE)
  filecon <- file(file_loc)
  writeLines(data_gj, filecon, sep = "\n")
  close(filecon)

}

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


mts_make_recipe <- function(..., name) {

  # Create recipe skeleton
  # Make this easier in the future
  recipe <- list(
    recipe = list(
      version = 1,
      layers = list(...)
    ),
    name = name
  )

  # Convert the recipe to JSON
  recipe_json <- jsonlite::toJSON(recipe, auto_unbox = TRUE)

  return(recipe_json)

}


mts_create_tileset <- function(recipe,
                               tileset_name,
                               username,
                               request_name = tileset_name,
                               access_token = NULL) {

  access_token <- get_mb_access_token(access_token, secret_required = TRUE)

  tileset <- sprintf("%s.%s", username, tileset_name)

  url <- sprintf("https://api.mapbox.com/tilesets/v1/%s", tileset)

  # Make a temporary directory to store the JSON recipe
  tmp <- tempdir()

  # Write the dataset to the temporary directory
  out_file <- file.path(tmp, "recipe.json")

  writeLines(recipe, out_file)

  request <- httr::POST(
    url = url,
    body = list(file = httr::upload_file(out_file)),
    query = list(access_token = access_token),
    httr::add_headers("Content-Type:application/json")
  )

  response <- request %>%
    httr::content(as = "text") %>%
    jsonlite::fromJSON()

  if (request$status_code != "200") {
    stop(sprintf("Tileset creation failed: your error message is %s", response),
         call. = FALSE
    )
  }


}

