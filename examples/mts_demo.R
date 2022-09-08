\dontrun{
library(tidycensus)
library(mapboxapi)
options(tigris_use_cache = TRUE)

# Get the national data on median age
us_median_age_tract <- get_acs(
  geography = "tract",
  variables = "B01002_001",
  state = c(state.abb, "DC"),
  year = 2020,
  geometry = TRUE
)

# Get it for counties as well
us_median_age_county <- get_acs(
  geography = "county",
  variables = "B01002_001",
  year = 2020,
  geometry = TRUE
)

# Create a source from the datasets
mts_create_source(data = us_median_age_tract,
                  tileset_id = "us_median_age_tract",
                  username = "your_mapbox_username")

mts_create_source(data = us_median_age_county,
                  tileset_id = "us_median_age_county",
                  username = "your_mapbox_username")

# Build out the recipe.  First, create a recipe layer with
# appropriate options.  We'll want a larger tile size and to restrict the minzoom
# to 4; a maxzoom of 12 will be fine as we can overzoom beyond that
#
# Your source ID will be returned by `mts_create_source()`, so use that value
tract_layer <- recipe_layer(
  source = "mapbox://tileset-source/your_mapbox_username/us_median_age_tract",
  minzoom = 4,
  maxzoom = 12,
  tiles = tile_options(layer_size = 2500)
)

county_layer <- recipe_layer(
  source = "mapbox://tileset-source/your_mapbox_username/us_median_age_county",
  minzoom = 2,
  maxzoom = 5
)

recipe <- mts_make_recipe(tracts = tract_layer, counties = county_layer)

# Validate the recipe
mts_validate_recipe(recipe)

# Create a tileset from the recipe
mts_create_tileset(tileset_name = "median_age_acs",
                   username = "your_mapbox_username",
                   recipe = recipe)

# Publish the tileset
mts_publish_tileset(tileset_name = "median_age_acs",
                    username = "your_mapbox_username")

# If necessary, update the recipe
mts_update_recipe(tileset_name = "median_age_acs",
                  username = "your_mapbox_username",
                  recipe = new_recipe)

# Publish the tileset again after you've updated the recipe
mts_publish_tileset(tileset_name = "median_age_acs",
                    username = "your_mapbox_username")
}
