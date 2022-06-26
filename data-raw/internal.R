## code to prepare `internal` dataset goes here

# See {overedge} package repo for reproducible code to generate dist_unit_options
# https://github.com/elipousson/overedge/blob/c4428036201270a70654b67121872a08d061c22e/data-raw/data.R#L248-L306
dist_unit_options <- overedge:::dist_unit_options

usethis::use_data(dist_unit_options, internal = TRUE, overwrite = TRUE)
