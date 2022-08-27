## code to prepare `internal` dataset goes here

# See {sfext} package repo for reproducible code to generate dist_unit_options
# https://github.com/elipousson/sfext/blob/main/data-raw/units.R
dist_unit_options <- sfext::dist_unit_options

usethis::use_data(dist_unit_options, internal = TRUE, overwrite = TRUE)
