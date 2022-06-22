test_that("helpers work", {

  nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

  bbox <- location_to_bbox(nc)

  expect_s3_class(bbox, "bbox")
  expect_type(bbox_to_center(bbox), "double")

  expect_true(is_bbox(bbox))
  expect_true(is_sfc(nc$geometry))
  expect_true(is_sf(nc))

  expect_equal(as_dist_units(1000, to = "km"), as_dist_units(1, from = "km", to = NULL))

  expect_true(is_hex("#ffffff"))
  hex_red <- col2hex("red")
  expect_true(is_hex(hex_red))
})
