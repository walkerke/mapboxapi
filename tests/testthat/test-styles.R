test_that("get_style and list_styles work", {
  skip_on_cran()

  expect_type(
    get_style(
      style_id = "satellite-v9",
      username = "mapbox"
    ),
    "list"
  )

  expect_error(
    list_styles(username = "mapbox")
  )

})
