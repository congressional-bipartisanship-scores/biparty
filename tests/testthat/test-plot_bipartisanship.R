test_that("plot_bipartisanship() returns a ggplot", {
  p <- plot_bipartisanship(117)
  expect_s3_class(p, "ggplot")
})

test_that("plot_bipartisanship() accepts a highlight Bioguide ID", {
  p <- plot_bipartisanship(117, chambers = "senate", highlight = "P000449")
  expect_s3_class(p, "ggplot")
})

test_that("plot_bipartisanship() errors cleanly on empty filter set", {
  expect_error(
    plot_bipartisanship(999, chambers = "senate"),
    "No data"
  )
})

test_that("plot_bipartisanship() rejects unknown direction", {
  expect_error(plot_bipartisanship(117, directions = "fake"),
               "Invalid direction")
})
