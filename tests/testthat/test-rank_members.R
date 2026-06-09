test_that("rank_members() returns N rows sorted by score", {
  out <- rank_members(117, "senate", score_type = "attract", n = 10)
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 10)
  expect_equal(out$rank, 1:10)
  expect_true(all(diff(out$attract_aggregate_weighted) <= 0))
})

test_that("rank_members() ascending mode reverses the order", {
  asc <- rank_members(117, "senate", score_type = "attract", n = 10,
                      descending = FALSE)
  expect_true(all(diff(asc$attract_aggregate_weighted) >= 0))
})

test_that("rank_members() respects party filter", {
  out <- rank_members(117, "senate", score_type = "attract", party = "R",
                      n = Inf)
  expect_true(all(out$party == "R"))
})

test_that("rank_members() supports issue argument", {
  out <- rank_members(117, "senate", score_type = "attract", issue = "health",
                      n = 5)
  expect_true("attract_health_weighted" %in% names(out))
})

test_that("rank_members() rejects bad inputs", {
  expect_error(rank_members(chamber = "senate"))
  expect_error(rank_members(117, chamber = "moon"), "Unrecognized chamber")
  expect_error(rank_members(117, "senate", score_type = "attract",
                             issue = "nonsense"), "Unknown issue")
})
