test_that("aggregate.cbs has the expected schema", {
  data(aggregate.cbs, package = "biparty")
  expect_s3_class(aggregate.cbs, "data.frame")
  expect_true(all(c("congress", "chamber", "bioguide_id", "name", "party",
                    "attract_aggregate_weighted", "offer_aggregate_weighted",
                    "attract_aggregate_unweighted", "offer_aggregate_unweighted",
                    "state", "wikipedia", "wikidata",
                    "total_n_bills_sponsored", "prop_bills_with_cosponsor") %in%
                    names(aggregate.cbs)))
  expect_true(all(aggregate.cbs$chamber %in% c("HOUSE", "SENATE")))
  expect_true(all(aggregate.cbs$party %in% c("D", "R")))
  expect_true(all(aggregate.cbs$attract_aggregate_weighted >= 0 &
                    aggregate.cbs$attract_aggregate_weighted <= 1,
                  na.rm = TRUE))
  expect_true(all(aggregate.cbs$offer_aggregate_weighted >= 0 &
                    aggregate.cbs$offer_aggregate_weighted <= 1,
                  na.rm = TRUE))
})

test_that("issue.area.cbs has the expected wide schema", {
  data(issue.area.cbs, package = "biparty")
  expect_s3_class(issue.area.cbs, "data.frame")
  expect_true(all(c("congress", "chamber", "bioguide_id", "name", "party",
                    "state", "wikipedia", "wikidata") %in%
                    names(issue.area.cbs)))
  for (iss in c("health", "defense", "econ")) {
    expect_true(paste0("attract_", iss, "_weighted") %in% names(issue.area.cbs))
    expect_true(paste0("offer_",   iss, "_weighted") %in% names(issue.area.cbs))
  }
})
