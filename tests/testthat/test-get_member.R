test_that("get_member_scores() finds Sen. Portman by bioguide ID", {
  out <- get_member_scores("P000449")
  expect_s3_class(out, "tbl_df")
  expect_true(nrow(out) >= 1)
  expect_true(all(out$bioguide_id == "P000449"))
})

test_that("get_member_scores() finds Sen. Portman by name substring", {
  out <- get_member_scores("portman")
  expect_true(nrow(out) >= 1)
  expect_true(all(grepl("PORTMAN", out$name)))
})

test_that("get_member_scores() respects the congress filter", {
  out <- get_member_scores("P000449", congress = 117)
  expect_true(all(out$congress == 117))
})

test_that("get_member_scores() errors on unknown member", {
  expect_error(get_member_scores("NOBODY_MATCHES_THIS_STRING"),
               "No member matches")
})

test_that("get_member_scores(include_issues = TRUE) attaches issue columns", {
  out <- get_member_scores("P000449", include_issues = TRUE)
  expect_true("attract_health_weighted" %in% names(out))
  expect_true("offer_health_weighted"   %in% names(out))
})

test_that("get_member_scores() includes crosswalk columns", {
  out <- get_member_scores("P000449")
  expect_true(all(c("state", "wikipedia", "wikidata", "govtrack_id") %in%
                    names(out)))
})

test_that("get_member_scores() validates inputs", {
  expect_error(get_member_scores(c("a", "b")))
  expect_error(get_member_scores(123))
})

test_that("get_member_scores() rejects unknown chamber", {
  expect_error(get_member_scores("P000449", chamber = "martian"),
               "Unrecognized chamber")
})
