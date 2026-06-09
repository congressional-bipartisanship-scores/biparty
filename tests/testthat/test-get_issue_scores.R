test_that("get_issue_scores() returns expected columns", {
  out <- get_issue_scores("health", congress = 117)
  expect_s3_class(out, "tbl_df")
  expect_equal(
    names(out),
    c("congress", "chamber", "bioguide_id", "name", "party",
      "issue", "attract", "offer")
  )
  expect_true(all(out$congress == 117))
  expect_true(all(out$issue == "health"))
})

test_that("get_issue_scores() accepts short, full, and title-case labels", {
  a <- get_issue_scores("defense",   congress = 117)
  b <- get_issue_scores("armed forces and national security", congress = 117)
  d <- get_issue_scores("Armed Forces and National Security", congress = 117)
  expect_equal(nrow(a), nrow(b))
  expect_equal(nrow(a), nrow(d))
})

test_that("get_issue_scores() drops all-NA rows by default", {
  out <- get_issue_scores("health", congress = 117)
  expect_false(any(is.na(out$attract) & is.na(out$offer)))
})

test_that("get_issue_scores() errors on unknown issue", {
  expect_error(get_issue_scores("blockchain"), "Unknown issue")
})

test_that("list_issues() returns a non-empty crosswalk", {
  out <- list_issues()
  expect_s3_class(out, "tbl_df")
  expect_true(all(c("policy_area_number", "policy_area",
                    "topic_short", "topic_label") %in% names(out)))
  expect_gt(nrow(out), 20)
})
