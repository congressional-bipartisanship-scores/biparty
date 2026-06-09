#' biparty: Congressional Bipartisanship Scores by Member and Issue Area
#'
#' @description
#' `biparty` provides Congressional Bipartisanship Scores (CBS) for all members
#' of the U.S. House and Senate from 1983 to 2024, spanning the 98th through
#' the 118th Congresses. The scores are introduced and described in Dobson and
#' Lollis (2026), with support from the Portman Center for Policy Solutions at
#' the University of Cincinnati. The package was developed by Mackenzie R.
#' Dobson, Assistant Professor of Political Science at the University of
#' Tennessee, Knoxville, and Jacob M. Lollis, Assistant Professor of Political
#' Science at the University of Cincinnati.
#'
#' @details
#' The package provides two datasets:
#' \itemize{
#'   \item [aggregate.cbs] - overall attract and offer scores per member,
#'     Congress, and chamber, with identifier crosswalk columns included.
#'   \item [issue.area.cbs] - the same scores computed separately within each
#'     of 34 CRS major policy areas, with identifier crosswalk columns included.
#' }
#'
#' Scores range from 0 to 1, with higher scores indicating more cross-party
#' activity, and are constructed from more than 2.4 million cosponsorship
#' decisions on 147,669 bills. The *attract* score measures the share of
#' out-party original cosponsors on bills the member sponsored; the *offer*
#' score measures the share of the member's own original cosponsorships that
#' went to out-party sponsors. Both measures are available in raw and
#' precision-weighted variants. The weighted version adjusts for imprecise
#' measurement associated with legislators with low bill activity, pulling
#' low-volume observations toward a Congress x chamber (or Congress x chamber
#' x issue) prior mean.
#'
#' Key functions:
#' \itemize{
#'   \item [get_member_scores()] - look up a single member's scores.
#'   \item [rank_members()] - leaderboard of members by score.
#'   \item [compare_members()] - side-by-side comparison of two or more members.
#'   \item [get_state_delegation()] - scores for an entire state delegation.
#'   \item [get_congress_summary()] - Congress-level summary statistics.
#'   \item [get_issue_scores()] - all members' scores for a single policy area.
#'   \item [get_issue_trend()] - issue-area bipartisanship trends over time.
#'   \item [list_issues()] - enumerate available CRS issue area labels.
#'   \item [plot_bipartisanship()] - violin plot of issue-area scores by party and chamber.
#'   \item [plot_trend()] - time-series line chart of scores across Congresses.
#'   \item [plot_member_profile()] - bar chart of a member's issue-area score profile.
#' }
#'
#' @references
#' Dobson, M.R. and Lollis, J.M. (2026). Congressional bipartisanship scores
#' by member and issue area, 1983--2024. Working paper.
#'
#' @author Authors: Mackenzie R. Dobson and Jacob M. Lollis.
#'   Maintainers: Mackenzie R. Dobson \email{mdobson1@@utk.edu} and
#'   Jacob M. Lollis \email{lollisjm@@ucmail.uc.edu}
#'
#' @keywords internal
"_PACKAGE"

# Silence R CMD check about unquoted tidy-eval / data column names
utils::globalVariables(c(
  "congress", "chamber", "bioguide_id", "name", "party",
  "attract_aggregate_weighted", "offer_aggregate_weighted",
  "attract_aggregate_unweighted", "offer_aggregate_unweighted",
  "state", "district", "wikipedia", "wikidata",
  "score_type", "score", "topic", "topic_label", "topic_short",
  "mean_score", "policy_area", "policy_area_number"
))
