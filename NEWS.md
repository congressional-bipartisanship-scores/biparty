# biparty 0.1.0

Initial public release of Congressional Bipartisanship Scores (CBS),
introduced and described in Authors (2026).

## Datasets

* `aggregate.cbs` — aggregate attract and offer scores for 11,549
  member-Congress-chamber observations covering 2,056 unique legislators
  across the 98th through 118th Congresses (1983--2024). Includes
  `total_n_bills_sponsored` and `prop_bills_with_cosponsor` to contextualize
  scores relative to overall sponsorship activity. Member-level identifier
  crosswalks (Bioguide, GovTrack, THOMAS, ICPSR, Wikipedia, Wikidata) are
  included directly in the dataset.
* `issue.area.cbs` — the same scores computed separately within each of 34
  CRS policy areas, with identifier crosswalk columns included.

## Functions

* `get_member_scores()` — look up a member's scores by Bioguide ID or name
  substring, with optional issue-area scores via `include_issues = TRUE`.
* `get_member_trend()` — return a member's scores as a tidy time series
  across Congresses.
* `compare_members()` — side-by-side comparison of two or more members.
* `get_state_delegation()` — scores for all members from a given state,
  with optional aggregation to a single summary row.
* `get_congress_summary()` — Congress-level mean attract and offer scores,
  with optional breakdown by party and issue area.
* `rank_members()` — leaderboard of members by attract or offer score,
  overall or within an issue area.
* `rank_members_by_issue()` — focused variant of `rank_members()` with
  issue area as the primary argument.
* `get_issue_scores()` — tidy subset of scores for a single CRS policy area.
* `get_issue_trend()` — mean attract and offer scores for a single CRS
  policy area across all available Congresses.
* `list_issues()` — enumerate available CRS issue area labels.
* `plot_bipartisanship()` — violin plot of issue-area score distributions
  by party and chamber, with optional member highlight.
* `plot_trend()` — time-series line chart of scores across Congresses,
  supporting both member and party-average modes.
* `plot_member_profile()` — profile card combining a member photo, career
  summary statistics, and a bar chart of issue-area scores.

## Methodology

Scores are constructed from more than 2.4 million cosponsorship decisions
on 147,669 bills scraped from the Congress.gov API. Both attract and offer
measures use original cosponsorship (cosponsor date matches bill introduction
date). Weighted variants adjust for imprecise measurement associated with
legislators with low bill activity, pulling low-volume observations toward a
Congress x chamber (or Congress x chamber x issue) prior mean.
