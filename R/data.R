#' Congressional Bipartisanship Scores (CBS) -- Aggregate
#'
#' Member-level aggregate (overall) bipartisanship scores for the U.S. Congress.
#' One row per legislator per Congress per chamber. Scores capture two distinct
#' forms of bipartisan behavior: attracting original out-party cosponsors to a
#' member's own bills and offering original cosponsorships to bills sponsored by
#' out-party members. Both measures are adjusted via empirical-Bayes shrinkage
#' toward the Congress x chamber mean to address imprecise measurement for members with low bill
#' activity. Identifier crosswalk columns are included directly in this dataset
#' to facilitate merging with other congressional data sources.
#'
#' @format A data frame with 11,549 rows and 18 columns:
#' \describe{
#'   \item{congress}{Integer. Congress number (e.g., \code{117}).}
#'   \item{chamber}{Character. \code{"HOUSE"} or \code{"SENATE"}.}
#'   \item{bioguide_id}{Character. Unique legislator identifier from the
#'     Biographical Directory of the U.S. Congress
#'     (e.g., \code{"P000449"} for Sen. Rob Portman).}
#'   \item{name}{Character. Legislator display name (uppercase).}
#'   \item{party}{Character. \code{"D"} or \code{"R"}. Independents and
#'     party-switchers are recoded to the major party with which they caucus.}
#'   \item{attract_aggregate_weighted}{Numeric, [0, 1]. Precision-weighted
#'     aggregate attract score. Share of out-party original cosponsors on
#'     bills the member sponsored, shrunk toward the Congress x chamber mean.}
#'   \item{offer_aggregate_weighted}{Numeric, [0, 1]. Precision-weighted
#'     aggregate offer score. Share of the member's original cosponsorships
#'     directed toward out-party-sponsored bills, shrunk toward the Congress
#'     x chamber mean.}
#'   \item{attract_aggregate_unweighted}{Numeric, [0, 1]. Raw (unshrunk)
#'     aggregate attract score.}
#'   \item{offer_aggregate_unweighted}{Numeric, [0, 1]. Raw (unshrunk)
#'     aggregate offer score.}
#'   \item{state}{Character. Two-letter state abbreviation.}
#'   \item{district}{Character. Congressional district. For senators:
#'     \code{"No Congressional District (Senate)"}. For non-voting House
#'     delegates: district designation as recorded by congress.gov.}
#'   \item{thomas_id}{Character. Legacy THOMAS/Congress.gov numeric ID.
#'     Available for House members only; \code{"Unavailable"} for senators.}
#'   \item{icpsr_id}{Numeric. ICPSR legislator identifier, used in
#'     DW-NOMINATE and related roll-call datasets. Excludes legislators who
#'     served only a partial term and non-voting delegates.}
#'   \item{govtrack_id}{Numeric. GovTrack.us person ID.}
#'   \item{wikipedia}{Character. Wikipedia article title for the legislator.}
#'   \item{wikidata}{Character. Wikidata item QID (e.g., \code{"Q59676310"}).}
#'   \item{total_n_bills_sponsored}{Integer. Total number of bills introduced
#'     by the member in that congressional term. Because bipartisanship scores
#'     are calculated only from bills that attracted at least one cosponsor,
#'     this variable allows users to contextualize scores relative to overall
#'     sponsorship activity.}
#'   \item{prop_bills_with_cosponsor}{Numeric, [0, 1]. Proportion of the
#'     member's sponsored bills that attracted at least one cosponsor
#'     (regardless of party) in that congressional term.}
#' }
#'
#' @source Dobson, M.R. and Lollis, J.M. (2026). Congressional bipartisanship
#'   scores by member and issue area, 1983--2024. \emph{Scientific Data}.
#'   Data constructed from Congress.gov bill and cosponsorship records spanning
#'   the 98th through 118th Congresses (1983--2024). Also available at Harvard
#'   Dataverse and through this package.
#'
#' @examples
#' data(aggregate.cbs)
#' head(aggregate.cbs)
#'
#' # Precision-weighted attract score for Rob Portman in the 117th Congress
#' subset(aggregate.cbs, bioguide_id == "P000449" & congress == 117,
#'        select = c("name", "congress", "attract_aggregate_weighted",
#'                   "offer_aggregate_weighted"))
"aggregate.cbs"


#' Congressional Bipartisanship Scores (CBS) -- Issue Area
#'
#' Member-level issue-specific bipartisanship scores for the U.S. Congress.
#' Same row structure as [aggregate.cbs] (one row per legislator per
#' Congress per chamber), but with attract and offer scores computed separately
#' within each of 34 Congressional Research Service (CRS) policy areas, in both
#' precision-weighted and raw (unweighted) variants. Missing values indicate the
#' member sponsored or cosponsored no bills in that issue area in that Congress.
#' Identifier crosswalk columns are included directly in this dataset.
#'
#' @details
#' **Column naming conventions:**
#' \itemize{
#'   \item \code{attract_<label>_weighted} -- precision-weighted attract score
#'     within the issue area (shrunk toward the Congress x chamber x issue
#'     area mean).
#'   \item \code{offer_<label>_weighted} -- precision-weighted offer score
#'     within the issue area.
#'   \item \code{attract_<label>_unweighted} -- raw attract score within the
#'     issue area.
#'   \item \code{offer_<label>_unweighted} -- raw offer score within the issue
#'     area.
#' }
#' where \code{<label>} is a short issue code (e.g., \code{"health"},
#' \code{"defense"}, \code{"tax"}). See [list_issues()] for the full mapping
#' from short label to CRS policy area name, see [list_issues()].
#'
#' The dataset contains score columns for all 34 CRS policy areas plus a small
#' residual category (\code{_na_}) retaining the 22 bills for which no CRS
#' policy area was available. These bills are included in the data but
#' \code{_na_} columns should not be interpreted as a substantive policy area.
#'
#' @format A data frame with 11,549 rows and 152 columns. Identifier and
#' crosswalk columns are: \code{congress}, \code{chamber}, \code{name},
#' \code{party}, \code{bioguide_id}, \code{state}, \code{district},
#' \code{thomas_id}, \code{icpsr_id}, \code{govtrack_id}, \code{wikipedia},
#' \code{wikidata}. The remaining 140 columns are score columns for the 34 CRS
#' policy areas plus the residual \code{_na_} category (4 score variants x 35
#' codes = 140 columns), following the naming conventions described above.
#'
#' @source Dobson, M.R. and Lollis, J.M. (2026). Congressional bipartisanship
#'   scores by member and issue area, 1983--2024. \emph{Scientific Data}.
#'   Data constructed from Congress.gov bill and cosponsorship records spanning
#'   the 98th through 118th Congresses (1983--2024). Also available at Harvard
#'   Dataverse and through this package.
#'
#' @examples
#' data(issue.area.cbs)
#'
#' # Health scores in the 117th Congress
#' subset(issue.area.cbs, congress == 117,
#'        select = c("name", "party",
#'                   "attract_health_weighted", "offer_health_weighted"))
"issue.area.cbs"

