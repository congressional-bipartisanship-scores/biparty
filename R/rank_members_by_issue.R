#' Rank members by Congressional Bipartisanship Score within an issue area
#'
#' A focused variant of [rank_members()] where the issue area is the primary
#' argument. Returns a sorted leaderboard of members by their attract or offer
#' score within a single CRS policy area.
#'
#' @param issue Character. Issue area label (e.g., \code{"health"},
#'   \code{"defense"}). See [list_issues()] for all valid values.
#' @param congress Integer. Congress number (e.g., \code{117}).
#' @param chamber Character. \code{"house"} or \code{"senate"}.
#' @param score_type Character. \code{"attract"} (default) or \code{"offer"}.
#' @param party Optional character. \code{"D"} or \code{"R"} to restrict to
#'   one party.
#' @param n Integer. Number of members to return. Default \code{10}.
#'   Pass \code{Inf} to return all.
#' @param descending Logical. Rank highest scores first (default \code{TRUE}).
#'
#' @return A tibble with columns \code{rank}, \code{name}, \code{party},
#'   \code{bioguide_id}, and the issue-area score column used for ranking.
#'
#' @examples
#' # Top 10 senators by health attract score in the 117th Congress
#' rank_members_by_issue("health", 117, "senate")
#'
#' # Offer scores instead
#' rank_members_by_issue("health", 117, "senate", score_type = "offer")
#'
#' # House members instead
#' rank_members_by_issue("health", 117, "house")
#'
#' # Republicans only
#' rank_members_by_issue("defense", 117, "senate", party = "R")
#'
#' # Democrats only
#' rank_members_by_issue("defense", 117, "senate", party = "D")
#'
#' # Top 5 only
#' rank_members_by_issue("tax", 117, "senate", n = 5)
#'
#' # Full ranking, lowest scores first
#' rank_members_by_issue("labor", 117, "house",
#'                       n = Inf, descending = FALSE)
#'
#' @seealso [rank_members()], [get_issue_scores()], [list_issues()]
#' @export
rank_members_by_issue <- function(issue,
                                   congress,
                                   chamber,
                                   score_type = c("attract", "offer"),
                                   party      = NULL,
                                   n          = 10,
                                   descending = TRUE) {
  score_type <- match.arg(score_type)

  rank_members(
    congress   = congress,
    chamber    = chamber,
    score_type = score_type,
    party      = party,
    issue      = issue,
    n          = n,
    descending = descending
  )
}
