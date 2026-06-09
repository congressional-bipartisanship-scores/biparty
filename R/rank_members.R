#' Rank members by Congressional Bipartisanship Score
#'
#' Returns a leaderboard of members sorted by either the attract or offer
#' score within a specified Congress and chamber. Optionally restrict
#' to a party, or rank by an issue-area score instead of the aggregate score.
#'
#' @param congress Integer. Congress number (e.g., \code{117}).
#' @param chamber Character. \code{"house"} or \code{"senate"}.
#' @param score_type Character. \code{"attract"} (default) or \code{"offer"}.
#' @param party Optional character. \code{"D"} or \code{"R"} to restrict
#'   to one party.
#' @param issue Optional character. If supplied, ranks by this issue-area
#'   score instead of the aggregate score. See [list_issues()] for valid
#'   labels.
#' @param n Integer. Number of members to return. Default \code{10}.
#'   Pass \code{Inf} to return all.
#' @param descending Logical. Rank highest scores first (default), or
#'   lowest-first if \code{FALSE}.
#'
#' @return A tibble with columns \code{rank}, \code{name}, \code{party},
#'   \code{bioguide_id}, and the score column used for ranking.
#'
#' @examples
#' # Top 10 senators by attract score in the 117th Congress
#' rank_members(117, "senate")
#'
#' # Offer scores instead
#' rank_members(117, "senate", score_type = "offer")
#'
#' # House members instead
#' rank_members(117, "house", score_type = "attract")
#'
#' # Republicans only
#' rank_members(117, "senate", score_type = "attract", party = "R")
#'
#' # Democrats only
#' rank_members(117, "senate", score_type = "attract", party = "D")
#'
#' # Top 5 only
#' rank_members(117, "senate", score_type = "attract", n = 5)
#'
#' # Rank by health-policy attract score
#' rank_members(117, "senate", score_type = "attract", issue = "health")
#'
#' # Full ranking, lowest scores first
#' rank_members(117, "senate", score_type = "attract",
#'              n = Inf, descending = FALSE)
#'
#' @seealso [get_member_scores()], [rank_members_by_issue()], [get_issue_scores()]
#' @export
rank_members <- function(congress,
                         chamber,
                         score_type = c("attract", "offer"),
                         party = NULL,
                         issue = NULL,
                         n = 10,
                         descending = TRUE) {
  score_type <- match.arg(score_type)
  if (length(congress) != 1 || !is.numeric(congress)) {
    stop("`congress` must be a single integer.", call. = FALSE)
  }
  chamber_norm <- .normalize_chamber(chamber)
  if (length(chamber_norm) != 1) {
    stop("`chamber` must be a single value ('house' or 'senate').",
         call. = FALSE)
  }
  party_norm <- .normalize_party(party)

  if (is.null(issue)) {
    df <- .require_data("aggregate.cbs")
    score_col <- if (score_type == "attract") "attract_aggregate_weighted" else "offer_aggregate_weighted"
  } else {
    df <- .require_data("issue.area.cbs")
    short <- .resolve_issue(issue)
    score_col <- if (score_type == "attract") {
      paste0("attract_", short, "_weighted")
    } else {
      paste0("offer_", short, "_weighted")
    }
    if (!score_col %in% names(df)) {
      stop("Column '", score_col, "' not found in issue.area.cbs.",
           call. = FALSE)
    }
  }

  sub <- df[df$congress == congress & df$chamber == chamber_norm, , drop = FALSE]
  if (!is.null(party_norm)) {
    sub <- sub[sub$party == party_norm, , drop = FALSE]
  }
  sub <- sub[!is.na(sub[[score_col]]), , drop = FALSE]

  if (nrow(sub) == 0) {
    stop("No members match the supplied congress/chamber/party filters.",
         call. = FALSE)
  }

  ord <- order(sub[[score_col]], decreasing = isTRUE(descending))
  sub <- sub[ord, , drop = FALSE]

  if (is.finite(n)) {
    sub <- utils::head(sub, n = n)
  }

  out <- data.frame(
    rank        = seq_len(nrow(sub)),
    name        = sub$name,
    party       = sub$party,
    bioguide_id = sub$bioguide_id,
    stringsAsFactors = FALSE
  )
  out[[score_col]] <- sub[[score_col]]
  tibble::as_tibble(out)
}
