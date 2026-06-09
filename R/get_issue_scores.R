#' Get all members' Congressional Bipartisanship Scores for a single issue area
#'
#' Returns a tidy data frame of attract and offer scores for a single CRS
#' policy area. Useful for issue-level analysis and plotting.
#'
#' @param issue Character. A short issue label (e.g., \code{"health"}),
#'   full CRS policy-area name (e.g., \code{"crime and law enforcement"}),
#'   or title-cased display label (e.g., \code{"Crime and Law Enforcement"}).
#'   See [list_issues()] for valid values.
#' @param congress Optional integer vector. Filter to these Congresses.
#' @param chamber Optional character. \code{"house"} or \code{"senate"}.
#' @param party Optional character. \code{"D"} or \code{"R"}.
#' @param drop_na Logical. If \code{TRUE} (default), drop rows where both
#'   attract and offer scores are missing for this issue.
#'
#' @return A tibble with one row per member-Congress-chamber observation
#'   and columns \code{congress}, \code{chamber}, \code{bioguide_id},
#'   \code{name}, \code{party}, \code{issue}, \code{attract}, and
#'   \code{offer} (weighted scores).
#'
#' @examples
#' # All members' taxation scores across all Congresses
#' get_issue_scores("tax")
#'
#' # Restrict to a single Congress
#' get_issue_scores("tax", congress = 117)
#'
#' # Range of Congresses
#' get_issue_scores("tax", congress = 115:117)
#'
#' # Senate only
#' get_issue_scores("tax", congress = 117, chamber = "senate")
#'
#' # Democrats only
#' get_issue_scores("tax", congress = 117, chamber = "senate", party = "D")
#'
#' # Republicans only
#' get_issue_scores("tax", congress = 117, chamber = "senate", party = "R")
#'
#' # Keep rows where both scores are missing (drop_na = FALSE)
#' get_issue_scores("tax", congress = 117, drop_na = FALSE)
#'
#' # Also accepts full CRS name or title-cased display label
#' get_issue_scores("Taxation", congress = 117, chamber = "senate",
#'                  party = "D", drop_na = TRUE)
#'
#' @seealso [list_issues()], [rank_members_by_issue()], [plot_bipartisanship()]
#' @export
get_issue_scores <- function(issue,
                             congress = NULL,
                             chamber  = NULL,
                             party    = NULL,
                             drop_na  = TRUE) {
  short        <- .resolve_issue(issue)
  chamber_norm <- .normalize_chamber(chamber)
  party_norm   <- .normalize_party(party)

  df <- .require_data("issue.area.cbs")

  attract_col <- paste0("attract_", short, "_weighted")
  offer_col   <- paste0("offer_",   short, "_weighted")

  if (!all(c(attract_col, offer_col) %in% names(df))) {
    stop("Expected columns ", attract_col, " / ", offer_col,
         " were not found in issue.area.cbs.", call. = FALSE)
  }

  keep <- df[, c("congress", "chamber", "bioguide_id", "name", "party",
                 attract_col, offer_col)]

  names(keep)[names(keep) == attract_col] <- "attract"
  names(keep)[names(keep) == offer_col]   <- "offer"
  keep$issue <- short

  if (!is.null(congress))     keep <- keep[keep$congress %in% congress,     , drop = FALSE]
  if (!is.null(chamber_norm)) keep <- keep[keep$chamber  %in% chamber_norm, , drop = FALSE]
  if (!is.null(party_norm))   keep <- keep[keep$party    %in% party_norm,   , drop = FALSE]

  if (isTRUE(drop_na)) {
    keep <- keep[!(is.na(keep$attract) & is.na(keep$offer)), , drop = FALSE]
  }

  keep <- keep[, c("congress", "chamber", "bioguide_id", "name", "party",
                   "issue", "attract", "offer")]

  tibble::as_tibble(keep)
}


#' List available issue-area labels
#'
#' Returns the crosswalk between short labels (used in column names and in
#' [get_issue_scores()] / [rank_members()] / [plot_bipartisanship()]) and
#' human-readable CRS policy-area names.
#'
#' @return A tibble with columns \code{policy_area_number}, \code{policy_area},
#'   \code{topic_short}, and \code{topic_label}.
#'
#' @examples
#' list_issues()
#'
#' @seealso [get_issue_scores()], [rank_members_by_issue()]
#' @export
list_issues <- function() {
  tibble::as_tibble(.require_data("issue.labels"))
}
