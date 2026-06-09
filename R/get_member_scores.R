#' Look up a member's Congressional Bipartisanship Scores
#'
#' Returns a member's score history across Congresses. The identifier can be
#' a Bioguide ID (exact match, e.g. \code{"P000449"}) or a name string
#' (case-insensitive substring match against the \code{name} column). When
#' \code{include_issues = TRUE}, the issue-area scores are joined on as
#' additional columns.
#'
#' @param identifier Character. Either a Bioguide ID (exact match, e.g.
#'   \code{"P000449"}) or a name substring (case-insensitive, e.g.
#'   \code{"portman"} or \code{"rob portman"}).
#' @param congress Optional integer vector. Restrict to these Congress
#'   numbers.
#' @param chamber Optional character. \code{"house"} or \code{"senate"}.
#' @param include_issues Logical. If \code{TRUE}, attach issue-area scores.
#'   Default \code{FALSE}.
#'
#' @return A tibble with one row per member-Congress-chamber observation.
#'   Errors if no matching member is found; issues a warning and returns
#'   all matches if the name substring is ambiguous.
#'
#' @examples
#' get_member_scores("P000449")                     # by Bioguide ID
#' get_member_scores("portman")                     # same via name substring
#' get_member_scores("portman", congress = 117)     # restrict to one Congress
#' get_member_scores("P000449", congress = 115:117) # range of Congresses
#' get_member_scores("P000449", chamber = "senate") # restrict to chamber
#' get_member_scores("P000449", congress = 117, chamber = "senate",
#'                   include_issues = TRUE)          # full call with issue scores
#'
#' @seealso [get_member_trend()], [compare_members()], [rank_members()]
#' @export
get_member_scores <- function(identifier,
                               congress = NULL,
                               chamber = NULL,
                               include_issues = FALSE) {
  if (length(identifier) != 1 || !is.character(identifier)) {
    stop("`identifier` must be a single character string.", call. = FALSE)
  }

  gen <- .require_data("aggregate.cbs")
  chamber_norm <- .normalize_chamber(chamber)

  is_bioguide <- grepl("^[A-Za-z][0-9]{6}$", identifier)

  if (is_bioguide) {
    hits <- gen[toupper(gen$bioguide_id) == toupper(identifier), , drop = FALSE]
  } else {
    pattern <- tolower(identifier)
    hits <- gen[grepl(pattern, tolower(gen$name), fixed = TRUE), , drop = FALSE]
  }

  if (nrow(hits) == 0) {
    stop("No member matches '", identifier, "'.", call. = FALSE)
  }

  if (!is_bioguide) {
    distinct_ids <- unique(hits$bioguide_id)
    if (length(distinct_ids) > 1) {
      warning("Name '", identifier, "' matched ", length(distinct_ids),
              " distinct members. Returning all of them. Use a Bioguide ID ",
              "or a more specific name to narrow down. Matched IDs: ",
              paste(distinct_ids, collapse = ", "),
              call. = FALSE)
    }
  }

  if (!is.null(congress)) {
    hits <- hits[hits$congress %in% congress, , drop = FALSE]
  }
  if (!is.null(chamber_norm)) {
    hits <- hits[hits$chamber %in% chamber_norm, , drop = FALSE]
  }

  if (nrow(hits) == 0) {
    stop("Member found, but no rows remain after applying congress/chamber ",
         "filters.", call. = FALSE)
  }

  hits <- hits[order(hits$congress, hits$chamber), , drop = FALSE]

  if (isTRUE(include_issues)) {
    iss <- .require_data("issue.area.cbs")
    id_cols    <- c("congress", "chamber", "bioguide_id", "name", "party")
    xwalk_cols <- c("state", "district", "thomas_id", "icpsr_id",
                    "govtrack_id", "wikipedia", "wikidata")
    score_cols <- setdiff(names(iss), c(id_cols, xwalk_cols))
    iss_scores <- iss[, c(id_cols, score_cols), drop = FALSE]
    hits <- merge(hits, iss_scores, by = id_cols, all.x = TRUE, sort = FALSE)
    hits <- hits[order(hits$congress, hits$chamber), , drop = FALSE]
  }

  tibble::as_tibble(hits)
}
