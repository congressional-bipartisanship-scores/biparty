#' Return a member's Congressional Bipartisanship Scores across Congresses
#'
#' A convenience wrapper around [get_member_scores()] that returns a tidy time
#' series of attract and/or offer scores, ordered by Congress. Suitable for
#' passing directly to [plot_trend()] or for custom longitudinal analysis.
#'
#' @param identifier Character. Bioguide ID or name substring
#'   (see [get_member_scores()]).
#' @param chamber Optional character. \code{"house"} or \code{"senate"}.
#' @param score_type Character. \code{"attract"}, \code{"offer"}, or
#'   \code{"both"} (default). Controls which score columns are returned.
#'
#' @return A tibble with columns \code{congress}, \code{chamber},
#'   \code{bioguide_id}, \code{name}, \code{party}, and the requested score
#'   column(s), ordered by Congress.
#'
#' @examples
#' get_member_trend("S001208")                      # full history, both scores
#' get_member_trend("slotkin")                      # same via name substring
#' get_member_trend("S001208", score_type = "attract")  # attract scores only
#' get_member_trend("S001208", score_type = "offer")    # offer scores only
#' get_member_trend("S001208", chamber = "house")       # restrict to chamber
#' get_member_trend("S001208", chamber = "house",
#'                  score_type = "attract")              # full call
#'
#' @seealso [get_member_scores()], [plot_trend()], [compare_members()]
#' @export
get_member_trend <- function(identifier,
                              chamber    = NULL,
                              score_type = c("both", "attract", "offer")) {
  score_type <- match.arg(score_type)

  df <- get_member_scores(identifier, chamber = chamber)

  keep_cols <- c("congress", "chamber", "bioguide_id", "name", "party")
  if (score_type %in% c("attract", "both")) keep_cols <- c(keep_cols, "attract_aggregate_weighted")
  if (score_type %in% c("offer",   "both")) keep_cols <- c(keep_cols, "offer_aggregate_weighted")

  df <- df[, keep_cols, drop = FALSE]
  df <- df[order(df$congress, df$chamber), , drop = FALSE]
  tibble::as_tibble(df)
}
