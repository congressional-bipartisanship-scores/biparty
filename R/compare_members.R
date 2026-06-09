#' Compare Congressional Bipartisanship Scores for multiple members
#'
#' Returns a single tidy tibble with aggregate bipartisanship scores for two or
#' more members side by side. Each member is looked up via [get_member_scores()],
#' so identifiers can be a mix of Bioguide IDs and name substrings.
#'
#' @param identifiers Character vector of Bioguide IDs or name substrings.
#'   Must contain at least two entries.
#' @param congress Optional integer vector. Restrict to these Congress numbers.
#' @param chamber Optional character. \code{"house"} or \code{"senate"}.
#'   Applied to all members simultaneously. See Note.
#' @param score_type Character. \code{"attract"}, \code{"offer"}, or
#'   \code{"both"} (default). Controls which score columns are returned.
#'
#' @return A tibble with columns \code{congress}, \code{chamber},
#'   \code{bioguide_id}, \code{name}, \code{party}, and the requested score
#'   column(s), sorted by Congress, chamber, and name.
#'
#' @note The \code{chamber} argument is applied globally to all members in the
#'   comparison. For members who served in both chambers within the same
#'   Congress, this allows you to isolate scores from a single chamber for all
#'   members being compared. However, it is not possible to compare one
#'   member's House scores against another member's Senate scores in a single
#'   call. For that use case, call [get_member_scores()] separately for each
#'   member with the desired \code{chamber} argument and combine the results.
#'
#' @examples
#' # Elissa Slotkin (D) and Susan Collins (R) — full history
#' compare_members(c("S001208", "C001035"))
#'
#' # Restrict to a single Congress
#' compare_members(c("S001208", "C001035"), congress = 117)
#'
#' # Range of Congresses
#' compare_members(c("S001208", "C001035"), congress = 116:117)
#'
#' # Attract scores only
#' compare_members(c("S001208", "C001035"), congress = 117,
#'                 score_type = "attract")
#'
#' # Offer scores only
#' compare_members(c("S001208", "C001035"), congress = 117,
#'                 score_type = "offer")
#'
#' # Restrict all members to Senate chamber
#' compare_members(c("S001208", "C001035"), congress = 117,
#'                 chamber = "senate", score_type = "both")
#'
#' @seealso [get_member_scores()], [rank_members()], [plot_trend()]
#' @export
compare_members <- function(identifiers,
                             congress   = NULL,
                             chamber    = NULL,
                             score_type = c("both", "attract", "offer")) {
  score_type <- match.arg(score_type)

  if (!is.character(identifiers) || length(identifiers) < 2) {
    stop("`identifiers` must be a character vector with at least two entries.",
         call. = FALSE)
  }

  rows <- lapply(identifiers, function(id) {
    tryCatch(
      get_member_scores(id, congress = congress, chamber = chamber),
      error = function(e) {
        warning("Could not retrieve member '", id, "': ", conditionMessage(e),
                call. = FALSE)
        NULL
      }
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1L))]

  if (length(rows) == 0L) {
    stop("No members could be retrieved from the supplied identifiers.",
         call. = FALSE)
  }

  df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))

  keep_cols <- c("congress", "chamber", "bioguide_id", "name", "party")
  if (score_type %in% c("attract", "both")) keep_cols <- c(keep_cols, "attract_aggregate_weighted")
  if (score_type %in% c("offer",   "both")) keep_cols <- c(keep_cols, "offer_aggregate_weighted")
  keep_cols <- intersect(keep_cols, names(df))

  df <- df[, keep_cols, drop = FALSE]
  df <- df[order(df$congress, df$chamber, df$name), , drop = FALSE]
  tibble::as_tibble(df)
}
