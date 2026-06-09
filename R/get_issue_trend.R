#' Issue-area Congressional Bipartisanship Score trends over time
#'
#' Returns mean attract and offer scores for a single CRS policy area across
#' all available Congresses, optionally broken down by party.
#'
#' @param issue Character. A short issue label (e.g., \code{"health"}),
#'   full CRS policy-area name, or title-cased display label.
#'   See [list_issues()] for valid values.
#' @param chamber Optional character. \code{"house"} or \code{"senate"}.
#' @param party Optional character. \code{"D"} or \code{"R"}.
#' @param by_party Logical. If \code{TRUE}, compute separate means per party
#'   within each Congress x chamber cell. Default \code{FALSE}.
#'
#' @return A tibble with columns \code{congress}, \code{chamber}, optionally
#'   \code{party}, \code{issue}, \code{mean_attract}, \code{mean_offer}, and
#'   \code{n_members}.
#'
#' @examples
#' # Immigration bipartisanship across all Congresses, both chambers
#' get_issue_trend("immig")
#'
#' # Senate only
#' get_issue_trend("immig", chamber = "senate")
#'
#' # Split by party
#' get_issue_trend("immig", chamber = "senate", by_party = TRUE)
#'
#' # Republicans only
#' get_issue_trend("immig", chamber = "senate", party = "R")
#'
#' # Democrats only
#' get_issue_trend("immig", chamber = "senate", party = "D")
#'
#' # Also accepts full CRS name or title-cased display label
#' get_issue_trend("Immigration", chamber = "senate", by_party = TRUE)
#'
#' @seealso [get_issue_scores()], [get_congress_summary()], [plot_trend()]
#' @export
get_issue_trend <- function(issue,
                             chamber  = NULL,
                             party    = NULL,
                             by_party = FALSE) {
  short        <- .resolve_issue(issue)
  chamber_norm <- .normalize_chamber(chamber)
  party_norm   <- .normalize_party(party)

  df <- .require_data("issue.area.cbs")

  attract_col <- paste0("attract_", short, "_weighted")
  offer_col   <- paste0("offer_",   short, "_weighted")

  if (!all(c(attract_col, offer_col) %in% names(df))) {
    stop("Issue columns for '", short, "' not found in issue.area.cbs.",
         call. = FALSE)
  }

  if (!is.null(chamber_norm)) df <- df[df$chamber %in% chamber_norm, , drop = FALSE]
  if (!is.null(party_norm))   df <- df[df$party   %in% party_norm,   , drop = FALSE]

  tbl <- tibble::as_tibble(df)
  tbl$attract_score <- tbl[[attract_col]]
  tbl$offer_score   <- tbl[[offer_col]]

  if (isTRUE(by_party)) {
    grp <- dplyr::group_by(tbl, .data$congress, .data$chamber, .data$party)
  } else {
    grp <- dplyr::group_by(tbl, .data$congress, .data$chamber)
  }

  out <- dplyr::summarise(
    grp,
    issue        = short,
    mean_attract = mean(.data$attract_score, na.rm = TRUE),
    mean_offer   = mean(.data$offer_score,   na.rm = TRUE),
    n_members    = sum(!is.na(.data$attract_score) | !is.na(.data$offer_score)),
    .groups      = "drop"
  )

  out[order(out$congress, out$chamber), , drop = FALSE]
}
