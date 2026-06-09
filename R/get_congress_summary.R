#' Congress-level summary of Congressional Bipartisanship Scores
#'
#' Computes mean attract and offer scores aggregated to the Congress x chamber
#' level, with optional subsetting by party, Congress, and issue area.
#'
#' When \code{issue} is supplied the summary is computed from [issue.area.cbs]
#' scores for that policy area; otherwise [aggregate.cbs] scores are used.
#'
#' @param chamber Optional character. \code{"house"} or \code{"senate"}.
#' @param party Optional character. \code{"D"} or \code{"R"}.
#' @param congress Optional integer vector. Restrict to these Congress numbers.
#' @param by_party Logical. If \code{TRUE}, split means by party. Default
#'   \code{FALSE}.
#' @param issue Optional character. Short issue label, full CRS name, or
#'   title-cased display label. See [list_issues()].
#'
#' @return A tibble with columns \code{congress}, \code{chamber}, optionally
#'   \code{party}, optionally \code{issue}, \code{mean_attract},
#'   \code{mean_offer}, and \code{n_members}.
#'
#' @examples
#' # All Congresses, both chambers
#' get_congress_summary()
#'
#' # Senate only
#' get_congress_summary(chamber = "senate")
#'
#' # Restrict to a single Congress
#' get_congress_summary(chamber = "senate", congress = 117)
#'
#' # Range of Congresses
#' get_congress_summary(chamber = "senate", congress = 115:117)
#'
#' # Split by party
#' get_congress_summary(chamber = "senate", congress = 115:117,
#'                      by_party = TRUE)
#'
#' # Republicans only
#' get_congress_summary(chamber = "senate", congress = 117, party = "R")
#'
#' # By issue area
#' get_congress_summary(chamber = "senate", congress = 117,
#'                      issue = "health")
#'
#' # Full call
#' get_congress_summary(chamber = "senate", congress = 115:117,
#'                      by_party = TRUE, issue = "health")
#'
#' @seealso [get_issue_trend()], [plot_trend()]
#' @export
get_congress_summary <- function(chamber  = NULL,
                                  party    = NULL,
                                  congress = NULL,
                                  by_party = FALSE,
                                  issue    = NULL) {
  chamber_norm <- .normalize_chamber(chamber)
  party_norm   <- .normalize_party(party)

  if (!is.null(issue)) {
    short       <- .resolve_issue(issue)
    df          <- .require_data("issue.area.cbs")
    attract_col <- paste0("attract_", short, "_weighted")
    offer_col   <- paste0("offer_",   short, "_weighted")
    if (!all(c(attract_col, offer_col) %in% names(df))) {
      stop("Issue columns for '", short, "' not found in issue.area.cbs.",
           call. = FALSE)
    }
    df$attract_aggregate_weighted <- df[[attract_col]]
    df$offer_aggregate_weighted   <- df[[offer_col]]
  } else {
    df    <- .require_data("aggregate.cbs")
    short <- NULL
  }

  if (!is.null(chamber_norm)) df <- df[df$chamber  %in% chamber_norm, , drop = FALSE]
  if (!is.null(party_norm))   df <- df[df$party    %in% party_norm,   , drop = FALSE]
  if (!is.null(congress))     df <- df[df$congress %in% congress,     , drop = FALSE]

  if (nrow(df) == 0L) {
    stop("No rows remain after applying filters.", call. = FALSE)
  }

  tbl <- tibble::as_tibble(df)

  grp <- if (isTRUE(by_party)) {
    dplyr::group_by(tbl, .data$congress, .data$chamber, .data$party)
  } else {
    dplyr::group_by(tbl, .data$congress, .data$chamber)
  }

  out <- dplyr::summarise(
    grp,
    mean_attract = mean(.data$attract_aggregate_weighted, na.rm = TRUE),
    mean_offer   = mean(.data$offer_aggregate_weighted,   na.rm = TRUE),
    n_members    = dplyr::n(),
    .groups      = "drop"
  )

  if (!is.null(short)) out$issue <- short

  out[order(out$congress, out$chamber), , drop = FALSE]
}
