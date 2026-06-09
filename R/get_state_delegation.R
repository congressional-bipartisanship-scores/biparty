#' Get Congressional Bipartisanship Scores for an entire state delegation
#'
#' Returns bipartisanship scores for all members who served from a given
#' state, optionally filtered by Congress, chamber, and party. State
#' membership is determined from the \code{state} column embedded in
#' [aggregate.cbs].
#'
#' @param state Character. Two-letter state abbreviation (e.g., \code{"TX"},
#'   \code{"CA"}). Case-insensitive.
#' @param congress Optional integer vector. Restrict to these Congresses.
#' @param chamber Optional character. \code{"house"} or \code{"senate"}.
#' @param party Optional character. \code{"D"} or \code{"R"}.
#' @param aggregate Logical. If \code{TRUE}, collapse to a single-row
#'   summary with mean scores. Default \code{FALSE}.
#'
#' @return A tibble of member-level scores (if \code{aggregate = FALSE}) or
#'   a one-row summary tibble with columns \code{state}, \code{n_members},
#'   \code{n_observations}, \code{mean_attract}, and \code{mean_offer}
#'   (if \code{aggregate = TRUE}).
#'
#' @examples
#' # Full Texas delegation across all Congresses
#' get_state_delegation("TX")
#'
#' # Restrict to a single Congress
#' get_state_delegation("TX", congress = 117)
#'
#' # Range of Congresses
#' get_state_delegation("TX", congress = 115:117)
#'
#' # Senate only
#' get_state_delegation("TX", congress = 117, chamber = "senate")
#'
#' # Republicans only
#' get_state_delegation("TX", congress = 117, party = "R")
#'
#' # Democrats only
#' get_state_delegation("TX", congress = 117, party = "D")
#'
#' # Summarize to a single mean score row
#' get_state_delegation("TX", congress = 117, aggregate = TRUE)
#'
#' # Full call
#' get_state_delegation("TX", congress = 117, chamber = "house",
#'                      party = "R", aggregate = TRUE)
#'
#' @seealso [get_member_scores()], [rank_members()], [get_congress_summary()]
#' @export
get_state_delegation <- function(state,
                                  congress  = NULL,
                                  chamber   = NULL,
                                  party     = NULL,
                                  aggregate = FALSE) {
  if (length(state) != 1L || !is.character(state)) {
    stop("`state` must be a single two-letter state abbreviation.",
         call. = FALSE)
  }
  state_norm   <- toupper(trimws(state))
  chamber_norm <- .normalize_chamber(chamber)
  party_norm   <- .normalize_party(party)

  gen <- .require_data("aggregate.cbs")

  df <- gen[gen$state == state_norm, , drop = FALSE]

  if (nrow(df) == 0L) {
    stop("No members found for state '", state_norm, "'.", call. = FALSE)
  }

  if (!is.null(congress))     df <- df[df$congress %in% congress,     , drop = FALSE]
  if (!is.null(chamber_norm)) df <- df[df$chamber  %in% chamber_norm, , drop = FALSE]
  if (!is.null(party_norm))   df <- df[df$party    %in% party_norm,   , drop = FALSE]

  if (nrow(df) == 0L) {
    stop("No rows remain after applying all filters.", call. = FALSE)
  }

  df <- df[order(df$congress, df$chamber, df$name), , drop = FALSE]

  if (isTRUE(aggregate)) {
    grp <- dplyr::group_by(tibble::as_tibble(df), .data$state)
    return(dplyr::summarise(
      grp,
      n_members      = dplyr::n_distinct(.data$bioguide_id),
      n_observations = dplyr::n(),
      mean_attract   = mean(.data$attract_aggregate_weighted, na.rm = TRUE),
      mean_offer     = mean(.data$offer_aggregate_weighted,   na.rm = TRUE),
      .groups        = "drop"
    ))
  }

  tibble::as_tibble(df)
}
