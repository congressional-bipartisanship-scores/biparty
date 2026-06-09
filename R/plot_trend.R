#' Plot Congressional Bipartisanship Score trends over time
#'
#' Generates a time-series line chart of attract and/or offer scores across
#' Congresses. Supports two modes:
#'
#' \describe{
#'   \item{\code{"member"}}{Traces one or more members' career trajectories.}
#'   \item{\code{"party"}}{Plots the party-average trend from
#'     [get_congress_summary()].}
#' }
#'
#' @param identifier Character vector. For \code{type = "member"}: Bioguide
#'   IDs or name substrings. For \code{type = "party"}: \code{"D"} and/or
#'   \code{"R"}.
#' @param type Character. \code{"member"} (default) or \code{"party"}.
#' @param chamber Optional character. \code{"house"} or \code{"senate"}.
#' @param score_type Character. \code{"attract"}, \code{"offer"}, or
#'   \code{"both"} (default).
#' @param title Optional character. Plot title.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' # Charles Grassley's full career — both scores
#' plot_trend("G000386")
#'
#' # Attract scores only
#' plot_trend("G000386", score_type = "attract")
#'
#' # Restrict to Senate career
#' plot_trend("G000386", chamber = "senate")
#'
#' # Compare two members
#' plot_trend(c("G000386", "H000874"), chamber = "senate")
#'
#' # Party-average trend, both parties
#' plot_trend(c("D", "R"), type = "party", chamber = "senate")
#'
#' # Party-average trend, attract only
#' plot_trend(c("D", "R"), type = "party", chamber = "senate",
#'            score_type = "attract")
#'
#' @seealso [get_member_trend()], [get_congress_summary()], [compare_members()]
#' @export
plot_trend <- function(identifier,
                        type       = c("member", "party"),
                        chamber    = NULL,
                        score_type = c("both", "attract", "offer"),
                        title      = NULL) {
  type       <- match.arg(type)
  score_type <- match.arg(score_type)

  attract_col <- "attract_aggregate_weighted"
  offer_col   <- "offer_aggregate_weighted"

  if (type == "member") {

    rows <- lapply(identifier, function(id) {
      tryCatch(
        get_member_trend(id, chamber = chamber, score_type = score_type),
        error = function(e) {
          warning("Could not retrieve '", id, "': ", conditionMessage(e),
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
    df$display_name <- tools::toTitleCase(tolower(df$name))

    display_names <- unique(df$display_name)
    if (is.null(title)) {
      title <- paste0(
        paste(display_names, collapse = " vs. "),
        " \u2014 Bipartisanship Over Time"
      )
    }

    score_cols   <- intersect(c(attract_col, offer_col), names(df))
    multi_member <- length(display_names) > 1L

  } else {
    party_norm <- .normalize_party(identifier)
    summary_df <- get_congress_summary(chamber = chamber, by_party = TRUE)
    df         <- summary_df[summary_df$party %in% party_norm, , drop = FALSE]

    if (nrow(df) == 0L) {
      stop("No party summary rows for the supplied party/chamber filters.",
           call. = FALSE)
    }

    names(df)[names(df) == "mean_attract"] <- attract_col
    names(df)[names(df) == "mean_offer"]   <- offer_col

    df$display_name <- paste0(
      ifelse(df$party == "D", "Democrats", "Republicans"),
      " (", tools::toTitleCase(df$chamber), ")"
    )

    score_cols   <- intersect(c(attract_col, offer_col), names(df))
    multi_member <- length(unique(df$display_name)) > 1L

    if (is.null(title)) {
      party_labels <- paste(
        ifelse(party_norm == "D", "Democrats", "Republicans"),
        collapse = " & "
      )
      title <- paste0(party_labels, " \u2014 Bipartisanship Over Time")
    }
  }

  if (score_type == "attract") score_cols <- intersect(attract_col, score_cols)
  if (score_type == "offer")   score_cols <- intersect(offer_col,   score_cols)
  if (length(score_cols) == 0L) {
    stop("No score columns available for the requested score_type.", call. = FALSE)
  }
  score_cols <- intersect(score_cols, names(df))

  long <- tidyr::pivot_longer(
    tibble::as_tibble(df),
    cols      = dplyr::all_of(score_cols),
    names_to  = "score_type",
    values_to = "score"
  )
  long$score_label <- ifelse(
    long$score_type == attract_col, "Attract", "Offer"
  )
  multi_score <- length(score_cols) > 1L

  .party_colors_for <- function(names_vec, party_vec) {
    d_names <- unique(names_vec[party_vec == "D"])
    r_names <- unique(names_vec[party_vec == "R"])
    d_cols  <- if (length(d_names) == 1L) {
      setNames("#2166ac", d_names)
    } else {
      setNames(
        grDevices::colorRampPalette(c("#2166ac", "#9ecae1"))(length(d_names)),
        d_names
      )
    }
    r_cols <- if (length(r_names) == 1L) {
      setNames("#d6604d", r_names)
    } else {
      setNames(
        grDevices::colorRampPalette(c("#d6604d", "#fc8d59"))(length(r_names)),
        r_names
      )
    }
    c(d_cols, r_cols)
  }

  score_colors <- c("Attract" = "#1b7837", "Offer" = "#762a83")
  lt_map       <- c("Attract" = "solid", "Offer" = "dashed")

  if (multi_member && multi_score) {
    if (type == "party" && "party" %in% names(long)) {
      color_vals <- .party_colors_for(long$display_name, long$party)
    } else {
      color_vals <- NULL
    }
    p <- ggplot2::ggplot(long,
      ggplot2::aes(
        x        = .data$congress,
        y        = .data$score,
        color    = .data$display_name,
        linetype = .data$score_label,
        group    = interaction(.data$display_name, .data$score_label)
      )
    ) +
      ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
      ggplot2::geom_point(size = 2.2, na.rm = TRUE) +
      ggplot2::scale_linetype_manual(name = "Score", values = lt_map)
    if (!is.null(color_vals)) {
      p <- p + ggplot2::scale_color_manual(name = NULL, values = color_vals)
    } else {
      p <- p + ggplot2::scale_color_discrete(name = NULL)
    }

  } else if (multi_member && !multi_score) {
    if (type == "party" && "party" %in% names(long)) {
      color_vals <- .party_colors_for(long$display_name, long$party)
    } else {
      color_vals <- NULL
    }
    p <- ggplot2::ggplot(long,
      ggplot2::aes(
        x     = .data$congress,
        y     = .data$score,
        color = .data$display_name,
        group = .data$display_name
      )
    ) +
      ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
      ggplot2::geom_point(size = 2.2, na.rm = TRUE)
    if (!is.null(color_vals)) {
      p <- p + ggplot2::scale_color_manual(name = NULL, values = color_vals)
    } else {
      p <- p + ggplot2::scale_color_discrete(name = NULL)
    }

  } else if (!multi_member && multi_score) {
    p <- ggplot2::ggplot(long,
      ggplot2::aes(
        x     = .data$congress,
        y     = .data$score,
        color = .data$score_label,
        group = .data$score_label
      )
    ) +
      ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
      ggplot2::geom_point(size = 2.2, na.rm = TRUE) +
      ggplot2::scale_color_manual(name = "Score", values = score_colors)

  } else {
    fixed_color <- if (score_cols == attract_col) "#1b7837" else "#762a83"
    p <- ggplot2::ggplot(long,
      ggplot2::aes(
        x     = .data$congress,
        y     = .data$score,
        group = .data$display_name
      )
    ) +
      ggplot2::geom_line(linewidth = 0.9, color = fixed_color, na.rm = TRUE) +
      ggplot2::geom_point(size = 2.2, color = fixed_color, na.rm = TRUE)
  }

  p +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom"
    ) +
    ggplot2::labs(
      x     = "Congress",
      y     = "Bipartisanship Score (0\u20131)",
      title = title
    )
}
