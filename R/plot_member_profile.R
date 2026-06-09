#' Plot a member's Congressional Bipartisanship Score profile across all issue areas
#'
#' Creates a grouped bar chart of attract and offer scores across all CRS policy
#' areas for a given Congress. When the \pkg{magick} and \pkg{patchwork} packages
#' are installed, the output is a rich profile card with a photo, info card,
#' and bar chart.
#'
#' @param identifier Character. Bioguide ID or name substring.
#' @param congress Integer. Congress to display. Defaults to the member's
#'   most recent Congress.
#' @param chamber Optional character. \code{"house"} or \code{"senate"}.
#' @param drop_issues Character vector of short issue labels to exclude.
#' @param show_photo Logical. Attempt to fetch congressional headshot.
#'   Requires \pkg{magick}. Default \code{TRUE}.
#' @param title Optional character. Figure title.
#'
#' @return A \code{ggplot} object or \pkg{patchwork} composite.
#'
#' @examples
#' # Steny Hoyer's most recent Congress profile
#' plot_member_profile("H000874")
#'
#' # Specific early Congress
#' plot_member_profile("H000874", congress = 100)
#'
#' # House chamber only
#' plot_member_profile("H000874", congress = 100, chamber = "house")
#'
#' # Without photo
#' plot_member_profile("H000874", congress = 100, show_photo = FALSE)
#'
#' # Full call with custom title
#' plot_member_profile("H000874", congress = 100, chamber = "house",
#'                     show_photo = TRUE,
#'                     title = "Steny Hoyer - 100th Congress Profile")
#'
#' @seealso [get_member_scores()], [get_issue_scores()], [plot_bipartisanship()]
#' @export
plot_member_profile <- function(identifier,
                                congress    = NULL,
                                chamber     = NULL,
                                drop_issues = c("commem", "history", "na",
                                                "private"),
                                show_photo  = TRUE,
                                title       = NULL) {
  
  df_all <- get_member_scores(identifier, chamber = chamber)
  
  if (is.null(congress)) {
    congress <- max(df_all$congress, na.rm = TRUE)
  }
  
  df_issue <- get_member_scores(identifier,
                                congress       = congress,
                                chamber        = chamber,
                                include_issues = TRUE)
  if (nrow(df_issue) == 0L) {
    stop("No data for Congress ", congress, " after applying filters.",
         call. = FALSE)
  }
  if (nrow(df_issue) > 1L) {
    warning("Multiple rows for Congress ", congress,
            "; using chamber '", df_issue$chamber[1L], "'. ",
            "Supply `chamber` to disambiguate.", call. = FALSE)
    df_issue <- df_issue[1L, , drop = FALSE]
  }
  
  bioguide_id  <- df_issue$bioguide_id[1L]
  member_name  <- df_issue$name[1L]
  member_party <- df_issue$party[1L]
  member_cham  <- df_issue$chamber[1L]
  
  state <- if ("state" %in% names(df_issue)) df_issue$state[1L] else NA_character_
  
  cong_min   <- min(df_all$congress, na.rm = TRUE)
  cong_max   <- max(df_all$congress, na.rm = TRUE)
  cong_range <- if (cong_min == cong_max) {
    paste0(cong_min, ordinal_suffix(cong_min), " Congress")
  } else {
    paste0(cong_min, ordinal_suffix(cong_min), "\u2013",
           cong_max, ordinal_suffix(cong_max), " Congresses")
  }
  
  mean_attract <- mean(df_all$attract_aggregate_weighted, na.rm = TRUE)
  mean_offer   <- mean(df_all$offer_aggregate_weighted,   na.rm = TRUE)
  
  labels_df  <- .require_data("issue.labels")
  col_suffix <- "_weighted"
  
  # Build attract scores
  attract_cols <- names(df_issue)[
    startsWith(names(df_issue), "attract_") &
      endsWith(names(df_issue), col_suffix) &
      !grepl("aggregate", names(df_issue))
  ]
  attract_shorts <- sub("^attract_(.+)_weighted$", "\\1", attract_cols)
  attract_df <- data.frame(
    topic_short = attract_shorts,
    attract     = as.numeric(df_issue[1L, attract_cols]),
    stringsAsFactors = FALSE
  )
  
  # Build offer scores
  offer_cols <- names(df_issue)[
    startsWith(names(df_issue), "offer_") &
      endsWith(names(df_issue), col_suffix) &
      !grepl("aggregate", names(df_issue))
  ]
  offer_shorts <- sub("^offer_(.+)_weighted$", "\\1", offer_cols)
  offer_df <- data.frame(
    topic_short = offer_shorts,
    offer       = as.numeric(df_issue[1L, offer_cols]),
    stringsAsFactors = FALSE
  )
  
  # Merge and clean
  issue_df <- merge(attract_df, offer_df, by = "topic_short", all = TRUE)
  issue_df <- issue_df[!issue_df$topic_short %in% drop_issues, , drop = FALSE]
  issue_df <- merge(
    issue_df,
    labels_df[, c("topic_short", "topic_label", "policy_area_number")],
    by    = "topic_short",
    all.x = TRUE,
    sort  = FALSE
  )
  issue_df <- issue_df[order(issue_df$policy_area_number), , drop = FALSE]
  issue_df$topic_label <- factor(issue_df$topic_label, levels = issue_df$topic_label)
  
  # Top 3 for attract and offer separately
  scored_attract   <- issue_df[!is.na(issue_df$attract), , drop = FALSE]
  scored_attract   <- scored_attract[order(scored_attract$attract, decreasing = TRUE), , drop = FALSE]
  top3_attract     <- utils::head(as.character(scored_attract$topic_label), 3L)
  
  scored_offer     <- issue_df[!is.na(issue_df$offer), , drop = FALSE]
  scored_offer     <- scored_offer[order(scored_offer$offer, decreasing = TRUE), , drop = FALSE]
  top3_offer       <- utils::head(as.character(scored_offer$topic_label), 3L)
  
  # Party colors matching paper Figure 3
  if (member_party == "D") {
    color_attract <- "#2166ac"
    color_offer   <- "#86b5d0"
    party_color   <- "#2166ac"
  } else {
    color_attract <- "#A04338"
    color_offer   <- "#c68e88"
    party_color   <- "#A04338"
  }
  party_full  <- if (member_party == "D") "Democrat" else "Republican"
  name_pretty <- tools::toTitleCase(tolower(member_name))
  state_str   <- if (!is.na(state)) state else ""
  
  # Pivot to long for dodged bars
  issue_long <- tidyr::pivot_longer(
    issue_df,
    cols      = c("attract", "offer"),
    names_to  = "score_type",
    values_to = "score"
  )
  issue_long$score_type <- factor(issue_long$score_type,
                                  levels = c("attract", "offer"))
  
  bar_chart <- ggplot2::ggplot(issue_long,
                               ggplot2::aes(x = .data$topic_label, y = .data$score,
                                            fill = .data$score_type)) +
    ggplot2::geom_col(position = "dodge", width = 0.7, na.rm = TRUE) +
    ggplot2::scale_fill_manual(
      name   = NULL,
      values = c("attract" = color_attract, "offer" = color_offer),
      labels = c("attract" = "Attract", "offer" = "Offer")
    ) +
    ggplot2::scale_y_continuous(
      limits = c(0, 1),
      expand = ggplot2::expansion(mult = c(0, 0.02))
    ) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(angle = 90, hjust = 1,
                                                 vjust = 0.5),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position    = "bottom",
      plot.subtitle      = ggplot2::element_text(face = "bold", hjust = 0)
    ) +
    ggplot2::labs(
      x        = "Issue Area",
      y        = "Bipartisanship Score",
      subtitle = paste0(
        "Bipartisanship Scores by CRS Policy Area \u2013 ",
        as.integer(congress), ordinal_suffix(as.integer(congress)),
        " Congress (", tools::toTitleCase(tolower(member_cham)), ")"
      )
    )
  
  has_magick    <- requireNamespace("magick",    quietly = TRUE)
  has_patchwork <- requireNamespace("patchwork", quietly = TRUE)
  
  if (!has_patchwork) {
    message(
      "Install the 'patchwork' package for the full profile-card layout:\n",
      "  install.packages(c('magick', 'patchwork'))"
    )
    return(bar_chart)
  }
  
  photo_panel <- NULL
  if (isTRUE(show_photo) && has_magick) {
    photo_url <- paste0(
      "https://raw.githubusercontent.com/unitedstates/images/gh-pages/congress/225x275/",
      bioguide_id, ".jpg"
    )
    img <- tryCatch(
      withCallingHandlers(
        magick::image_read(photo_url),
        warning = function(w) invokeRestart("muffleWarning")
      ),
      error   = function(e) NULL,
      warning = function(w) NULL
    )
    if (!is.null(img) && length(img) > 0L) {
      photo_panel <- magick::image_ggplot(img, interpolate = TRUE)
    }
  }
  
  if (is.null(photo_panel)) {
    name_parts <- strsplit(name_pretty, "\\s+")[[1L]]
    initials   <- paste0(
      substr(name_parts[1L], 1L, 1L),
      if (length(name_parts) > 1L) substr(name_parts[length(name_parts)], 1L, 1L) else ""
    )
    photo_panel <- ggplot2::ggplot() +
      ggplot2::annotate("point", x = 0.5, y = 0.5,
                        size = 35, color = party_color, alpha = 0.12) +
      ggplot2::annotate("text", x = 0.5, y = 0.5,
                        label = initials, size = 16,
                        color = party_color, fontface = "bold") +
      ggplot2::theme_void() +
      ggplot2::coord_fixed() +
      ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
  }
  
  top3_attract_str <- if (length(top3_attract) > 0L) paste(top3_attract, collapse = ", ") else "insufficient data"
  top3_offer_str   <- if (length(top3_offer)   > 0L) paste(top3_offer,   collapse = ", ") else "insufficient data"
  
  info_panel <- ggplot2::ggplot() +
    ggplot2::theme_void(base_size = 12) +
    ggplot2::xlim(0, 1) +
    ggplot2::ylim(-0.05, 1) +
    # Name
    ggplot2::annotate("text", x = 0.03, y = 0.95,
                      label = name_pretty,
                      hjust = 0, size = 5.5, fontface = "bold") +
    # Party · State · Chamber
    ggplot2::annotate("text", x = 0.03, y = 0.82,
                      label = paste0(party_full, "  \u00b7  ", state_str,
                                     "  \u00b7  ", tools::toTitleCase(tolower(member_cham))),
                      hjust = 0, size = 3.8, color = "grey30") +
    # Served
    ggplot2::annotate("text", x = 0.03, y = 0.70,
                      label = paste0("Served: ", cong_range),
                      hjust = 0, size = 3.5, color = "grey40") +
    # Divider
    ggplot2::annotate("segment", x = 0.03, xend = 0.97,
                      y = 0.60, yend = 0.60,
                      color = "grey70", linewidth = 0.4) +
    # Career mean attract — label left, value right
    ggplot2::annotate("text", x = 0.03, y = 0.51,
                      label = "Career Mean Out\u2013Party Support Attracted",
                      hjust = 0, size = 3.5) +
    ggplot2::annotate("text", x = 0.97, y = 0.51,
                      label = sprintf("%.3f", mean_attract),
                      hjust = 1, size = 3.5, fontface = "bold") +
    # Career mean offer — label left, value right
    ggplot2::annotate("text", x = 0.03, y = 0.40,
                      label = "Career Mean Out\u2013Party Support Offered",
                      hjust = 0, size = 3.5) +
    ggplot2::annotate("text", x = 0.97, y = 0.40,
                      label = sprintf("%.3f", mean_offer),
                      hjust = 1, size = 3.5, fontface = "bold") +
    # Divider
    ggplot2::annotate("segment", x = 0.03, xend = 0.97,
                      y = 0.30, yend = 0.30,
                      color = "grey70", linewidth = 0.4) +
    # Top areas attract label
    ggplot2::annotate("text", x = 0.03, y = 0.22,
                      label = "Top areas (Attract)",
                      hjust = 0, size = 3.3, color = "grey40",
                      fontface = "italic") +
    # Top areas attract values
    ggplot2::annotate("text", x = 0.03, y = 0.13,
                      label = top3_attract_str,
                      hjust = 0, size = 3.3, color = "grey20") +
    # Top areas offer label
    ggplot2::annotate("text", x = 0.03, y = 0.04,
                      label = "Top areas (Offer)",
                      hjust = 0, size = 3.3, color = "grey40",
                      fontface = "italic") +
    # Top areas offer values
    ggplot2::annotate("text", x = 0.03, y = -0.04,
                      label = top3_offer_str,
                      hjust = 0, size = 3.3, color = "grey20")
  
  top_row   <- patchwork::wrap_plots(photo_panel, info_panel, widths = c(1, 2.6))
  full_plot <- patchwork::wrap_plots(top_row, bar_chart,
                                     ncol    = 1,
                                     heights = c(1.3, 2.2))
  
  if (!is.null(title)) {
    full_plot <- full_plot +
      patchwork::plot_annotation(
        title = title,
        theme = ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 14, face = "bold", hjust = 0.5,
            margin = ggplot2::margin(b = 8)
          )
        )
      )
  }
  
  full_plot
}