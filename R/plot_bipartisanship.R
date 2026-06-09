#' Plot Congressional Bipartisanship Scores by issue area, party, and chamber
#'
#' Generates violin plots of issue-area scores with party-mean bars overlaid,
#' faceted into a score type (Attracted / Offered) x chamber (House / Senate)
#' grid.
#'
#' @param congress Integer. Congress to plot (default \code{117}).
#' @param chambers Character vector. Subset of \code{c("house", "senate")}.
#' @param directions Character vector. Subset of
#'   \code{c("attract", "offer")}.
#' @param parties Character vector. Subset of \code{c("D", "R")}.
#' @param drop_issues Character vector of short issue labels to exclude.
#'   Defaults to \code{c("commem", "history", "na", "private")}.
#' @param highlight Optional Bioguide ID for a member to overlay as a
#'   labeled dot (e.g., \code{"G000386"} for Sen. Charles Grassley).
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#'
#' @return A \code{ggplot} object.
#'
#' @examples
#' # 100th Congress, both chambers
#' plot_bipartisanship(100)
#'
#' # Senate only
#' plot_bipartisanship(100, chambers = "senate")
#'
#' # Attract scores only
#' plot_bipartisanship(100, chambers = "senate", directions = "attract")
#'
#' # Republicans only
#' plot_bipartisanship(100, chambers = "senate", parties = "R")
#'
#' # Highlight a specific member
#' plot_bipartisanship(100, chambers = "senate", highlight = "G000386")
#'
#' # Custom title
#' plot_bipartisanship(100, chambers = "senate",
#'                     title = "Senate Bipartisanship, 100th Congress")
#'
#' @seealso [get_issue_scores()], [rank_members()], [plot_trend()]
#' @export
plot_bipartisanship <- function(congress = 117,
                                chambers = c("house", "senate"),
                                directions = c("attract", "offer"),
                                parties = c("D", "R"),
                                drop_issues = c("commem", "history", "na",
                                                "private"),
                                highlight = NULL,
                                title = NULL,
                                subtitle = NULL) {
  if (length(congress) != 1 || !is.numeric(congress)) {
    stop("`congress` must be a single integer.", call. = FALSE)
  }
  chambers   <- .normalize_chamber(chambers)
  directions <- tolower(as.character(directions))
  bad_dir    <- setdiff(directions, c("attract", "offer"))
  if (length(bad_dir) > 0) {
    stop("Invalid direction(s): ", paste(shQuote(bad_dir), collapse = ", "),
         ". Use 'attract' and/or 'offer'.", call. = FALSE)
  }
  parties <- .normalize_party(parties)

  iss    <- .require_data("issue.area.cbs")
  labels <- .require_data("issue.labels")

  sub <- iss[iss$congress == congress & iss$chamber %in% chambers &
               iss$party %in% parties, , drop = FALSE]
  if (nrow(sub) == 0) {
    stop("No data for Congress ", congress, " with the supplied filters.",
         call. = FALSE)
  }

  score_cols <- grep("^(attract|offer)_[a-z]+_weighted$", names(sub),
                     value = TRUE)
  long <- tidyr::pivot_longer(
    tibble::as_tibble(sub),
    cols      = dplyr::all_of(score_cols),
    names_to  = c("direction", "topic"),
    names_pattern = "^(attract|offer)_(.+)_weighted$",
    values_to = "score"
  )
  long <- long[long$direction %in% directions, , drop = FALSE]
  long <- long[!long$topic %in% drop_issues, , drop = FALSE]

  long <- merge(long,
                labels[, c("topic_short", "topic_label", "policy_area_number")],
                by.x = "topic", by.y = "topic_short",
                all.x = TRUE, sort = FALSE)
  long <- long[order(long$policy_area_number), , drop = FALSE]

  long$direction <- factor(
    ifelse(long$direction == "attract", "Attracted", "Offered"),
    levels = c("Attracted", "Offered")
  )
  long$chamber <- factor(
    tools::toTitleCase(long$chamber),
    levels = tools::toTitleCase(chambers)
  )
  label_levels <- unique(long$topic_label[order(long$policy_area_number)])
  long$topic_label <- factor(long$topic_label, levels = label_levels)

  violin_dat <- long[!is.na(long$score), , drop = FALSE]
  violin_dat <- dplyr::group_by(violin_dat,
                                .data$direction, .data$chamber,
                                .data$topic_label)
  violin_dat <- dplyr::filter(violin_dat, dplyr::n() >= 2)
  violin_dat <- dplyr::ungroup(violin_dat)

  pm <- dplyr::group_by(long,
                        .data$direction, .data$chamber,
                        .data$topic_label, .data$party)
  party_means <- dplyr::summarise(
    pm,
    mean_score = mean(.data$score, na.rm = TRUE),
    .groups = "drop"
  )
  party_means <- party_means[!is.nan(party_means$mean_score) &
                               !is.na(party_means$mean_score), , drop = FALSE]
  party_means$party_label <- ifelse(party_means$party == "D",
                                    "Democratic Party (mean)",
                                    "Republican Party (mean)")

  highlight_pts   <- NULL
  highlight_label <- NULL
  if (!is.null(highlight)) {
    hh <- long[long$bioguide_id == highlight, , drop = FALSE]
    if (nrow(hh) > 0) {
      gh <- dplyr::group_by(hh, .data$direction, .data$chamber,
                            .data$topic_label)
      highlight_pts <- dplyr::summarise(
        gh,
        mean_score = mean(.data$score, na.rm = TRUE),
        .groups = "drop"
      )
      highlight_pts <- highlight_pts[!is.nan(highlight_pts$mean_score) &
                                       !is.na(highlight_pts$mean_score),
                                     , drop = FALSE]
      highlight_label <- paste0(unique(hh$name)[1], " (mean)")
      if (nrow(highlight_pts) > 0) {
        highlight_pts$label <- highlight_label
      }
    }
  }

  color_values <- c(
    "Democratic Party (mean)" = "blue",
    "Republican Party (mean)" = "red"
  )
  if (!is.null(highlight_label)) {
    color_values[[highlight_label]] <- "black"
  }

  if (is.null(title))    title    <- "Congressional Bipartisanship Scores by Issue Area, Party, and Chamber"
  if (is.null(subtitle)) subtitle <- paste0(
    as.integer(congress), ordinal_suffix(as.integer(congress)),
    " Congress"
  )

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$topic_label,
                                          y = .data$score)) +
    ggplot2::geom_violin(
      data = violin_dat,
      fill = "grey85", color = "grey40", trim = FALSE,
      na.rm = TRUE
    ) +
    ggplot2::geom_errorbar(
      data = party_means,
      ggplot2::aes(
        x    = .data$topic_label,
        ymin = .data$mean_score,
        ymax = .data$mean_score,
        color = .data$party_label
      ),
      width = 0.65, linewidth = 1.2, inherit.aes = FALSE
    )

  if (!is.null(highlight_pts) && nrow(highlight_pts) > 0) {
    p <- p + ggplot2::geom_point(
      data = highlight_pts,
      ggplot2::aes(
        x     = .data$topic_label,
        y     = .data$mean_score,
        color = .data$label
      ),
      size = 2.5, inherit.aes = FALSE
    )
  }

  p +
    ggplot2::scale_color_manual(name = NULL, values = color_values) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::facet_grid(direction ~ chamber) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white", color = "black"),
      axis.text.x = ggplot2::element_text(angle = 90, hjust = 1),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      legend.background = ggplot2::element_rect(color = "black", fill = "white",
                                                linewidth = 0.6),
      legend.box.background = ggplot2::element_rect(color = "black", fill = NA,
                                                    linewidth = 0.8)
    ) +
    ggplot2::labs(
      x        = "Issue Area",
      y        = "Bipartisanship Score (0-1)",
      title    = title,
      subtitle = subtitle
    )
}

ordinal_suffix <- function(n) {
  n <- as.integer(n)
  if (n %% 100 %in% c(11, 12, 13)) return("th")
  switch(as.character(n %% 10),
         "1" = "st", "2" = "nd", "3" = "rd",
         "th")
}
