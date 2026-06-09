#' @importFrom dplyr %>%
NULL

# Internal: normalize chamber argument.
.normalize_chamber <- function(chamber) {
  if (is.null(chamber)) return(NULL)
  chamber <- tolower(as.character(chamber))
  map <- c(
    "house"  = "HOUSE",
    "senate" = "SENATE",
    "h"      = "HOUSE",
    "s"      = "SENATE",
    "hr"     = "HOUSE"
  )
  out <- unname(map[chamber])
  if (any(is.na(out))) {
    bad <- chamber[is.na(out)]
    stop("Unrecognized chamber value(s): ",
         paste(shQuote(bad), collapse = ", "),
         ". Use 'house' or 'senate'.", call. = FALSE)
  }
  out
}

# Internal: normalize party argument.
.normalize_party <- function(party) {
  if (is.null(party)) return(NULL)
  party <- toupper(as.character(party))
  map <- c("D" = "D", "R" = "R",
           "DEM" = "D", "REP" = "R",
           "DEMOCRAT" = "D", "REPUBLICAN" = "R",
           "DEMOCRATIC" = "D")
  out <- unname(map[party])
  if (any(is.na(out))) {
    bad <- party[is.na(out)]
    stop("Unrecognized party value(s): ",
         paste(shQuote(bad), collapse = ", "),
         ". Use 'D' or 'R'.", call. = FALSE)
  }
  out
}

# Internal: resolve an issue argument to its short label.
.resolve_issue <- function(issue) {
  if (length(issue) != 1 || !is.character(issue)) {
    stop("`issue` must be a single character string.", call. = FALSE)
  }
  labels <- .require_data("issue.labels")
  needle <- tolower(trimws(issue))
  
  hit_short <- labels$topic_short == needle
  hit_area  <- labels$policy_area == needle
  hit_label <- tolower(labels$topic_label) == needle
  
  idx <- which(hit_short | hit_area | hit_label)
  if (length(idx) == 0) {
    stop("Unknown issue '", issue,
         "'. Call list_issues() to see available labels.",
         call. = FALSE)
  }
  if (length(idx) > 1) idx <- idx[1]
  labels$topic_short[idx]
}

# Internal: safe access to an optional package dataset.
.require_data <- function(name) {
  # First check the package namespace (covers internal sysdata objects)
  ns <- asNamespace("biparty")
  if (exists(name, envir = ns, inherits = FALSE)) {
    return(get(name, envir = ns, inherits = FALSE))
  }
  # Fall back to data() for exported datasets
  env <- new.env()
  data(list = name, package = "biparty", envir = env)
  if (!exists(name, envir = env)) {
    stop("Dataset '", name, "' not found in package 'biparty'.",
         call. = FALSE)
  }
  get(name, envir = env)
}