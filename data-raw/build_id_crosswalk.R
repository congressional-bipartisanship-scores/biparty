# data-raw/build_id_crosswalk.R
#
# Build the id_crosswalk dataset shipped with biparty.
#
# Source: unitedstates/congress-legislators (public domain)
#   https://github.com/unitedstates/congress-legislators
#
# Expects two YAML files in the working directory (download once, then commit
# to data-raw/ or keep out of the package and regenerate periodically):
#   legislators-current.yaml
#   legislators-historical.yaml
#
# These can be fetched with:
#   download.file(
#     "https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-current.yaml",
#     "legislators-current.yaml")
#   download.file(
#     "https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-historical.yaml",
#     "legislators-historical.yaml")
#
# Run from the package root:
#   source("data-raw/build_id_crosswalk.R")

library(yaml)
library(dplyr)
library(purrr)
library(tibble)

stopifnot(file.exists("legislators-current.yaml"),
          file.exists("legislators-historical.yaml"))

people <- c(
  yaml::read_yaml("legislators-current.yaml"),
  yaml::read_yaml("legislators-historical.yaml")
)

year_to_congress <- function(y) {
  if (is.null(y) || is.na(y)) return(NA_integer_)
  as.integer((as.integer(y) - 1789L) %/% 2L + 1L)
}

yr <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_integer_)
  as.integer(substr(as.character(x), 1, 4))
}

flatten_person <- function(p) {
  ids  <- p$id   %||% list()
  name <- p$name %||% list()
  bio  <- ids$bioguide
  if (is.null(bio)) return(NULL)

  fec_ids <- if (is.null(ids$fec)) NA_character_
             else paste(ids$fec, collapse = ";")

  base <- tibble(
    bioguide_id    = bio,
    govtrack_id    = ids$govtrack     %||% NA_integer_,
    thomas_id      = ids$thomas       %||% NA_character_,
    opensecrets_id = ids$opensecrets  %||% NA_character_,
    icpsr_id       = ids$icpsr        %||% NA_real_,
    fec_ids        = fec_ids,
    maplight_id    = ids$maplight     %||% NA_real_,
    wikipedia      = ids$wikipedia    %||% NA_character_,
    wikidata       = ids$wikidata     %||% NA_character_,
    first_name     = name$first       %||% NA_character_,
    last_name      = name$last        %||% NA_character_,
    display_name   = name$official_full %||%
                       trimws(paste(name$first %||% "", name$last %||% ""))
  )

  terms <- p$terms %||% list()
  if (!length(terms)) return(NULL)

  purrr::map_dfr(terms, function(t) {
    tt <- t$type %||% NA_character_
    chamber <- switch(tt, sen = "senate", rep = "house", tt)
    ys <- yr(t$start); ye <- yr(t$end)
    dplyr::bind_cols(base, tibble(
      term_type      = tt,
      chamber        = chamber,
      state          = t$state %||% NA_character_,
      district       = t$district %||% NA_real_,
      party          = t$party %||% NA_character_,
      start_date     = as.character(t$start %||% NA),
      end_date       = as.character(t$end   %||% NA),
      start_year     = ys,
      end_year       = ye,
      years          = if (!is.na(ys) && !is.na(ye)) paste0(ys, "-", ye) else NA_character_,
      congress_start = year_to_congress(ys),
      congress_end   = year_to_congress(ye)
    ))
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

all_terms <- purrr::map_dfr(people, flatten_person)

# Scope to biparty coverage
load("data/portman_general.rda")
keep <- unique(portman_general$bioguide_id)
id_crosswalk <- all_terms |>
  dplyr::filter(bioguide_id %in% keep) |>
  dplyr::arrange(bioguide_id, start_date)

stopifnot(dplyr::n_distinct(id_crosswalk$bioguide_id) == length(keep))
message("id_crosswalk: ", nrow(id_crosswalk), " rows, ",
        dplyr::n_distinct(id_crosswalk$bioguide_id), " unique legislators")

if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(id_crosswalk, overwrite = TRUE, compress = "gzip")
} else {
  save(id_crosswalk, file = "data/id_crosswalk.rda", compress = "gzip")
}

dir.create("inst/extdata", recursive = TRUE, showWarnings = FALSE)
utils::write.csv(id_crosswalk, "inst/extdata/id_crosswalk.csv",
                 row.names = FALSE, na = "")
