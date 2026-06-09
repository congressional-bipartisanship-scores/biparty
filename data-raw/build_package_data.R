#-----------------------------------------------------------
# data-raw/build_package_data.R
#
# Canonical data-build script for the 'biparty' package. Run this after
# changes to the upstream Portman scoring pipeline to refresh the .rda
# files shipped in data/.
#
# Inputs (expected in this directory):
#   - 3_portman_general_scores_dataframe.rds   (from 3_create_portman_scores.R)
#   - 3_portman_issue_scores_dataframe.rds     (from 3_create_portman_scores.R)
#
# Outputs:
#   - data/portman_general.rda
#   - data/portman_issue.rda
#   - data/issue_labels.rda
#   - inst/extdata/portman_general_scores.csv
#   - inst/extdata/portman_issue_scores.csv
#   - inst/extdata/issue_labels.csv
#
# Usage (from the package root):
#   source("data-raw/build_package_data.R")
#-----------------------------------------------------------

library(dplyr)
library(tibble)

#-----------------------------------------------------------
# 1. Load upstream scores
#-----------------------------------------------------------
portman_general <- readRDS("data-raw/3_portman_general_scores_dataframe.rds")
portman_issue   <- readRDS("data-raw/3_portman_issue_scores_dataframe.rds")

# Normalize types and column order
portman_general <- portman_general %>%
  mutate(across(c(chamber, name, party, bioguide_id), as.character),
         congress = as.integer(congress)) %>%
  select(congress, chamber, bioguide_id, name, party,
         PORTMAN_attract, PORTMAN_offer) %>%
  as_tibble()

id_cols <- c("congress", "chamber", "bioguide_id", "name", "party")
portman_issue <- portman_issue %>%
  mutate(across(c(chamber, name, party, bioguide_id), as.character),
         congress = as.integer(congress)) %>%
  select(all_of(id_cols), everything()) %>%
  as_tibble()

#-----------------------------------------------------------
# 2. Build the issue-area label crosswalk
#-----------------------------------------------------------
issue_labels <- tibble::tribble(
  ~policy_area_number, ~policy_area,                                     ~topic_short, ~topic_label,
  1L,  "families",                                    "fam",      "Families",
  2L,  "education",                                   "edu",      "Education",
  3L,  "commerce",                                    "comm",     "Commerce",
  4L,  "labor and employment",                        "labor",    "Labor and Employment",
  5L,  "government operations and politics",          "gov",      "Government Operations and Politics",
  6L,  "native americans",                            "native",   "Native Americans",
  7L,  "animals",                                     "animals",  "Animals",
  8L,  "transportation and public works",             "trans",    "Transportation and Public Works",
  9L,  "emergency management",                        "emerg",    "Emergency Management",
  10L, "finance and financial sector",                "finance",  "Finance and Financial Sector",
  11L, "public lands and natural resources",          "lands",    "Public Lands and Natural Resources",
  12L, "crime and law enforcement",                   "crime",    "Crime and Law Enforcement",
  13L, "science, technology, communications",         "sci",      "Science, Technology, Communications",
  14L, "law",                                         "law",      "Law",
  15L, "environmental protection",                    "env",      "Environmental Protection",
  16L, "health",                                      "health",   "Health",
  17L, "water resources development",                 "water",    "Water Resources Development",
  18L, "immigration",                                 "immig",    "Immigration",
  19L, "energy",                                      "energy",   "Energy",
  20L, "international affairs",                       "intl",     "International Affairs",
  21L, "economics and public finance",                "econ",     "Economics and Public Finance",
  22L, "social welfare",                              "welfare",  "Social Welfare",
  23L, "taxation",                                    "tax",      "Taxation",
  24L, "foreign trade and international finance",     "trade",    "Foreign Trade and International Finance",
  25L, "arts, culture, religion",                     "arts",     "Arts, Culture, Religion",
  26L, "housing and community development",           "housing",  "Housing and Community Development",
  27L, "agriculture and food",                        "ag",       "Agriculture and Food",
  28L, "armed forces and national security",          "defense",  "Armed Forces and National Security",
  29L, "private legislation",                         "private",  "Private Legislation",
  30L, "civil rights and liberties, minority issues", "civil",    "Civil Rights and Liberties, Minority Issues",
  31L, "congress",                                    "cong",     "Congress",
  32L, "sports and recreation",                       "sports",   "Sports and Recreation",
  33L, "commemorations",                              "commem",   "Commemorations",
  34L, "social sciences and history",                 "history",  "Social Sciences and History",
  99L, "na",                                          "na",       "Unclassified"
)

#-----------------------------------------------------------
# 3. Save as package data
#-----------------------------------------------------------
if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(portman_general, overwrite = TRUE, compress = "xz")
  usethis::use_data(portman_issue,   overwrite = TRUE, compress = "xz")
  usethis::use_data(issue_labels,    overwrite = TRUE, compress = "xz")
} else {
  dir.create("data", showWarnings = FALSE)
  save(portman_general, file = "data/portman_general.rda", compress = "xz")
  save(portman_issue,   file = "data/portman_issue.rda",   compress = "xz")
  save(issue_labels,    file = "data/issue_labels.rda",    compress = "xz")
}

#-----------------------------------------------------------
# 4. Write CSV copies for non-R users
#-----------------------------------------------------------
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
write.csv(portman_general, "inst/extdata/portman_general_scores.csv",
          row.names = FALSE)
write.csv(portman_issue,   "inst/extdata/portman_issue_scores.csv",
          row.names = FALSE)
write.csv(issue_labels,    "inst/extdata/issue_labels.csv",
          row.names = FALSE)

message(
  "Built package data: ",
  nrow(portman_general), " general rows, ",
  nrow(portman_issue),   " issue rows, ",
  nrow(issue_labels),    " issue labels."
)
