# ============================================================================
# setup.R — one-time setup for the biparty R package
# ============================================================================
#
# Run this ONCE, from the biparty/ folder, after unzipping biparty.zip and
# opening it in RStudio (File > New Project > Existing Directory > biparty/).
#
# HOW TO USE:
#   1. Make sure the four source data files live in data-raw/ (see STEP 2).
#   2. Open this file in RStudio.
#   3. Click "Source" (top right of the script pane) OR run line by line.
#
# The script will tell you at each step whether it succeeded or what's wrong.
# ============================================================================


# ----------------------------------------------------------------------------
# STEP 0. Sanity check — are we in the right folder?
# ----------------------------------------------------------------------------
if (!file.exists("DESCRIPTION") || !dir.exists("R")) {
  stop("This script must be run from the biparty/ package root. ",
       "Your current working directory is:\n  ", getwd(), "\n",
       "In RStudio: File > Open Project > biparty.Rproj (or set the ",
       "working directory to the biparty folder).")
}
message("OK: running from ", getwd())


# ----------------------------------------------------------------------------
# STEP 1. Install the R packages we need.
# ----------------------------------------------------------------------------
needed <- c("devtools", "usethis", "roxygen2",
            "yaml", "purrr", "dplyr", "tibble", "tidyr",
            "rlang", "stringr", "ggplot2", "scales", "testthat")
to_install <- setdiff(needed, rownames(installed.packages()))
if (length(to_install)) {
  message("Installing: ", paste(to_install, collapse = ", "))
  install.packages(to_install)
}
message("OK: all dependencies installed.")


# ----------------------------------------------------------------------------
# STEP 2. Put the raw source files in data-raw/.
# ----------------------------------------------------------------------------
# You need FOUR files in data-raw/ before this script can rebuild the
# bundled .rda data:
#
#   data-raw/3_portman_general_scores_dataframe.rds   (from your pipeline)
#   data-raw/3_portman_issue_scores_dataframe.rds     (from your pipeline)
#   data-raw/legislators-current.yaml                 (congress-legislators)
#   data-raw/legislators-historical.yaml              (congress-legislators)
#
# The two YAMLs are public-domain. If you don't have them locally, download:
yaml_dir <- "data-raw"
if (!dir.exists(yaml_dir)) dir.create(yaml_dir)

yaml_current  <- file.path(yaml_dir, "legislators-current.yaml")
yaml_historic <- file.path(yaml_dir, "legislators-historical.yaml")

if (!file.exists(yaml_current)) {
  message("Downloading legislators-current.yaml ...")
  utils::download.file(
    "https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-current.yaml",
    yaml_current, mode = "wb")
}
if (!file.exists(yaml_historic)) {
  message("Downloading legislators-historical.yaml ...")
  utils::download.file(
    "https://raw.githubusercontent.com/unitedstates/congress-legislators/main/legislators-historical.yaml",
    yaml_historic, mode = "wb")
}

rds_gen <- file.path(yaml_dir, "3_portman_general_scores_dataframe.rds")
rds_iss <- file.path(yaml_dir, "3_portman_issue_scores_dataframe.rds")
missing_rds <- !c(file.exists(rds_gen), file.exists(rds_iss))
if (any(missing_rds)) {
  stop("Copy your two .rds files into data-raw/ before continuing:\n",
       "  - ", rds_gen, "\n",
       "  - ", rds_iss, "\n",
       "Then re-run this script.")
}
message("OK: all four source files are in data-raw/.")


# ----------------------------------------------------------------------------
# STEP 3. Rebuild the bundled .rda data via native R.
# ----------------------------------------------------------------------------
# The .rda files that shipped in the zip were built with a Python helper
# (because the build sandbox had no R). Round-tripping them through real R
# here makes them CRAN-grade.
message("Rebuilding portman_general, portman_issue, issue_labels ...")
source("data-raw/build_package_data.R")

message("Rebuilding id_crosswalk ...")
# build_id_crosswalk.R expects the YAMLs in the working directory, not in
# data-raw/. Stage symlinks so it finds them without editing the script.
if (!file.exists("legislators-current.yaml"))
  file.copy(yaml_current, "legislators-current.yaml")
if (!file.exists("legislators-historical.yaml"))
  file.copy(yaml_historic, "legislators-historical.yaml")
source("data-raw/build_id_crosswalk.R")
# Clean up the staged copies
file.remove("legislators-current.yaml", "legislators-historical.yaml")

message("OK: data/ rebuilt. You should see:")
print(list.files("data", full.names = FALSE))


# ----------------------------------------------------------------------------
# STEP 4. Regenerate documentation, run tests, and run R CMD check.
# ----------------------------------------------------------------------------
message("\nRegenerating man/ from roxygen comments ...")
devtools::document()

message("\nRunning unit tests ...")
devtools::test()

message("\nRunning R CMD check (this takes a minute) ...")
chk <- devtools::check(error_on = "never")
# Look for 0 errors, 0 warnings, ideally 0 notes.
# Some NOTEs like "New submission" or "unable to verify current time" are fine.


# ----------------------------------------------------------------------------
# STEP 5. Try the package to make sure it works.
# ----------------------------------------------------------------------------
message("\nLoading biparty and running a smoke test ...")
devtools::load_all()

cat("\n-- Rob Portman (all Congresses) --\n")
print(get_member("Rob Portman"))

cat("\n-- Top 10 most bipartisan senators, 117th Congress, attract --\n")
print(rank_members(congress = 117, chamber = "senate",
                   direction = "attract", n = 10))

cat("\n-- Available issue labels --\n")
print(list_issues())

cat("\n-- Crosswalk: Portman's career --\n")
data(id_crosswalk)
print(subset(id_crosswalk, bioguide_id == "P000449",
             select = c("display_name","chamber","state",
                        "congress_start","congress_end",
                        "govtrack_id","icpsr_id")))


# ----------------------------------------------------------------------------
# STEP 6. Initialize git and push to GitHub.
# ----------------------------------------------------------------------------
# Skip this step if you already have the repo set up.
#
# If this is your first time using usethis + GitHub from R, you may need:
#   usethis::create_github_token()     # opens a browser; copy the token
#   gitcreds::gitcreds_set()           # paste the token when prompted
#
# Then:
#   usethis::use_git()
#   usethis::use_github(organisation = "portman-center")
#
# If you don't have rights to create repos under portman-center yet, push
# to your personal account and transfer ownership later:
#   usethis::use_github()
#
# Either way, users can then install with:
#   remotes::install_github("portman-center/biparty")

message("\nSETUP COMPLETE.")
message("If R CMD check showed 0 errors / 0 warnings, you're ready.")
message("Next: run usethis::use_git() and usethis::use_github() when ready.")
