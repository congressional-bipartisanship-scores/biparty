# biparty

> Congressional Bipartisanship Scores by Member and Issue Area

`biparty` provides Congressional Bipartisanship Scores (CBS) for all members
of the U.S. House and Senate from 1983 to 2024, spanning the 98th through the
118th Congresses. The scores are introduced and described in Authors (2026),
with support from an anonymous funding source.

## What the scores measure

For each member–Congress–chamber observation, the package provides two
complementary measures, each computed overall and within 34 CRS policy areas:

- **Attract** — the share of out-party original cosponsors on bills the
  member *sponsored*. Captures the member's ability to draw cross-party
  support to their own legislation.
- **Offer** — the share of the member's own original cosponsorships that
  went to bills sponsored by the out-party. Captures the member's
  willingness to lend support to the other party.

Both measures are available in unweighted and weighted variants. The weighted
version adjusts for imprecise measurement associated with legislators with low
bill activity, pulling low-volume observations toward a Congress × chamber
(or Congress × chamber × issue) prior mean.

Scores range from 0 to 1, with higher scores indicating more cross-party activity.

## Installation

```r
# Install from GitHub (requires 'remotes' or 'devtools')
# install.packages("remotes")
remotes::install_github("congressional-bipartisanship-scores/biparty")
```

## Quick start

```r
library(biparty)

# Pull a specific member's scores across all Congresses they served
get_member_scores("Rob Portman")

# Rank the most bipartisan senators in the 117th Congress by attract score
rank_members(congress = 117, chamber = "senate", score_type = "attract", n = 10)

# Pull all members' health-policy scores in the 117th Congress
get_issue_scores(issue = "health", congress = 117)

# Violin plot of issue-area scores by party and chamber
plot_bipartisanship(congress = 117)
```

## Datasets

| Dataset | Description |
|---|---|
| `aggregate.cbs` | Aggregate attract and offer scores per member × Congress × chamber, including identifier crosswalk columns. |
| `issue.area.cbs` | Issue-area-specific attract and offer scores across 34 CRS policy areas, including identifier crosswalk columns. |

```r
data(aggregate.cbs)
data(issue.area.cbs)
```

## Coverage

The current release includes the **98th through 118th Congresses**
(1983–2024) — 11,549 member-Congress-chamber observations covering
2,056 unique legislators across both chambers.

## Methodology and replication

The full construction pipeline is documented in Authors (2026).
Replication code and data are publicly available through Harvard Dataverse.

## Citation

If you use these scores, please cite the paper:

> Authors (2026). Congressional bipartisanship scores
> by member and issue area, 1983–2024. Working paper.

and the R package:

> Authors (2026). *biparty: Congressional Bipartisanship
> Scores by Member and Issue Area*. R package version 0.1.0.
> https://github.com/congressional-bipartisanship-scores/biparty

## About the authors

Details withheld for blind review.

## License

MIT © 2026 Anonymous Authors.
