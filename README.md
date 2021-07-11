[![R-CMD-check](https://github.com/KWB-R/kwb.odmx/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.odmx/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.odmx/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.odmx/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.odmx/branch/main/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.odmx)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.odmx)]()

# kwb.odmx

Extended functions to access the data in an ODM database.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwb.odmx' from GitHub
remotes::install_github("KWB-R/kwb.odmx")
```

## Documentation

Release: [https://kwb-r.github.io/kwb.odmx](https://kwb-r.github.io/kwb.odmx)

Development: [https://kwb-r.github.io/kwb.odmx/dev](https://kwb-r.github.io/kwb.odmx/dev)
