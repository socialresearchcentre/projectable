
# projectable

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Overview

Producing output tables is an exceedingly manual activity, particularly when
tabling complex statistics with associated metadata. When preparing large
numbers of tables for presentation or publication, providing different views of
the same result set can require large amounts of re-processing and fiddly manual
combination and reshaping of outputs.

Inspired by the `gt` R package (https://gt.rstudio.com/), `projectable` is
designed to easily support flexible specification and table manipulation. The
`projectable` approach treats a table as a collection of calculations with
accompanying metadata rather than simple values. It aims to support easily
moving from specification to data production to presentation of results by
treating an output table as a “projection” of complex results.

## Installation

You can install the unreleased version of projectable from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("socialresearchcentre/projectable")
```

## Usage

The `projectable` package implements a set of interlocking functions which are
designed to ingest a simple dataframe, perform calculations upon it to 
generate a metadata-rich table like object, and reshape that object back into
a 'flat' dataframe to be formatted using the `gt` package.

``` r
library(projectable)
library(dplyr)
library(gt)

mtcars %>% 
  # Create metadata rich table
  set_table(
      .rows = list(
        Cylinders = cyl,
        Transmission = am
      ),
      .cols = list(
        vshaped = encol_freq(little_n = vs %in% 1, big_n = vs %in% 0:1),
        not_vshaped = encol_freq(little_n = vs %in% 0, big_n = vs %in% 0:1)
      )
    ) %>% 
  # Reshape for display 
  project_table(
    .cols = list(
      rows = "identity",
      row_spanner = "identity",
      vshaped = c("proportion", "little_n", "big_n"),
      not_vshaped = c("proportion", "little_n", "big_n")
    )
  ) %>% 
  # Pass through to `gt` for formatting
  dplyr::group_by(row_spanner) %>% 
  proje_gt(rowname_col = "rows")
  
```

