
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tibbletator

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/kaneplusplus/tibbletator/workflows/R-CMD-check/badge.svg)](https://github.com/kaneplusplus/tibbletator/actions)
<!-- badges: end -->

At the unholy intersection of the tidyverse, the shinyverse, and
Microsoft Excel isthe `tibbletator`, which allows you to manually change
values in a `data.frame` object using shiny.

## Installation

``` r
devtools::install_github("presagia-analytics/tibbletator")
```

## Example

Say you want to check whether or not the cars in the mtcars data set are
aesthetic and/or practical.

``` r
library(tibbletator)
library(dplyr)
library(tibble)

# Take the `mtcars` model and tell if a car is aesthetic and/or practical.
my_pics <- mtcars %>%
  head() %>%
  mutate(make = rownames(.)) %>%
  select(make) %>%
  mutate(aesthetic = FALSE, practical = FALSE) %>%
  tibbletate(annotate_cols = c("aesthetic", "practical"))

# The `aesthetic` and `practical columns in the `my_pics` dataframe reflect 
# your choices.
my_pics
```

    #> # A tibble: 6 × 3
    #>   make              aesthetic practical
    #>   <chr>             <lgl>     <lgl>    
    #> 1 Mazda RX4         TRUE      TRUE     
    #> 2 Mazda RX4 Wag     FALSE     TRUE     
    #> 3 Datsun 710        FALSE     FALSE    
    #> 4 Hornet 4 Drive    TRUE      FALSE    
    #> 5 Hornet Sportabout FALSE     FALSE    
    #> 6 Valiant           FALSE     FALSE
