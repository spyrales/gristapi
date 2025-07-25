
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gristapi <img src='man/figures/gristapi_logo.png' align="right" width="139px" />

<!-- badges: start -->

[![R-CMD-check](https://github.com/spyrales/gristapi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/spyrales/gristapi/actions/workflows/R-CMD-check.yaml)
[![Version](https://img.shields.io/github/r-package/v/spyrales/gristapi)](https://github.com/spyrales/gristapi/blob/main/DESCRIPTION)
<!-- badges: end -->

The goal of this package is to communicate from R with a Grist API.

he package provides an R6 class to connect to a Grist document (which
contains multiple tables), and a set of functions to interact with the
document.

This work follows the Grist API references
<https://support.getgrist.com/api/> as of July 2025.

This project is currently under development; feel free to submit issues!

------------------------------------------------------------------------

L’objectif de ce package est de communiquer depuis R avec une API Grist.

Le paquet fournit une classe R6 pour se connecter à un document Grist
(qui contient plusieurs tables), et, un ensemble de fonctions pour
échanger avec le document.

Ce travail suit les références de l’API Grist
<https://support.getgrist.com/api/> de juillet 2025.

Projet en développement, n’hésitez pas à faire des tickets !

## Installation

You can install the current version of gristapi from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("spyrales/gristapi")
```

## Documentation

Presentation site of the package and its functions
<https://spyrales.github.io/gristapi/>

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(gristapi)
suppressMessages(library(dplyr))
#> Warning: le package 'dplyr' a été compilé avec la version R 4.4.3
# Working only if Env Var "GRIST_KEY" and "GRIST_DOC_TEST" are defined

# Create a New Api Object on one grist document
api <- grist_api$new(
  server = 'https://grist.numerique.gouv.fr', 
  api_key = Sys.getenv("GRIST_KEY"), 
  doc_id = Sys.getenv("GRIST_DOC_TEST")
  )

# Create a table with data
add_records(api,
  table_id = "Mtcars",
  record_dicts = mtcars |> mutate(names = row.names(mtcars)) |> relocate(names),
  create_or_replace = TRUE
  )
#>  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25
#> [26] 26 27 28 29 30 31 32

# Call all data from a table
fetch_table(api,"Mtcars")
#> # A tibble: 32 × 13
#>       id names   mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <int> <chr> <dbl> <int> <dbl> <int> <dbl> <dbl> <dbl> <int> <int> <int> <int>
#>  1     1 Mazd…  21       6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2     2 Mazd…  21       6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3     3 Dats…  22.8     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4     4 Horn…  21.4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5     5 Horn…  18.7     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6     6 Vali…  18.1     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7     7 Dust…  14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8     8 Merc…  24.4     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9     9 Merc…  22.8     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10    10 Merc…  19.2     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows

# Synchronize a table with a dataframe (update or add)
# Preparing the dataframe
new_data <- fetch_table(api,"Mtcars", filters = 'filter={"cyl": [8]}')
new_data <- new_data |> 
  mutate(carb = 4, names = ifelse(names =='Camaro Z28', 'Camaro Z28 - custom', names)) |> 
  select(-id)
new_data
#> # A tibble: 14 × 12
#>    names         mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <chr>       <dbl> <int> <dbl> <int> <dbl> <dbl> <dbl> <int> <int> <int> <dbl>
#>  1 Hornet Spo…  18.7     8  360    175  3.15  3.44  17.0     0     0     3     4
#>  2 Duster 360   14.3     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  3 Merc 450SE   16.4     8  276.   180  3.07  4.07  17.4     0     0     3     4
#>  4 Merc 450SL   17.3     8  276.   180  3.07  3.73  17.6     0     0     3     4
#>  5 Merc 450SLC  15.2     8  276.   180  3.07  3.78  18       0     0     3     4
#>  6 Cadillac F…  10.4     8  472    205  2.93  5.25  18.0     0     0     3     4
#>  7 Lincoln Co…  10.4     8  460    215  3     5.42  17.8     0     0     3     4
#>  8 Chrysler I…  14.7     8  440    230  3.23  5.34  17.4     0     0     3     4
#>  9 Dodge Chal…  15.5     8  318    150  2.76  3.52  16.9     0     0     3     4
#> 10 AMC Javelin  15.2     8  304    150  3.15  3.44  17.3     0     0     3     4
#> 11 Camaro Z28…  13.3     8  350    245  3.73  3.84  15.4     0     0     3     4
#> 12 Pontiac Fi…  19.2     8  400    175  3.08  3.84  17.0     0     0     3     4
#> 13 Ford Pante…  15.8     8  351    264  4.22  3.17  14.5     0     1     5     4
#> 14 Maserati B…  15       8  301    335  3.54  3.57  14.6     0     1     5     4
# Launching synchronization on the "names" key
sync_table(api,"Mtcars", new_data, key_cols = c("names"))
#> [1] TRUE
# One entry was added, the other 13 were updated
tail(fetch_table(api,"Mtcars"))
#> # A tibble: 6 × 13
#>      id names    mpg   cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>   <int> <chr>  <dbl> <int> <dbl> <int> <dbl> <dbl> <dbl> <int> <int> <int> <int>
#> 1    28 Lotus…  30.4     4  95.1   113  3.77  1.51  16.9     1     1     5     2
#> 2    29 Ford …  15.8     8 351     264  4.22  3.17  14.5     0     1     5     4
#> 3    30 Ferra…  19.7     6 145     175  3.62  2.77  15.5     0     1     5     6
#> 4    31 Maser…  15       8 301     335  3.54  3.57  14.6     0     1     5     4
#> 5    32 Volvo…  21.4     4 121     109  4.11  2.78  18.6     1     1     4     2
#> 6    33 Camar…  13.3     8 350     245  3.73  3.84  15.4     0     0     3     4
```

## Acknowledgements

Thanks to [py_grist_api](https://github.com/gristlabs/py_grist_api) by
Grist Labs and [gristr](https://forge.ird.fr/phim/cunnac/gristr) by
Sebastien Cunnac, which provided the basis of inspiration for this work.
