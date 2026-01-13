Population
================
Author: Andreas Beger

Last updated on: 13 January 2026

- [Acquire/update raw data](#acquireupdate-raw-data)
  - [Expanded population data from
    KSG](#expanded-population-data-from-ksg)
  - [WDI pop data](#wdi-pop-data)
  - [UN Pop data](#un-pop-data)
- [Combine and overlap](#combine-and-overlap)
  - [Within country covariances between UN and
    KSG](#within-country-covariances-between-un-and-ksg)
  - [Countries where UN, KSG, WDI do not
    agree](#countries-where-un-ksg-wdi-do-not-agree)
  - [Can we combine KSG for pre-50 with
    UN?](#can-we-combine-ksg-for-pre-50-with-un)
  - [Germany](#germany)
  - [Vietnam](#vietnam)
  - [Yemen](#yemen)
  - [Tibet](#tibet)
  - [Czechoslovakia](#czechoslovakia)
- [Get ready to write final data](#get-ready-to-write-final-data)
  - [Check values for splitting/joining
    countries](#check-values-for-splittingjoining-countries)
  - [Missingness](#missingness)

NOTE: This file is generated from README.R. To spint/knit/compile the
.md file, run:
`setwd("population"); rmarkdown::render("clean-population.R")`

This file combines UN, WDI, and KSG population data to create a complete
coverage dataset for 1950 onwards, with no major states missing.

The data sources are:

- <https://population.un.org/wpp/DataQuery/>: select total population by
  sex and SDG regions; only 35 years can be exported at a time so three
  files are needed
- <http://ksgleditsch.com/data/exppop.txt>
- World Bank, via the WDI package

To update the data:

1.  Download a new version of the UN population dataset at
    <https://population.un.org/dataportal/>. Only 35 years can be
    exported at a time so three files are needed. Select total
    population by sex, countries, years needed, then search. In the
    resulting display, on the display tab select Locations \>
    Characteristics \> Variants, then in Settings select “Scaling
    Factor” as ‘k’. This gets data in the previous format. Export to
    Excel and rename the output file to match the pattern in the
    `input/` folder.
2.  Delete “input/wdipop.csv”; it’ll be downloaded and cached again.
3.  Search for “UPDATE:” for lines in this document that need attention,
    possibly.

``` r
suppressPackageStartupMessages({
  library(countrycode)
  library(dplyr)
  library(readr)
  library(states)
  library(WDI)
  library(lubridate)
  library(readxl)
  library(kableExtra)
  library(imputeTS)
  library(here)
  library(purrr)
  library(tidyr)
  library(ggplot2)
})

oldwd <- getwd()
setwd(here::here("population"))


# Functions ----------------------------------------------------------

wdi_add_gwcode <- function(x) {
  starty <- min(x$year)
  endy <- max(x$year)
  cy <- states::state_panel(starty, endy, useGW = TRUE, partial = "any")

  x <- x %>%
    mutate(gwcode = suppressWarnings(countrycode::countrycode(x$iso2c, "iso2c", "cown")),
           gwcode = as.integer(gwcode)) %>%
    mutate(gwcode = case_when(
      iso2c=="RS" ~ 340L,
      iso2c=="XK" ~ 347L,
      iso2c=="VN" ~ 816L,

      gwcode==255 ~ 260L,
      gwcode==679 ~ 678L,

      gwcode==970 ~ 971L,
      gwcode==946 ~ 970L,
      gwcode==947 ~ 973L,
      gwcode==955 ~ 972L,
      TRUE ~ gwcode
    ))

  # fix Czechoslovakia
  x$gwcode[x$gwcode==316 & x$year <= 1992] <- 315L

  # drop countries that unify
  x <- x %>%
    # pre-95 Serbia
    filter(!(gwcode==340 & year < 1995)) %>%
    filter(!(gwcode==260 & year < 1990)) %>%
    filter(!(gwcode==678 & year < 1990)) %>%
    # pre-75 Vietnam
    filter(!(gwcode==816 & year < 1975))

  x <- dplyr::select(x, -iso2c, -country)
  x <- x %>% filter(!is.na(gwcode))

  cy <- dplyr::left_join(cy, x, by = c("gwcode", "year"))
  cy
}

data(gwstates)
cnames <- gwstates %>%
  group_by(gwcode) %>%
  slice(n()) %>%
  select(gwcode, country_name) %>%
  ungroup()


# Acquire/update raw data -------------------------------------------------
```

## Acquire/update raw data

### Expanded population data from KSG

Data are at <http://ksgleditsch.com/data/exppop.txt>

``` r
if (!file.exists("input/exppop.tsv")) {
  url <- "http://ksgleditsch.com/data/exppop.txt"
  download.file(url, destfile = "input/exppop.tsv")
}

ksg <- read_tsv("input/exppop.tsv")
```

    ## Rows: 16729 Columns: 5
    ## ── Column specification ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## chr (1): idacr
    ## dbl (4): idnum, year, pop, source
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

The data range from 1816 to 2004.

``` r
plot_missing(ksg, x = "pop", ccode = "idnum", time = "year", statelist = "GW")
```

![](clean-population_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# These data only go to 2004. Need to splice in updates.
```

### WDI pop data

Via the WB API thanks to WDI package.

``` r
# UPDATE: delete input/wdipop.csv to re-download
if (!file.exists("input/wdipop.csv")) {
  wdi_raw <- WDI(country = "all", indicator = "SP.POP.TOTL",
               start = 1960, end = year(Sys.Date()), extra = FALSE)
  write.csv(wdi_raw, "input/wdipop.csv", row.names = FALSE)
}

wdi_raw <- read.csv("input/wdipop.csv")

wdi <- wdi_raw %>% wdi_add_gwcode(.) %>%
  rename(pop = SP.POP.TOTL) %>%
  # change to pop in 1,000s
  mutate(pop = pop / 1e3)

plot_missing(wdi, x = "pop", ccode = "gwcode", time = "year", statelist = "GW")
```

![](clean-population_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# UN Pop data -------------------------------------------------------------
```

### UN Pop data

``` r
files <- dir("input", pattern = "^UNPop", full.names = TRUE)

# 2023-03: the data portal at the UN website has changed, giving different
# output format now as well. So make separate parser functions.
parse_un_wide <- function(x) {
  # data are iso, location, ..., year1, year2, ...
  df <- readxl::read_xlsx(x, sheet = "Data", skip = 1)
  df <- rename(df, iso3n = `ISO 3166-1 numeric code`)
  df$Sex <- df$Note <- NULL
  df <- df[df$iso3n < 900, ]
  df <- tidyr::pivot_longer(df, -c(iso3n, Location), names_to = "year", values_to = "pop")
  df$year <- as.integer(df$year)
  df$iso3n <- as.character(df$iso3n)
  df
}
parse_un_long <- function(x) {
  # data are iso, location, year, indicator
  df <- readxl::read_xlsx(x, sheet = "Data", skip = 5,
                          col_names = c("iso3n", "Location", "year", "pop"))
  df$year <- as.integer(df$year)
  df
}
parse_un_wide_2026 <- function(x) {
  # compared to the UN format in previous years, this only has ISO character
  # codes, not numberic ones. And the codes are in a separate sheet, the data
  # sheet only has country names
  #
  # Converting these to numberic codes because
  #
  # the data has country names, but not the iso codes
  df <- readxl::read_xlsx(x, sheet = "Data", skip = 1)

  if (df$...1[[1]] != "Afghanistan") {
    stop("Doesn't look like the first column has country names, adjust the index below")
  } else {
    df <- df |> rename(Location = `...1`)
    df <- df |> select(-c(`...2`, `...3`, `...4`, `...5`))
  }

  # read sheet mapping names to iso
  iso_map <- readxl::read_xlsx(x, sheet = "Locations") |>
    select(`Location Name`, `ISO 3`) |>
    setNames(c("Location", "iso3c")) |>
    mutate(iso3n = countrycode::countrycode(iso3c, "iso3c", "iso3n", warn=FALSE),
           # Kosovo is in the data but doesn't have a ISO code, use a temp one
           iso3n = ifelse(str_detect(Location, "Kosovo"), 1000, iso3n)) |>
    select(-iso3c)

  df <- left_join(df, iso_map, by = "Location")

  df <- tidyr::pivot_longer(df, -c(iso3n, Location), names_to = "year", values_to = "pop")
  df$year <- as.integer(df$year)
  df$iso3n <- as.character(df$iso3n)
  df
}


#un_1950_1984 <- parse_un_wide(files[2])
#un_1985_2019 <- parse_un_wide(files[4])
un_1950_1984 <- parse_un_wide_2026(files[1])
```

    ## New names:
    ## • `` -> `...1`
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`

``` r
un_1985_2019 <- parse_un_wide_2026(files[3])
```

    ## New names:
    ## • `` -> `...1`
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`

``` r
un_2020_now <- parse_un_wide_2026(files[5])
```

    ## New names:
    ## • `` -> `...1`
    ## • `` -> `...2`
    ## • `` -> `...3`
    ## • `` -> `...4`
    ## • `` -> `...5`

``` r
# This is the UN pop data for countries, taking out the region aggregations
un_raw <- rbind(un_1950_1984,
                un_1985_2019,
                un_2020_now)

# Add GW codes
un <- un_raw %>%
  mutate(iso3n = as.integer(iso3n),
         gwcode = countrycode::countrycode(iso3n, "iso3n", "cown", warn = FALSE),
         gwcode = as.integer(gwcode)) %>%
  mutate(gwcode = case_when(
      Location=="Serbia" ~ 340L,
      iso3n==1000 ~ 347L, # Kosovo
      gwcode==255 ~ 260L,
      gwcode==679 ~ 678L,
      gwcode==970 ~ 971L,
      gwcode==946 ~ 970L,
      gwcode==947 ~ 973L,
      gwcode==955 ~ 972L,
      gwcode==817 ~ 816L,
      TRUE ~ gwcode
  ))

un <- un %>%
  filter(!is.na(gwcode))
```

#### Reconstruct some countries that later split

Since the UN data has series for all current countries going back to
1950, we can use those to re-construct countries like USSR and
Yugoslavia that today are several countries. \#### Czechoslovakia

Czechia and Slovakia split on 1 January 1993.

``` r
add <- tibble(
  gwcode = 315,
  year = 1950:1992,
  pop = rowSums(cbind(un$pop[un$gwcode==316 & un$year < 1993],
                         un$pop[un$gwcode==317 & un$year < 1993]))
)
un <- bind_rows(un, add) %>%
  filter(!(gwcode==316 & year < 1993),
         !(gwcode==317 & year < 1993))
```

#### Pakistan pre-1971

Before East Pakistan became Bangladesh.

``` r
pak70 <- rowSums(cbind(un$pop[un$gwcode==770 & un$year < 1970],
                       un$pop[un$gwcode==771 & un$year < 1970]))
un$pop[un$gwcode==770 & un$year < 1970] <- pak70
```

#### Yugoslavia/Serbia & Montenegro/Serbia

- 1990 and before: Slovenia, Croatia, BiH, Serbia, Kosovo, (UN does not
  seem to treat it separately), Montenegro, Macedonia
- 1991: Slovenia, Croatia, BiH, Serbia, Kosovo, (UN does not seem to
  treat it separately), Montenegro
- 1992 and until 2006: Serbia, Kosovo, Montenegro

``` r
yugo <- tibble(
  gwcode = 345,
  year = 1950:2006,
  pop  = rowSums(cbind(
    # Serbia and Montenegro
    un$pop[un$gwcode==340 & un$year <= 2006],
    un$pop[un$gwcode==341 & un$year <= 2006],
    # Slovenia, Croatia, BiH
    c(un$pop[un$gwcode==349 & un$year <= 1991], rep(0, 15)),
    c(un$pop[un$gwcode==344 & un$year <= 1991], rep(0, 15)),
    c(un$pop[un$gwcode==346 & un$year <= 1991], rep(0, 15)),
    # Macedonia
    c(un$pop[un$gwcode==343 & un$year <= 1990], rep(0, 16))
  )))

plot(yugo$year, yugo$pop, ylim = c(0, 25e3), type = "l")
```

![](clean-population_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
un <- un %>%
  filter(!(gwcode==340 & year < 2006),
         !(gwcode==341 & year < 2006),
         !(gwcode==343 & year < 1991),
         !(gwcode==344 & year < 1992),
         !(gwcode==346 & year < 1992),
         !(gwcode==349 & year < 1992)) %>%
  bind_rows(., yugo)
```

##### USSR/Russia

- 1990 and before: Russia (365), the Baltics (366, 367, 368), Ukraine
  (369), Belarus (370), Armenia (371), Georgia (372), Azerbaijan (373),
  central Asia (701, 702, 703, 704, 705)
- 1991 and on: 365

``` r
ussr <- tibble(
  gwcode = 365,
  year = 1950:1990,
  pop = rowSums(cbind(
    un$pop[un$gwcode==365 & un$year < 1991],
    un$pop[un$gwcode==366 & un$year < 1991],
    un$pop[un$gwcode==367 & un$year < 1991],
    un$pop[un$gwcode==368 & un$year < 1991],
    un$pop[un$gwcode==369 & un$year < 1991],
    un$pop[un$gwcode==370 & un$year < 1991],
    un$pop[un$gwcode==371 & un$year < 1991],
    un$pop[un$gwcode==372 & un$year < 1991],
    un$pop[un$gwcode==373 & un$year < 1991],
    un$pop[un$gwcode==701 & un$year < 1991],
    un$pop[un$gwcode==702 & un$year < 1991],
    un$pop[un$gwcode==703 & un$year < 1991],
    un$pop[un$gwcode==704 & un$year < 1991],
    un$pop[un$gwcode==705 & un$year < 1991]
  ))
)

un <- un %>%
  filter(
    !(gwcode==365 & year < 1991),
    !(gwcode==366 & year < 1991),
    !(gwcode==367 & year < 1991),
    !(gwcode==368 & year < 1991),
    !(gwcode==369 & year < 1991),
    !(gwcode==370 & year < 1991),
    !(gwcode==371 & year < 1991),
    !(gwcode==372 & year < 1991),
    !(gwcode==373 & year < 1991),
    !(gwcode==701 & year < 1991),
    !(gwcode==702 & year < 1991),
    !(gwcode==703 & year < 1991),
    !(gwcode==704 & year < 1991),
    !(gwcode==705 & year < 1991)
    ) %>%
  bind_rows(., ussr)
```

#### Sudan/South Sudan

South Sudan independent on . So use combined for 2011 and before.

``` r
sud <- rowSums(cbind(un$pop[un$gwcode==625 & un$year <= 2011],
                     un$pop[un$gwcode==626 & un$year <= 2011]))
un$pop[un$gwcode==625 & un$year <= 2011] <- sud
```

#### Indonesia/East Timor

East Timor gained independence in 2002, so use combined for 2001 and
before.

``` r
ind <- rowSums(cbind(un$pop[un$gwcode==850 & un$year <= 2001],
                     un$pop[un$gwcode==860 & un$year <= 2001]))
un$pop[un$gwcode==850 & un$year <= 2001] <- ind
```

#### Check left-over discrepancies

Unions like GDR joining FRG are problematic and have to be fixed
outside.

- German re-unification
- Yemeni unification
- Vietnamese unification

Other discrepancies in the data:

``` r
# UPDATE: end_year
end_year <- 2025

gw <- state_panel(1950, end_year, partial = "any")
gw_not_in_un <- gw %>%
  anti_join(un, by = c("gwcode", "year")) %>%
  group_by(gwcode) %>%
  mutate(seq = id_date_sequence(year, "year")) %>%
  group_by(gwcode, seq) %>%
  summarize(years = paste0(range(year), collapse = " - "),
            .groups = "drop") %>%
  left_join(cnames, by = "gwcode")
gw_not_in_un %>%
  knitr::kable(caption = "GW CYs not in UN")
```

| gwcode | seq | years       | country_name                |
|-------:|----:|:------------|:----------------------------|
|    265 |   1 | 1950 - 1990 | German Democratic Republic  |
|    396 |   1 | 2008 - 2025 | Abkhazia                    |
|    397 |   1 | 2008 - 2025 | South Ossetia               |
|    511 |   1 | 1963 - 1964 | Zanzibar                    |
|    680 |   1 | 1967 - 1990 | Yemen, People’s Republic of |
|    711 |   1 | 1950 - 1950 | Tibet                       |
|    817 |   1 | 1954 - 1975 | Vietnam, Republic of        |

GW CYs not in UN

``` r
un_not_in_gw <- un %>%
  anti_join(gw, by = c("gwcode", "year")) %>%
  group_by(gwcode) %>%
  mutate(seq = id_date_sequence(year, "year")) %>%
  group_by(gwcode, seq) %>%
  summarize(years = paste0(range(year), collapse = " - "),
            .groups = "drop") %>%
  left_join(cnames, by = "gwcode")
un_not_in_gw %>%
  knitr::kable(caption = "UN CYs not in GW")
```

| gwcode | seq | years       | country_name                          |
|-------:|----:|:------------|:--------------------------------------|
|     31 |   1 | 1950 - 1972 | Bahamas                               |
|     51 |   1 | 1950 - 1961 | Jamaica                               |
|     52 |   1 | 1950 - 1961 | Trinidad and Tobago                   |
|     53 |   1 | 1950 - 1965 | Barbados                              |
|     54 |   1 | 1950 - 1977 | Dominica                              |
|     55 |   1 | 1950 - 1973 | Grenada                               |
|     56 |   1 | 1950 - 1978 | Saint Lucia                           |
|     57 |   1 | 1950 - 1978 | Saint Vincent and the Grenadines      |
|     58 |   1 | 1950 - 1980 | Antigua & Barbuda                     |
|     60 |   1 | 1950 - 1982 | Saint Kitts and Nevis                 |
|     80 |   1 | 1950 - 1980 | Belize                                |
|    110 |   1 | 1950 - 1965 | Guyana                                |
|    115 |   1 | 1950 - 1974 | Surinam                               |
|    338 |   1 | 1950 - 1963 | Malta                                 |
|    347 |   1 | 1950 - 2007 | Kosovo                                |
|    352 |   1 | 1950 - 1959 | Cyprus                                |
|    359 |   1 | 1950 - 1990 | Moldova                               |
|    402 |   1 | 1950 - 1974 | Cape Verde                            |
|    403 |   1 | 1950 - 1974 | Sao Tome and Principe                 |
|    404 |   1 | 1950 - 1973 | Guinea-Bissau                         |
|    411 |   1 | 1950 - 1967 | Equatorial Guinea                     |
|    420 |   1 | 1950 - 1964 | Gambia                                |
|    432 |   1 | 1950 - 1959 | Mali                                  |
|    433 |   1 | 1950 - 1959 | Senegal                               |
|    434 |   1 | 1950 - 1959 | Benin                                 |
|    435 |   1 | 1950 - 1959 | Mauritania                            |
|    436 |   1 | 1950 - 1959 | Niger                                 |
|    437 |   1 | 1950 - 1959 | Cote D’Ivoire                         |
|    438 |   1 | 1950 - 1957 | Guinea                                |
|    439 |   1 | 1950 - 1959 | Burkina Faso (Upper Volta)            |
|    451 |   1 | 1950 - 1960 | Sierra Leone                          |
|    452 |   1 | 1950 - 1956 | Ghana                                 |
|    461 |   1 | 1950 - 1959 | Togo                                  |
|    471 |   1 | 1950 - 1959 | Cameroon                              |
|    475 |   1 | 1950 - 1959 | Nigeria                               |
|    481 |   1 | 1950 - 1959 | Gabon                                 |
|    482 |   1 | 1950 - 1959 | Central African Republic              |
|    483 |   1 | 1950 - 1959 | Chad                                  |
|    484 |   1 | 1950 - 1959 | Congo                                 |
|    490 |   1 | 1950 - 1959 | Congo, Democratic Republic of (Zaire) |
|    500 |   1 | 1950 - 1961 | Uganda                                |
|    501 |   1 | 1950 - 1962 | Kenya                                 |
|    510 |   1 | 1950 - 1960 | Tanzania/Tanganyika                   |
|    516 |   1 | 1950 - 1961 | Burundi                               |
|    517 |   1 | 1950 - 1961 | Rwanda                                |
|    520 |   1 | 1950 - 1959 | Somalia                               |
|    522 |   1 | 1950 - 1976 | Djibouti                              |
|    531 |   1 | 1950 - 1992 | Eritrea                               |
|    540 |   1 | 1950 - 1974 | Angola                                |
|    541 |   1 | 1950 - 1974 | Mozambique                            |
|    551 |   1 | 1950 - 1963 | Zambia                                |
|    552 |   1 | 1950 - 1964 | Zimbabwe (Rhodesia)                   |
|    553 |   1 | 1950 - 1963 | Malawi                                |
|    565 |   1 | 1950 - 1989 | Namibia                               |
|    570 |   1 | 1950 - 1965 | Lesotho                               |
|    571 |   1 | 1950 - 1965 | Botswana                              |
|    572 |   1 | 1950 - 1967 | Swaziland                             |
|    580 |   1 | 1950 - 1959 | Madagascar                            |
|    581 |   1 | 1950 - 1974 | Comoros                               |
|    590 |   1 | 1950 - 1967 | Mauritius                             |
|    591 |   1 | 1950 - 1975 | Seychelles                            |
|    600 |   1 | 1950 - 1955 | Morocco                               |
|    615 |   1 | 1950 - 1961 | Algeria                               |
|    616 |   1 | 1950 - 1955 | Tunisia                               |
|    620 |   1 | 1950 - 1950 | Libya                                 |
|    625 |   1 | 1950 - 1955 | Sudan                                 |
|    626 |   1 | 1950 - 2010 | South Sudan                           |
|    690 |   1 | 1950 - 1960 | Kuwait                                |
|    692 |   1 | 1950 - 1970 | Bahrain                               |
|    694 |   1 | 1950 - 1970 | Qatar                                 |
|    696 |   1 | 1950 - 1970 | United Arab Emirates                  |
|    771 |   1 | 1950 - 1970 | Bangladesh                            |
|    781 |   1 | 1950 - 1964 | Maldives                              |
|    811 |   1 | 1950 - 1952 | Cambodia (Kampuchea)                  |
|    812 |   1 | 1950 - 1953 | Laos                                  |
|    816 |   1 | 1950 - 1953 | Vietnam, Democratic Republic of       |
|    820 |   1 | 1950 - 1956 | Malaysia                              |
|    830 |   1 | 1950 - 1964 | Singapore                             |
|    835 |   1 | 1950 - 1983 | Brunei                                |
|    860 |   1 | 1950 - 2001 | East Timor                            |
|    910 |   1 | 1950 - 1974 | Papua New Guinea                      |
|    935 |   1 | 1950 - 1979 | Vanuatu                               |
|    940 |   1 | 1950 - 1977 | Solomon Islands                       |
|    950 |   1 | 1950 - 1969 | Fiji                                  |
|    970 |   1 | 1950 - 1978 | Kiribati                              |
|    971 |   1 | 1950 - 1967 | Nauru                                 |
|    972 |   1 | 1950 - 1969 | Tonga                                 |
|    973 |   1 | 1950 - 1977 | Tuvalu                                |
|    983 |   1 | 1950 - 1985 | Marshall Islands                      |
|    986 |   1 | 1950 - 1993 | Palau                                 |
|    987 |   1 | 1950 - 1985 | Federated States of Micronesia        |
|    990 |   1 | 1950 - 1961 | Samoa/Western Samoa                   |

UN CYs not in GW

``` r
plot_missing(un, x = "pop", ccode = "gwcode", time = "year", statelist = "GW")
```

![](clean-population_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
# Combine pop sources -----------------------------------------------------
```

## Combine and overlap

``` r
ksg2 <- ksg %>%
  rename(gwcode = idnum, pop_ksg = pop, source_ksg = source) %>%
  select(gwcode, year, pop_ksg, source_ksg)
wdi2 <- wdi %>%
  rename(pop_wdi = pop) %>%
  select(gwcode, year, pop_wdi)
un2 <- un %>%
  rename(pop_un = pop) %>%
  select(gwcode, year, pop_un)
joint <- list(ksg2, wdi2, un2) %>%
  purrr::reduce(full_join, by = c("gwcode", "year")) %>%
  tidyr::pivot_longer(names_to = "source", values_to = "pop", -c(gwcode, year, source_ksg)) %>%
  # add an indicator for whether the sources have overlapping coverage
  group_by(gwcode, year) %>%
  mutate(overlap = !any(is.na(pop))) %>%
  ungroup()

joint_wide <- joint |>
  pivot_wider(names_from = source, values_from = pop)
```

The next plot shows the UN, WDI, and KSG population series for each
country.

``` r
ggplot(joint, aes(x = year, y = pop, group = interaction(gwcode, source),
                  color = source)) +
  geom_line() +
  scale_y_log10() +
  theme_minimal()
```

    ## Warning: Removed 26100 rows containing missing values or values outside the scale range (`geom_line()`).

![](clean-population_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

There are quite some divergences. The UN and WDI data seem to generally
be much more smooth than the KSG data, which has sometimes drastic
shifts. I would say the UN values would be preferable where possible,
they are more smooth than WDI and are easier to use to reconstruct
historical state unions like Yugoslavia that are now several states.

Try to sample some series to get a better look.

``` r
countries <- c(2, 260, 344, 345, 365, 436, 540, 645, 651, 678, 710)
joint %>%
  filter(year > 1959) %>%
  filter(gwcode %in% countries) %>%
  left_join(cnames, by = c("gwcode")) %>%
  ggplot(aes(x = year, y = pop, color = source)) +
  facet_wrap(~ country_name, scales = "free_y") +
  geom_line() +
  theme_minimal()
```

    ## Warning: Removed 80 rows containing missing values or values outside the scale range (`geom_line()`).

![](clean-population_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

So it seems that UN is generally preferable, except for cases like
Germany before 1990, where historical data were adjusted in WDI to
ignore country changes. There it would be preferable to use KSG.

### Within country covariances between UN and KSG

Check the country correlations.

``` r
cors <- joint %>%
  spread(source, pop) %>%
  group_by(gwcode) %>%
  summarize(cor = tryCatch(
    cor(pop_ksg, pop_un, use = "complete.obs"),
    error = function(e) NA_real_))

ggplot(cors, aes(x = cor)) +
  geom_histogram(binwidth = 0.1) +
  theme_minimal()
```

    ## Warning: Removed 32 rows containing non-finite outside the scale range (`stat_bin()`).

![](clean-population_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

### Countries where UN, KSG, WDI do not agree

Most of them are 0.9 or higher. What about the exceptions?

``` r
countries <- filter(cors, cor < 0.85) %>% pull(gwcode)
joint %>%
  filter(year > 1959) %>%
  filter(gwcode %in% countries) %>%
  left_join(cnames, by = c("gwcode")) %>%
  ggplot(aes(x = year, y = pop, color = source)) +
  facet_wrap(~ country_name, scales = "free_y") +
  geom_line() +
  theme_minimal()
```

    ## Warning: Removed 64 rows containing missing values or values outside the scale range (`geom_line()`).

![](clean-population_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

UN seems fine on these except Germany pre-1990, USSR pre-1990, and
Pakistan pre-1971.

### Can we combine KSG for pre-50 with UN?

Check to see how well they are aligned.

``` r
# Only look at countries where GW is meeting UN; UN has too many
weld_countries <- joint %>%
  filter(year==1949 & source == "pop_ksg") %>%
  pull(gwcode)
weld <- joint %>%
  filter(year > 1945 & year < 1956 & gwcode %in% weld_countries) %>%
  left_join(cnames, by = c("gwcode")) %>%
  filter(source!="pop_wdi") %>%
  spread(source, pop) %>%
  mutate(diff = (pop_ksg - pop_un) / (.5*(pop_ksg+pop_un)))

# Countries with divergence in meeting up year
lookat <- filter(weld, year==1950 & abs(diff) > .05) %>% pull(gwcode)

weld %>%
  gather(source, pop, pop_ksg, pop_un) %>%
  filter(gwcode %in% lookat) %>%
  ggplot(., aes(x = year, y = pop, color = source, group = interaction(source, gwcode))) +
  geom_line() +
  theme_minimal() +
  scale_y_log10()
```

    ## Warning: Removed 104 rows containing missing values or values outside the scale range (`geom_line()`).

![](clean-population_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

Some divergences greater than 5%, but let’s gloss over those for now.
\## Start imputing/combining

Take UN as the preferable source, but drop in KSG for known deviations
and pre-1950. For Kosovo we use WDI.

``` r
master <- state_panel(1816, max(joint$year), partial = "any") %>%
  mutate(gw = TRUE)
pop <- joint_wide %>%
  mutate(
    pop = case_when(
      year > 1949 ~ pop_un,
      year < 1950 ~ pop_ksg,
      TRUE ~ NA_real_),
    source = case_when(
      year > 1949 ~ "un",
      year < 1950 ~ "ksg",
      TRUE ~ NA_character_)
  ) %>%
  right_join(., master, by = c("gwcode", "year")) %>%
  replace_na(list(gw = FALSE))
```

### Germany

``` r
joint_wide %>%
  filter(gwcode==265 & year > 1985 & year < 1995)
```

    ## # A tibble: 5 × 7
    ##   gwcode  year source_ksg overlap pop_ksg pop_wdi pop_un
    ##    <dbl> <dbl>      <dbl> <lgl>     <dbl>   <dbl>  <dbl>
    ## 1    265  1986          0 FALSE     16624      NA     NA
    ## 2    265  1987          0 FALSE     16641      NA     NA
    ## 3    265  1988          0 FALSE     16666      NA     NA
    ## 4    265  1989          0 FALSE     16630      NA     NA
    ## 5    265  1990          0 FALSE     16247      NA     NA

``` r
## use 1990 and before KSG for Germany
idx <- pop$gwcode==265 & pop$year <= 1990
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"
```

### Vietnam

``` r
joint_wide %>%
  filter(gwcode==816 & year > 1970 & year < 1980)
```

    ## # A tibble: 9 × 7
    ##   gwcode  year source_ksg overlap pop_ksg pop_wdi pop_un
    ##    <dbl> <dbl>      <dbl> <lgl>     <dbl>   <dbl>  <dbl>
    ## 1    816  1971          0 FALSE     21595     NA  42449.
    ## 2    816  1972          0 FALSE     22038     NA  43429.
    ## 3    816  1973          0 FALSE     22481     NA  44410.
    ## 4    816  1974          0 FALSE     23244     NA  45414.
    ## 5    816  1975          0 TRUE      24032  46483. 46483.
    ## 6    816  1976          0 TRUE      49160  47685. 47685.
    ## 7    816  1977          0 TRUE      50413  48955. 48955.
    ## 8    816  1978          0 TRUE      51423  50250. 50250.
    ## 9    816  1979          0 TRUE      52462  51378. 51378.

``` r
## use 1974 and before KSG for DRV
idx <- pop$gwcode==816 & pop$year <= 1974
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"


joint_wide %>%
  filter(gwcode==817 & year > 1970 & year < 1980)
```

    ## # A tibble: 5 × 7
    ##   gwcode  year source_ksg overlap pop_ksg pop_wdi pop_un
    ##    <dbl> <dbl>      <dbl> <lgl>     <dbl>   <dbl>  <dbl>
    ## 1    817  1971          0 FALSE     18810      NA     NA
    ## 2    817  1972          0 FALSE     19086      NA     NA
    ## 3    817  1973          0 FALSE     19367      NA     NA
    ## 4    817  1974          0 FALSE     19652      NA     NA
    ## 5    817  1975          0 FALSE     19941      NA     NA

``` r
## use 1975 and before KSG for RV
idx <- pop$gwcode==817 & pop$year <= 1975
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"
```

### Yemen

``` r
joint_wide %>%
  filter(gwcode==678 & year > 1985 & year < 1995)
```

    ## # A tibble: 9 × 7
    ##   gwcode  year source_ksg overlap pop_ksg pop_wdi pop_un
    ##    <dbl> <dbl>      <dbl> <lgl>     <dbl>   <dbl>  <dbl>
    ## 1    678  1986          0 FALSE      7911     NA  11901.
    ## 2    678  1987          0 FALSE      8213     NA  12370.
    ## 3    678  1988          0 FALSE      8529     NA  12860.
    ## 4    678  1989          0 FALSE      8857     NA  13364.
    ## 5    678  1990          0 TRUE       9196  13888. 13888.
    ## 6    678  1991          0 TRUE      11613  14430. 14430.
    ## 7    678  1992          0 TRUE      11952  14989. 14989.
    ## 8    678  1993          0 TRUE      12302  15564. 15564.
    ## 9    678  1994          0 TRUE      14859  16149. 16149.

``` r
## use 1989 and before KSG for north Yemen
idx <- pop$gwcode==678 & pop$year <= 1989
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"


joint_wide %>%
  filter(gwcode==680 & year > 1985 & year < 1995)
```

    ## # A tibble: 5 × 7
    ##   gwcode  year source_ksg overlap pop_ksg pop_wdi pop_un
    ##    <dbl> <dbl>      <dbl> <lgl>     <dbl>   <dbl>  <dbl>
    ## 1    680  1986          0 FALSE      2220      NA     NA
    ## 2    680  1987          0 FALSE      2278      NA     NA
    ## 3    680  1988          0 FALSE      2337      NA     NA
    ## 4    680  1989          0 FALSE      2398      NA     NA
    ## 5    680  1990          0 FALSE      2460      NA     NA

``` r
## use KSG for south Yemen
idx <- pop$gwcode==680 & pop$year <= 1990
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"
```

### Tibet

Tibet is missing in UN data, so use KSG as with 1949 and before.

``` r
joint_wide %>%
  filter(gwcode==711 & year > 1945 & year < 1955)
```

    ## # A tibble: 5 × 7
    ##   gwcode  year source_ksg overlap pop_ksg pop_wdi pop_un
    ##    <dbl> <dbl>      <dbl> <lgl>     <dbl>   <dbl>  <dbl>
    ## 1    711  1946          2 FALSE     1708.      NA     NA
    ## 2    711  1947          1 FALSE     1700       NA     NA
    ## 3    711  1948          2 FALSE     1651.      NA     NA
    ## 4    711  1949          2 FALSE     1604.      NA     NA
    ## 5    711  1950          2 FALSE     1558.      NA     NA

``` r
# Use KSG for 1950 as well
idx <- pop$gwcode==711 & pop$year == 1950
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"
```

### Czechoslovakia

Missing first year (1918), backwards impute.

``` r
joint_wide %>%
  filter(gwcode==315 & year < 1921)
```

    ## # A tibble: 2 × 7
    ##   gwcode  year source_ksg overlap pop_ksg pop_wdi pop_un
    ##    <dbl> <dbl>      <dbl> <lgl>     <dbl>   <dbl>  <dbl>
    ## 1    315  1919          0 FALSE     13398      NA     NA
    ## 2    315  1920          0 FALSE     13530      NA     NA

``` r
pop %>%
  filter(gwcode==315 & year < 1921)
```

    ## # A tibble: 3 × 10
    ##   gwcode  year source_ksg overlap pop_ksg pop_wdi pop_un   pop source gw   
    ##    <dbl> <dbl>      <dbl> <lgl>     <dbl>   <dbl>  <dbl> <dbl> <chr>  <lgl>
    ## 1    315  1919          0 FALSE     13398      NA     NA 13398 ksg    TRUE 
    ## 2    315  1920          0 FALSE     13530      NA     NA 13530 ksg    TRUE 
    ## 3    315  1918         NA NA           NA      NA     NA    NA <NA>   TRUE

Plot the pop series:

``` r
idx <- pop$gwcode==315
csk <- pop$pop_ksg[idx]
plot(pop$year[idx], csk)
```

![](clean-population_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

It is quite jumpy, so use only pre-1937

``` r
idx2 <- pop$gwcode==315 & pop$year <= 1937
csk  <- pop$pop_ksg[idx2]
csk  <- rev(imputeTS::na_kalman(rev(csk), "auto.arima"))
pop$pop[idx2]   <- csk
pop$source[idx2] <- "ksg"

# Verify via plot
idx <- pop$gwcode==315
csk <- pop$pop[idx]
plot(pop$year[idx], csk)
```

![](clean-population_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
# Prepare to write out data -----------------------------------------------
```

## Get ready to write final data

### Check values for splitting/joining countries

``` r
countries <- c(260, 265,
               345, 340, 341, 343, 344, 346, 347, 349,
               816, 817,
               678, 680,
               365, 366, 367, 368)
pop %>%
  filter(gwcode %in% countries) %>%
  left_join(cnames, by = c("gwcode")) %>%
  ggplot(aes(x = year, y = pop)) +
  facet_wrap(~ country_name, scales = "free", ncol = 4) +
  geom_line() +
  theme_minimal()
```

![](clean-population_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

### Missingness

``` r
plot_missing(pop, x = "pop", ccode = "gwcode", time = "year", statelist = "GW")
```

![](clean-population_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

The final data is complete for 1950 to 2019, except for Abkhazia, South
Ossetia, and Zanzibar.

``` r
pop %>%
  filter(is.na(pop)) %>%
  group_by(gwcode) %>%
  summarize(years = paste0(range(year), collapse = " - "), N = n()) %>%
  mutate(country_name = country_names(gwcode)) %>%
  knitr::kable()
```

| gwcode | years       |   N | country_name  |
|-------:|:------------|----:|:--------------|
|    396 | 2008 - 2025 |  18 | Abkhazia      |
|    397 | 2008 - 2025 |  18 | South Ossetia |
|    511 | 1963 - 1964 |   2 | Zanzibar      |

``` r
pop %>%
  select(gwcode, year, pop) %>%
  write_csv(., path = "output/population.csv")


setwd(oldwd)
```
