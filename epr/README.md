Ethnic Power Relations (EPR)
================

*Last compiled on: 2022-02-19*

Source: <https://icr.ethz.ch/data/epr/core/>

License: not clear

Citation: Vogt, Manuel, Nils-Christian Bormann, Seraina Rüegger,
Lars-Erik Cederman, Philipp Hunziker, and Luc Girardin. 2015.
“Integrating Data on Ethnicity, Geography, and Conflict: The Ethnic
Power Relations Data Set Family.” Journal of Conflict Resolution 59(7):
1327–42.

Coverage: Yearly resolution. Covers all major G&W countries.

Lagged: no

------------------------------------------------------------------------

To update:

1.  Download and get EPR data from the URL above.

2.  Run the `clean-data.Rmd` script (interactively!) and adjust as
    needed base on new facts on the ground.

3.  The main output is “output/epr.csv”.

## Usage

``` r
library(readr)
library(dplyr)
library(states)
library(yaml)

data(gwstates)

epr <- read_csv("output/epr.csv")

glimpse(epr)
```

    ## Rows: 11,916
    ## Columns: 10
    ## $ gwcode                    <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    ## $ year                      <dbl> 1946, 1947, 1948, 1949, 1950, 1951, 1952, 19…
    ## $ epr_groups                <dbl> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,…
    ## $ epr_elf                   <dbl> 0.4929178, 0.4929178, 0.4929178, 0.4929178, …
    ## $ epr_excluded_groups_count <dbl> 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,…
    ## $ epr_excluded_group_pop    <dbl> 0.1318, 0.1318, 0.1318, 0.1318, 0.1318, 0.13…
    ## $ epr_inpower_groups_count  <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ epr_inpower_groups_pop    <dbl> 0.691, 0.691, 0.691, 0.691, 0.691, 0.691, 0.…
    ## $ epr_regaut_groups_count   <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,…
    ## $ epr_regaut_group_pop      <dbl> 0.0078, 0.0078, 0.0078, 0.0078, 0.0078, 0.00…

``` r
stats <- read_yaml("output/stats-epr.yml")
stats
```

    ## $rows
    ## [1] 11916
    ## 
    ## $cols
    ## [1] 10
    ## 
    ## $complete_rows
    ## [1] 10838
    ## 
    ## $year_coverage
    ## [1] "1946 - 2021"
    ## 
    ## $n_countries
    ## [1] 181
    ## 
    ## $col_names
    ## [1] "gwcode, year, epr_groups, epr_elf, epr_excluded_groups_count, epr_excluded_group_pop, epr_inpower_groups_count, epr_inpower_groups_pop, epr_regaut_groups_count, epr_regaut_group_pop"

### Missing cases

``` r
# Missing values
incomplete <- epr %>% 
  filter(!complete.cases(.)) %>%
  group_by(gwcode) %>%
  summarize(n = n(), years = paste0(min(year), "-", max(year))) %>%
  left_join(gwstates[, c("gwcode", "country_name", "microstate")]) %>%
  mutate(microstate = as.integer(microstate)) %>%
  select(country_name, everything()) 
```

    ## Joining, by = "gwcode"

``` r
# Total number of incomplete rows
sum(incomplete$n)
```

    ## [1] 1078

``` r
incomplete %>%
  knitr::kable()
```

| country_name                     | gwcode |   n | years     | microstate |
|:---------------------------------|-------:|----:|:----------|-----------:|
| Dominica                         |     54 |  44 | 1978-2021 |          1 |
| Grenada                          |     55 |  48 | 1974-2021 |          1 |
| Saint Lucia                      |     56 |  43 | 1979-2021 |          1 |
| Saint Vincent and the Grenadines |     57 |  43 | 1979-2021 |          1 |
| Antigua & Barbuda                |     58 |  41 | 1981-2021 |          1 |
| Saint Kitts and Nevis            |     60 |  39 | 1983-2021 |          1 |
| Monaco                           |    221 |  76 | 1946-2021 |          1 |
| Liechtenstein                    |    223 |  76 | 1946-2021 |          1 |
| Andorra                          |    232 |  76 | 1946-2021 |          1 |
| San Marino                       |    331 |  76 | 1946-2021 |          1 |
| Abkhazia                         |    396 |  14 | 2008-2021 |          1 |
| South Ossetia                    |    397 |  14 | 2008-2021 |          1 |
| Sao Tome and Principe            |    403 |  47 | 1975-2021 |          1 |
| Seychelles                       |    591 |  46 | 1976-2021 |          1 |
| Vanuatu                          |    935 |  42 | 1980-2021 |          1 |
| Kiribati                         |    970 |  43 | 1979-2021 |          1 |
| Nauru                            |    971 |  54 | 1968-2021 |          1 |
| Tonga                            |    972 |  52 | 1970-2021 |          1 |
| Tuvalu                           |    973 |  44 | 1978-2021 |          1 |
| Marshall Islands                 |    983 |  36 | 1986-2021 |          1 |
| Palau                            |    986 |  28 | 1994-2021 |          1 |
| Federated States of Micronesia   |    987 |  36 | 1986-2021 |          1 |
| Samoa/Western Samoa              |    990 |  60 | 1962-2021 |          1 |
