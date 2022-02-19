Clean EPR data
================

*Last compiled on: 2022-02-19*

``` r
raw <- read_csv("data-raw/EPR-2021.csv",
                col_types = cols(
                  gwid = col_integer(),
                  statename = col_character(),
                  from = col_integer(),
                  to = col_integer(),
                  group = col_character(),
                  groupid = col_double(),
                  gwgroupid = col_double(),
                  umbrella = col_integer(),
                  size = col_double(),
                  status = col_character(),
                  reg_aut = col_logical()
                ))

raw <- raw %>%
    rename(gwcode = gwid) %>%
    mutate(reg_aut = case_when(
      is.na(reg_aut) ~ FALSE,
      TRUE ~ reg_aut
    ))
  raw
```

    ## # A tibble: 4,339 × 11
    ##    gwcode statename   from    to group  groupid gwgroupid umbrella   size status
    ##     <int> <chr>      <int> <int> <chr>    <dbl>     <dbl>    <int>  <dbl> <chr> 
    ##  1      2 United St…  1946  1965 Whites    1000    201000       NA 0.691  MONOP…
    ##  2      2 United St…  1946  1965 Afric…    3000    203000       NA 0.124  DISCR…
    ##  3      2 United St…  1946  1965 Ameri…    5000    205000       NA 0.0078 POWER…
    ##  4      2 United St…  1966  2008 Whites    1000    201000       NA 0.691  DOMIN…
    ##  5      2 United St…  1966  2008 Latin…    2000    202000       NA 0.125  POWER…
    ##  6      2 United St…  1966  2008 Afric…    3000    203000       NA 0.124  POWER…
    ##  7      2 United St…  1966  2008 Asian…    4000    204000       NA 0.036  POWER…
    ##  8      2 United St…  1966  2008 Ameri…    5000    205000       NA 0.0078 POWER…
    ##  9      2 United St…  1966  2008 Arab …    6000    206000       NA 0.0042 POWER…
    ## 10      2 United St…  2009  2014 Whites    1000    201000       NA 0.66   SENIO…
    ## # … with 4,329 more rows, and 1 more variable: reg_aut <lgl>

``` r
# convert to country-year
epr <- raw %>%
  dplyr::mutate(year = min(raw$from)) %>%
  tidyr::complete(year = seq(min(from), max(to)),
                  nesting(gwcode, from, to, group, groupid, size, status, reg_aut)) %>%
  dplyr::filter(year >= from & year <= to)

# not clear about irrelevant and state collapse
excl_val <- c("POWERLESS", "DISCRIMINATED", "SELF-EXCLUSION")
incl_val <- c("MONOPOLY", "DOMINANT", "JUNIOR PARTNER", "SENIOR PARTNER",
              "IRRELEVANT", "STATE COLLAPSE")

epr <- epr %>%
  group_by(gwcode, year) %>%
  dplyr::summarize(
    groups = n(),
    elf    = sum(size^2),
    excluded_groups_count = sum(status %in% excl_val),
    excluded_group_pop    = sum(subset(size, status %in% excl_val)),
    inpower_groups_count  = sum(status %in% incl_val),
    inpower_groups_pop    = sum(subset(size, status %in% incl_val)),
    regaut_groups_count = sum(reg_aut %in% TRUE),
    regaut_group_pop    = sum(subset(size, reg_aut %in% TRUE))
  )
```

    ## `summarise()` has grouped output by 'gwcode'. You can override using the `.groups` argument.

``` r
master <- state_panel(min(epr$year), max(epr$year), by = "year", partial = "any")

comp <- states::compare(epr, master)
report(comp)
```

    ## 11917 total rows
    ## 10839 rows in df1
    ## 11916 rows in df2
    ## 
    ## 10838 rows match and have no missing values
    ## 2-1946, 2-1947, 2-1948, 2-1949, 2-1950, 2-1951, 2-1952, 2-1953, 2-1954, 2-1955, and 10828 more
    ## 
    ## 1 rows in df1 (no missing values) but not df2
    ## 812-1953
    ## 
    ## 1078 rows not in df1 but in df2 (no missing values)
    ## 54-1978, 54-1979, 54-1980, 54-1981, 54-1982, 54-1983, 54-1984, 54-1985, 54-1986, 54-1987, and 1068 more

The following cases are not present in the country-year EPR data:

``` r
data(gwstates)

comp %>% 
  filter(case_in_df1==0, case_in_df2==1) %>%
  group_by(gwcode) %>%
  summarize(n = n(), years = paste0(min(year), "-", max(year))) %>%
  left_join(gwstates[, c("gwcode", "country_name", "microstate")]) %>%
  mutate(microstate = as.integer(microstate)) %>%
  select(country_name, everything()) %>%
  knitr::kable()
```

    ## Joining, by = "gwcode"

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

All are microstates. So this is ok.

``` r
epr <- left_join(master, epr)
```

    ## Joining, by = c("gwcode", "year")

``` r
sapply(epr, function(x) sum(is.na(x))) %>%
  enframe(name = "Variable", value = "N_missing")
```

    ## # A tibble: 10 × 2
    ##    Variable              N_missing
    ##    <chr>                     <int>
    ##  1 gwcode                        0
    ##  2 year                          0
    ##  3 groups                     1078
    ##  4 elf                        1078
    ##  5 excluded_groups_count      1078
    ##  6 excluded_group_pop         1078
    ##  7 inpower_groups_count       1078
    ##  8 inpower_groups_pop         1078
    ##  9 regaut_groups_count        1078
    ## 10 regaut_group_pop           1078

``` r
# Add 'epr_' prefix to non-ID columns
idx <- !colnames(epr) %in% c("gwcode", "year")
colnames(epr)[idx] <- paste0("epr_", colnames(epr)[idx])
```

``` r
stats <- list(
  rows = nrow(epr),
  cols = ncol(epr),
  complete_rows = sum(complete.cases(epr)),
  year_coverage = paste0(range(epr$year), collapse = " - "),
  n_countries = length(unique(epr[complete.cases(epr), ][["gwcode"]])),
  col_names = paste0(colnames(epr), collapse = ", ")
)

write_yaml(stats, "output/stats-epr.yml")

write_csv(epr, "output/epr.csv")
```
