Combined GDP data
================

*Last updated on: 2022-02-07*

GDP data from 1950 on, mostly based on WDI, with some gaps filled with
KSG’s extended GDP data and data from the UN.

Four step imputation procedure:

1.  Acquire the WDI data
2.  Where WDI is missing, drop in UN GDP figures, scaled by a linear
    model.
3.  Where WDI is missing, drop in KSG figures, scaled by a log-linear
    country-varying scaling model.
4.  Model-based extrapolation: use Kalman-smoothing to forward
    extrapolate missing GDP values (most notably Taiwan and several
    countries missing current year GDP values) and backward extrapolate
    GDP growth in first year of existences of a country.

## Overview

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(yaml)
library(tidyr)

gdp <- read.csv("output/gdp.csv")
str(gdp)
```

    ## 'data.frame':    11400 obs. of  6 variables:
    ##  $ gwcode           : int  2 20 40 41 42 70 90 91 92 93 ...
    ##  $ year             : int  1950 1950 1950 1950 1950 1950 1950 1950 1950 1950 ...
    ##  $ NY.GDP.MKTP.KD   : num  2.37e+12 1.47e+11 1.65e+10 5.07e+09 2.15e+09 ...
    ##  $ NY.GDP.MKTP.KD.ZG: num  3.942 5.669 0.933 3.071 0 ...
    ##  $ NY.GDP.PCAP.KD   : num  14927 10729 2794 1573 910 ...
    ##  $ NY.GDP.PCAP.KD.ZG: num  2.451 3.261 0.241 2.116 0 ...

``` r
head(gdp)
```

    ##   gwcode year NY.GDP.MKTP.KD NY.GDP.MKTP.KD.ZG NY.GDP.PCAP.KD NY.GDP.PCAP.KD.ZG
    ## 1      2 1950   2.370483e+12         3.9424644     14927.0980         2.4510849
    ## 2     20 1950   1.473373e+11         5.6685979     10728.7060         3.2606498
    ## 3     40 1950   1.654004e+10         0.9328612      2793.9264         0.2411519
    ## 4     41 1950   5.066289e+09         3.0708620      1572.8933         2.1162749
    ## 5     42 1950   2.152813e+09         0.0000000       910.2805         0.0000000
    ## 6     70 1950   7.546493e+10         6.5228341      2700.4805         3.2988478

``` r
stats <- yaml::read_yaml("output/gdp-signature.yml")
stats
```

    ## $Class
    ## [1] "tbl_df, tbl, data.frame"
    ## 
    ## $Size_in_mem
    ## [1] "0.8 Mb"
    ## 
    ## $N_countries
    ## [1] 204
    ## 
    ## $Years
    ## [1] "1950 - 2020"
    ## 
    ## $N_columns
    ## [1] 6
    ## 
    ## $Columns
    ## [1] "gwcode, year, NY.GDP.MKTP.KD, NY.GDP.MKTP.KD.ZG, NY.GDP.PCAP.KD, NY.GDP.PCAP.KD.ZG"
    ## 
    ## $N_rows
    ## [1] 11400
    ## 
    ## $N_complete_rows
    ## [1] 11370

``` r
gdp %>%
  pivot_longer(-one_of("gwcode", "year")) %>%
  ggplot(., aes(x = year, y = value, group = gwcode)) +
  facet_wrap(~ name, ncol = 1, scales = "free_y") +
  geom_line(alpha = .2) +
  theme_minimal()
```

    ## Warning: Removed 57 row(s) containing missing values (geom_path).

![](README_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->
