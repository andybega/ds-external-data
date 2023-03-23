#' ---
#' title: "Population"
#' author: "Author: Andreas Beger\n\n"
#' date: "`r paste0('Last updated on: ', format(Sys.Date(), '%d %B %Y'))`"
#' output: 
#'   github_document:
#'    toc: yes
#'---

#' NOTE: This file is generated from README.R. To spint/knit/compile the .md 
#' file, run:  
#' `setwd("population"); rmarkdown::render("clean-population.R")`
#'
#' This file combines UN, WDI, and KSG population data to create a complete coverage dataset for 1950 onwards, with no major states missing. 
#'
#' The data sources are:
#' 
#' - https://population.un.org/wpp/DataQuery/: select total population by sex and SDG regions; only 35 years can be exported at a time so three files are needed
#' - http://ksgleditsch.com/data/exppop.txt
#' - World Bank, via the WDI package
#'
#' To update the data:
#' 
#' 1. Download a new version of the UN population dataset at 
#'    https://population.un.org/dataportal/. Only 35 years can be exported at 
#'    a time so three files are needed. Select total population by sex, 
#'    countries, years needed, then search. In the resulting display, on the 
#'    display tab select Locations > Characteristics > Variants, then in 
#'    Settings select "Scaling Factor" as 'k'. This gets data in the previous
#'    format. Export to Excel and rename the output file to match the pattern in 
#'    the `input/` folder. 
#' 2. Delete "input/wdipop.csv"; it'll be downloaded and cached again. 
#' 3. Search for "UPDATE:" for lines in this document that need attention, possibly.
#' 

suppressPackageStartupMessages({
  library(dplyr)
  library(states)
  library(WDI)
  library(lubridate)
  library(readxl)
  library(kableExtra)
  library(imputeTS)
  library(here)
  library(purrr)
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

#' ## Acquire/update raw data

#' ### Expanded population data from KSG
#'
#' Data are at http://ksgleditsch.com/data/exppop.txt

if (!file.exists("input/exppop.tsv")) {
  url <- "http://ksgleditsch.com/data/exppop.txt"
  download.file(url, destfile = "input/exppop.tsv")
}

ksg <- read_tsv("input/exppop.tsv")

#' The data range from `r min(ksg$year)` to `r max(ksg$year)`. 

plot_missing(ksg, x = "pop", ccode = "idnum", time = "year", statelist = "GW")

# These data only go to 2004. Need to splice in updates. 

#' ### WDI pop data
#'
#' Via the WB API thanks to WDI package. 

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



# UN Pop data -------------------------------------------------------------

#' ### UN Pop data

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
parse_un <- function(file) {
  x <- readxl::read_xlsx(file, sheet = "Data", range = "A1")
  if (length(x)==0) {
    out <- parse_un_long(file)
  } else if (names(x)=="Total Population by sex (thousands)") {
    out <- parse_un_wide(file)
  } else {
    stop("unexpected input")
  }
  out
}

# This is the UN pop data for countries, taking out the region aggregations
un_raw <- files %>%
  # read in data
  map(., parse_un) %>%
  purrr::reduce(rbind) 

# UPDATE: drop the last year of data; i just didn't want to export a file with a 
# single year
drop_year <- 2023
un_raw <- un_raw[!un_raw$year==drop_year, ] 

# Add GW codes
un <- un_raw %>%
  mutate(gwcode = countrycode::countrycode(iso3n, "iso3n", "cown", warn = FALSE),
         gwcode = as.integer(gwcode)) %>%
  mutate(gwcode = case_when(
      Location=="Serbia" ~ 340L, 
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

#' #### Reconstruct some countries that later split
#' 
#' Since the UN data has series for all current countries going back to 1950, we 
#' can use those to re-construct countries like USSR and Yugoslavia that today 
#' are several countries.

#' #### Czechoslovakia
#'
#' Czechia and Slovakia split on 1 January 1993. 

add <- tibble(
  gwcode = 315, 
  year = 1950:1992,
  pop = rowSums(cbind(un$pop[un$gwcode==316 & un$year < 1993], 
                         un$pop[un$gwcode==317 & un$year < 1993]))
)
un <- bind_rows(un, add) %>%
  filter(!(gwcode==316 & year < 1993),
         !(gwcode==317 & year < 1993))

#' #### Pakistan pre-1971
#'
#' Before East Pakistan became Bangladesh. 

pak70 <- rowSums(cbind(un$pop[un$gwcode==770 & un$year < 1970], 
                       un$pop[un$gwcode==771 & un$year < 1970]))
un$pop[un$gwcode==770 & un$year < 1970] <- pak70


#' #### Yugoslavia/Serbia & Montenegro/Serbia
#' 
#' - 1990 and before: Slovenia, Croatia, BiH, Serbia, Kosovo, (UN does not seem to treat it separately), Montenegro, Macedonia
#' - 1991: Slovenia, Croatia, BiH, Serbia, Kosovo, (UN does not seem to treat it separately), Montenegro
#' - 1992 and until 2006: Serbia, Kosovo, Montenegro

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

un <- un %>%
  filter(!(gwcode==340 & year < 2006),
         !(gwcode==341 & year < 2006),
         !(gwcode==343 & year < 1991),
         !(gwcode==344 & year < 1992),
         !(gwcode==346 & year < 1992),
         !(gwcode==349 & year < 1992)) %>%
  bind_rows(., yugo) 

#' ##### USSR/Russia
#' 
#' - 1990 and before: Russia (365), the Baltics (366, 367, 368), Ukraine (369), Belarus (370), Armenia (371), Georgia (372), Azerbaijan (373), central Asia (701, 702, 703, 704, 705) 
#' - 1991 and on: 365

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


#' #### Sudan/South Sudan
#' 
#' South Sudan independent on `r sfind("South Sudan", "G&W")$start`. So use 
#' combined for 2011 and before. 

sud <- rowSums(cbind(un$pop[un$gwcode==625 & un$year <= 2011], 
                     un$pop[un$gwcode==626 & un$year <= 2011]))
un$pop[un$gwcode==625 & un$year <= 2011] <- sud


#' #### Indonesia/East Timor
#' 
#' East Timor gained independence in 2002, so use combined for 2001 and before. 

ind <- rowSums(cbind(un$pop[un$gwcode==850 & un$year <= 2001], 
                     un$pop[un$gwcode==860 & un$year <= 2001]))
un$pop[un$gwcode==850 & un$year <= 2001] <- ind


#' #### Check left-over discrepancies
#' 
#' Unions like GDR joining FRG are problematic and have to be fixed outside. 
#' 
#' - German re-unification
#' - Yemeni unification
#' - Vietnamese unification
#' 
#' Other discrepancies in the data:

# UPDATE: end_year
end_year <- 2022
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


plot_missing(un, x = "pop", ccode = "gwcode", time = "year", statelist = "GW")



# Combine pop sources -----------------------------------------------------

#' ## Combine and overlap


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
  tidyr::gather(source, pop, -gwcode, -year, -source_ksg) %>%
  # add an indicator for whether the sources have overlapping coverage
  group_by(gwcode, year) %>%
  mutate(overlap = !any(is.na(pop))) %>%
  ungroup()

joint_wide <- joint %>%
  spread(source, pop)


#' The next plot shows the UN, WDI, and KSG population series for each country. 


ggplot(joint, aes(x = year, y = pop, group = interaction(gwcode, source),
                  color = source)) +
  geom_line() +
  scale_y_log10() +
  theme_minimal()

#' There are quite some divergences. The UN and WDI data seem to generally be much more smooth than the KSG data, which has sometimes drastic shifts. I would say the UN values would be preferable where possible, they are more smooth than WDI and are easier to use to reconstruct historical state unions like Yugoslavia that are now several states. 
#' 
#' Try to sample some series to get a better look.

countries <- c(2, 260, 344, 345, 365, 436, 540, 645, 651, 678, 710)
joint %>%
  filter(year > 1959) %>%
  filter(gwcode %in% countries) %>%
  left_join(cnames, by = c("gwcode")) %>%
  ggplot(aes(x = year, y = pop, color = source)) +
  facet_wrap(~ country_name, scales = "free_y") +
  geom_line() + 
  theme_minimal()

#' So it seems that UN is generally preferable, except for cases like Germany before 1990, where historical data were adjusted in WDI to ignore country changes. There it would be preferable to use KSG. 
#' 
#' ### Within country covariances between UN and KSG
#' 
#' Check the country correlations.

cors <- joint %>%
  spread(source, pop) %>%
  group_by(gwcode) %>%
  summarize(cor = tryCatch(
    cor(pop_ksg, pop_un, use = "complete.obs"),
    error = function(e) NA_real_))

ggplot(cors, aes(x = cor)) +
  geom_histogram(binwidth = 0.1) +
  theme_minimal()


#' ### Countries where UN, KSG, WDI do not agree
#' 
#' Most of them are 0.9 or higher. What about the exceptions?

countries <- filter(cors, cor < 0.85) %>% pull(gwcode)
joint %>%
  filter(year > 1959) %>%
  filter(gwcode %in% countries) %>%
  left_join(cnames, by = c("gwcode")) %>%
  ggplot(aes(x = year, y = pop, color = source)) +
  facet_wrap(~ country_name, scales = "free_y") +
  geom_line() + 
  theme_minimal()

#' UN seems fine on these except Germany pre-1990, USSR pre-1990, and Pakistan pre-1971. 
#' 
#' ### Can we combine KSG for pre-50 with UN?
#' 
#' Check to see how well they are aligned. 

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

#' Some divergences greater than 5%, but let's gloss over those for now. 

#' ## Start imputing/combining
#' 
#' Take UN as the preferable source, but drop in KSG for known deviations and pre-1950. For Kosovo we use WDI. 

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



#' ### Germany

joint_wide %>%
  filter(gwcode==265 & year > 1985 & year < 1995)

## use 1990 and before KSG for Germany
idx <- pop$gwcode==265 & pop$year <= 1990
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"

#' ### Vietnam

joint_wide %>%
  filter(gwcode==816 & year > 1970 & year < 1980)

## use 1974 and before KSG for DRV
idx <- pop$gwcode==816 & pop$year <= 1974
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"


joint_wide %>%
  filter(gwcode==817 & year > 1970 & year < 1980)

## use 1975 and before KSG for RV
idx <- pop$gwcode==817 & pop$year <= 1975
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"


#' ### Yemen


joint_wide %>%
  filter(gwcode==678 & year > 1985 & year < 1995)

## use 1989 and before KSG for north Yemen
idx <- pop$gwcode==678 & pop$year <= 1989
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"


joint_wide %>%
  filter(gwcode==680 & year > 1985 & year < 1995)

## use KSG for south Yemen
idx <- pop$gwcode==680 & pop$year <= 1990
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"

#' ### Kosovo

joint_wide %>%
  filter(gwcode==347 & year > 2006 & year < 2015)

# Get Kosovo series and kalman smooth
idx <- pop$gwcode==347
kos <- pop$pop_wdi[idx]
kos <- imputeTS::na_kalman(kos, "auto.arima")
pop$pop[idx] <- kos
pop$source[idx] <- "wdi"

#' ### Tibet
#' 
#' Tibet is missing in UN data, so use KSG as with 1949 and before. 

joint_wide %>%
  filter(gwcode==711 & year > 1945 & year < 1955)

# Use KSG for 1950 as well
idx <- pop$gwcode==711 & pop$year == 1950
pop$pop[idx] <- pop$pop_ksg[idx]
pop$source[idx] <- "ksg"

#' ### Czechoslovakia
#' 
#' Missing first year (1918), backwards impute.

joint_wide %>%
  filter(gwcode==315 & year < 1921)

pop %>% 
  filter(gwcode==315 & year < 1921)


#' Plot the pop series:

idx <- pop$gwcode==315
csk <- pop$pop_ksg[idx]
plot(pop$year[idx], csk)


#' It is quite jumpy, so use only pre-1937

idx2 <- pop$gwcode==315 & pop$year <= 1937
csk  <- pop$pop_ksg[idx2]
csk  <- rev(imputeTS::na.kalman(rev(csk), "auto.arima"))
pop$pop[idx2]   <- csk
pop$source[idx2] <- "ksg"

# Verify via plot
idx <- pop$gwcode==315
csk <- pop$pop[idx]
plot(pop$year[idx], csk)


# Prepare to write out data -----------------------------------------------

#' ## Get ready to write final data

#' ### Check values for splitting/joining countries

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

#' ### Missingness

plot_missing(pop, x = "pop", ccode = "gwcode", time = "year", statelist = "GW")

#' The final data is complete for 1950 to 2019, except for Abkhazia, South Ossetia, and Zanzibar. 

pop %>%
  filter(is.na(pop)) %>%
  group_by(gwcode) %>%
  summarize(years = paste0(range(year), collapse = " - "), N = n()) %>%
  mutate(country_name = country_names(gwcode)) %>%
  knitr::kable()


pop %>%
  select(gwcode, year, pop) %>%
  write_csv(., path = "output/population.csv")


setwd(oldwd)
