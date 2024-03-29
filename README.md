
# valetr

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/valetr)](https://CRAN.R-project.org/package=valetr)
[![CRAC\_Downloads](https://cranlogs.r-pkg.org/badges/grand-total/valetr)](https://CRAN.R-project.org/package=valetr)

`valetr` provides an interface to the [Bank of Canada’s ‘Valet’
API](https://www.bankofcanada.ca/valet/docs) —read the API’s [terms and
condtions](https://www.bankofcanada.ca/terms/). Some of the data
available include key monetary policy variables —i.e. Total CPI and M
aggregates.

‘Valet’ also provides a route to Foreign Exchange Rates in RSS. This
route returns the **most recent** foreign exchange rate —one observation
only. This functionality is not included in `valetr`.

## Retrieve key monetary policy variables

The ‘Valet’ API houses key monetary policy variables series. For
instance, it is possible to extract the series for some measures of core
inflation:

``` r
# getSeriesInfo will return a data.frame with the series info (including link and name)
cpiInfo <- getSeriesInfo(patternGroupLabel = "consumer price index$",
              patternSeriesLabel = "trim|median|common|total cpi$",  
                ignore.case=TRUE)

# now use the series'  link or name to feed getSerieData
# note that getSeriesData accepts multilpe links or names
cpis <- getSeriesData(series=cpiInfo[["series_link"]]) # can also use cpiInfo[["series_name"]]
```

Note that `getSeriesData()` allows for the API’s query
functionality:

``` r
cpis <- getSeriesData(series=cpiInfo[["series_link"]], start_date="2008-07-01", end_date="2012-07-01")
# cpis <- getSeriesData(series=cpiInfo[["series_link"]], recent_years=12L) # last 12 yrs
# cpis <- getSeriesData(series=cpiInfo[["series_link"]], recent_months=12L) # last 12 months
# cpis <- getSeriesData(series=cpiInfo[["series_link"]], recent_weeks=12L) # last 12 weeks
# cpis <- getSeriesData(series=cpiInfo[["series_link"]], recent=12L) # last 12 obs
```

Note that only `start_date` and `end_date` can be used together as query
parameters.

## Obtain exchange rate series

You can also retrieve foreign exchange rate series:

``` r
fxInfo <- getSeriesInfo(patternGroupLabel = "monthly exchange",
              patternSeriesLabel = "(EUR|GBP|USD)\\/CAD",  
                ignore.case=TRUE)

# now use the series'  link or name to feed getSerieData
# note that getSeriesData accepts multilpe links or names
fx <- getSeriesData(series=fxInfo[["series_link"]]) # can also use fx[["series_name"]]
```

## Installation

`valetr` version 0.1.0 is now on CRAN:

``` r
install.packages("valetr")
```

You can also install the development
    version:

``` r
devtools::install_github("jdemello/valetr")
```

## Functions

    regexSeriesLabel  Matches a `pattern` to the label of one or more series.
                      Returns a `data.frame` with label, link and name
    
    getSeriesInfo     Allows to pattern-match both group and series labels. 
                      This function returns a `data.frame` with a more 
                      complete description of a series. It includes their 
                      group information (name, label, desc.) as well as 
                      the series descriptive components
    
    getSeriesData     Extracts one or more series by specifying their name 
                      or link --vector can contain a mix of both. It also 
                      allows the insertion of the API's query parameters.

Read more about the API
[here](https://www.bankofcanada.ca/valet/docs#valet_api).
