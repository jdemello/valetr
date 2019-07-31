
# valetr

The `valetr` package provides an R-native interface to [Bank of Canada’s
Valet API](https://www.bankofcanada.ca/valet/docs) —read the API's [terms and
condtions](https://www.bankofcanada.ca/terms/). Some of the data
available include key monetary policy variables —i.e. Total CPI and M
aggregates.

Valet also provides a route to Foreign Exchange Rates in RSS. This route
returns the **most recent** foreign exchange rate —one observation only.
Therefore, it has little application for this package.

## Installation

`valetr` has yet to be submitted to CRAN.

To install the development version:

``` r
devtools::install_github("jdemello/valetr")
```
