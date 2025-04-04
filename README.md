# eikonapir  
**R wrapper for Refinitiv Eikon API**

---

## Overview  
`eikonapir` provides a streamlined interface for retrieving financial data from the Refinitiv Eikon platform directly within `R`. The package wraps around the Eikon Scripting API, allowing users to access historical time series and forward market data efficiently in `data.table` format.

---

## Prerequisites  

To use this R package, you must have:

- ✅ **A valid Refinitiv Eikon license**  
  Get one at [Refinitiv Eikon](https://customers.thomsonreuters.com/eikon/)
  
- ✅ **Refinitiv Eikon 4 Desktop** _or_ the **Refinitiv Eikon API Proxy**  
  For more information: [Eikon Web and Scripting APIs](https://developers.thomsonreuters.com/eikon-apis/eikon-web-and-scripting-apis-limited-access)

---

## Installation

Install the package using `remotes`:

````r
install.packages("remotes")
remotes::install_github("mbsenergy/eikonapir")
````


## Getting Started
### 1. Set up environment
Start the Eikon Desktop or API Proxy and configure the API access in `R`:


````r
library(eikonapir)
set_proxy_port(9000L)  # If using the proxy
set_app_id(Sys.getenv("REUTERS_KEY"))
````
### 2. Retrieve Market Data

````r
get_timeseries(
  rics = "TTFDA",
  fields = c("TIMESTAMP", "CLOSE"),
  start_date = "2024-01-01T00:00:00",
  end_date = "2024-01-10T00:00:00"
)

get_rics_f("FDBMJ5", from_date = "2023-01-01", to_date = "2023-12-31")

````

## Documentation
Full documentation is generated with `roxygen2` and available in the `man/` directory after building the package. Run:

````r
?get_rics_d
?get_rics_h
?get_rics_f
````
