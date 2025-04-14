#' Spot Prices - PWR
#'
#' A small dataset for demonstration purposes.
#'
#' @format A data.table with 5 rows and 2 columns:
#' \describe{
#'   \item{id}{Integer ID}
#'   \item{value}{Numeric values}
#' }
#' @source Generated manually
"dt_spot_pwr"

#' Spot Prices - GAS
#'
#' A small dataset for demonstration purposes.
#'
#' @format A data.table with 5 rows and 2 columns:
#' \describe{
#'   \item{id}{Integer ID}
#'   \item{value}{Numeric values}
#' }
#' @source Generated manually
"dt_spot_gas"


#' FWD Prices - PWR
#'
#' A small dataset for demonstration purposes.
#'
#' @format A data.table with 5 rows and 2 columns:
#' \describe{
#'   \item{id}{Integer ID}
#'   \item{value}{Numeric values}
#' }
#' @source Generated manually
"dt_fwds_pwr"

#' FWD Prices - GAS
#'
#' A small dataset for demonstration purposes.
#'
#' @format A data.table with 5 rows and 2 columns:
#' \describe{
#'   \item{id}{Integer ID}
#'   \item{value}{Numeric values}
#' }
#' @source Generated manually
"dt_fwds_gas"



#' FWD Prices or DAM Prices- PWR
#'
#' A small dataset for demonstration purposes.
#'
#' @format A data.table with 5 rows and 2 columns:
#' \describe{
#'   \item{id}{Integer ID}
#'   \item{value}{Numeric values}
#' }
#' @source Generated manually
"dt_fwds_pwr_fwddam"


#' Calendar of Public Holidays
#'
#' `calendar_holidays` with countries and date range.
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{date}{Date}
#'   \item{holiday_GR}{Greece}
#'   \item{holiday_HU}{Hungary}
#'   ...
#' }
#' @usage data(new_calendar_holidays)
"calendar_holidays"

#' New Calendar of Public Holidays
#'
#' Updated version of `calendar_holidays` with additional countries or extended date range.
#'
#' @format A data.table with columns:
#' \describe{
#'   \item{date}{Date}
#'   \item{holiday_GR}{Greece}
#'   \item{holiday_HU}{Hungary}
#'   ...
#' }
#' @usage data(new_calendar_holidays)
"new_calendar_holidays"

#' Power Market Mapping Table
#'
#' A reference table mapping countries to availability in Eikon data.
#'
#' @format A `data.table` with 11 rows and 2 columns:
#' \describe{
#'   \item{countries}{Country name (character)}
#'   \item{eikon}{Eikon data availability flag ("YES"/"NO")}
#' }
#' @usage data(pwr_mapped_codes)
#' @keywords datasets
"pwr_mapped_codes"

#' Gas Market Mapping Table
#'
#' A reference table mapping gas products to availability in Eikon data.
#'
#' @format A `data.table` with 5 rows and 2 columns:
#' \describe{
#'   \item{product}{Country name (character)}
#'   \item{eikon}{Eikon data availability flag ("YES"/"NO")}
#' }
#' @usage data(gas_mapped_codes)
#' @keywords datasets
"gas_mapped_codes"


#' Power Market Product Codes Mapping
#'
#' Reference table mapping countries to their power product codes used in internal models and spot market data sources.
#'
#' @format A `data.table` with 11 rows and 4 columns:
#' \describe{
#'   \item{countries_2d}{Two-letter country code (character)}
#'   \item{countries}{Full country name (character)}
#'   \item{products_PWR_code}{Internal product code used in power models (character)}
#'   \item{spot_PWR_code}{Spot market code used for Eikon or other sources (character)}
#' }
#' @usage data(pwr_products_full)
#' @keywords datasets
"pwr_products_full"


#' Gas Market Product Codes Mapping
#'
#' Reference table mapping gas market hubs to internal and external product codes.
#'
#' @format A `data.table` with 5 rows and 3 columns:
#' \describe{
#'   \item{products_GAS}{Gas market hub name (character)}
#'   \item{products_GAS_code}{Internal product code used in models or systems (character)}
#'   \item{spot_GAS_code}{Spot market code used for Eikon or other external data sources (character)}
#' }
#' @usage data(gas_products_full)
#' @keywords datasets
"gas_products_full"


#' Reuters Month Codes
#'
#' Mapping between calendar months and their corresponding Reuters futures codes.
#'
#' @format A `data.table` with 12 rows and 2 columns:
#' \describe{
#'   \item{month}{Numeric month (1 to 12) as character}
#'   \item{code}{Single-letter Reuters futures code (character)}
#' }
#' @usage data(reuters_months)
#' @keywords datasets
"reuters_months"

#' Reuters Quarter Codes for Gas Markets
#'
#' Mapping between calendar quarters and Reuters futures codes used for gas market instruments.
#'
#' @format A `data.table` with 4 rows and 2 columns:
#' \describe{
#'   \item{quarter}{Numeric quarter (1 to 4) as character}
#'   \item{code}{Single-letter Reuters futures code (character)}
#' }
#' @usage data(reuters_quarters_gas)
#' @keywords datasets
"reuters_quarters_gas"


#' Reuters Quarter Codes for Power Markets
#'
#' Mapping between calendar quarters and Reuters futures codes used for power market instruments.
#'
#' @format A `data.table` with 4 rows and 2 columns:
#' \describe{
#'   \item{quarter}{Numeric quarter (1 to 4) as character}
#'   \item{code}{Single-letter Reuters futures code for power contracts (character)}
#' }
#' @usage data(reuters_quarters_pwr)
#' @keywords datasets
"reuters_quarters_pwr"
