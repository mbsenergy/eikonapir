#' Retrieve Historical Time Series Data
#'
#' Fetches historical data for one or multiple RICs from the API.
#'
#' @param rics A character string or list of character strings specifying the RICs to retrieve data for.
#' @param fields A character string or list of strings specifying the fields to retrieve.
#'   Available fields: `c("TIMESTAMP", "VALUE", "VOLUME", "HIGH", "LOW", "OPEN", "CLOSE", "COUNT")`.
#'   Default: `"*"` (all fields).
#' @param start_date A character string specifying the start date in format `"%Y-%m-%dT%H:%M:%S"` (e.g., `"2016-01-20T15:04:05"`).
#'   Default: 100 days before the current date.
#' @param end_date A character string specifying the end date in format `"%Y-%m-%dT%H:%M:%S"`.
#'   Default: current date.
#' @param interval A character string specifying the data interval.
#'   Possible values: `"tick"`, `"minute"`, `"hour"`, `"daily"`, `"weekly"`, `"monthly"`, `"quarterly"`, `"yearly"`.
#'   Default: `"daily"`.
#' @param count An integer specifying the maximum number of data points to retrieve.
#' @param calendar A character string specifying the calendar type.
#'   Possible values: `"native"`, `"tradingdays"`, `"calendardays"`.
#' @param corax A character string specifying the correction method.
#'   Possible values: `"adjusted"`, `"unadjusted"`.
#' @param raw_output A logical value. If `TRUE`, returns data in raw JSON format.
#'   If `FALSE`, returns a data frame. Default: `FALSE`.
#'   This parameter is ignored if `raw_output = TRUE`. Default: `FALSE`.
#' @param debug A logical value. If `TRUE`, prints debug messages, including JSON request and response. Default: `FALSE`.
#' @param verbose A logical value. If `TRUE`, prints messages. Default: `FALSE`.
#'
#' @return A data frame with historical data or a JSON response if `raw_output = TRUE`.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom crayon red green bold
#' @importFrom emo ji
#' @export
get_timeseries = function(rics, fields = "*", start_date = NULL, end_date = NULL, interval = "daily",
                          raw_output = FALSE, calendar = NULL, count = NULL, corax = NULL,
                          debug = FALSE, verbose = FALSE) {

  TimeSeries_endpoint = "TimeSeries"

  # Validate RICs
  if (is.character(rics)) {
    rics = list(trimws(rics))
  }

  # Ensure fields include TIMESTAMP if not using "*"
  if (!("*" %in% fields)) {
    fields = unique(c(fields, "TIMESTAMP"))
  }

  # Validate interval
  if (!is.character(interval)) {
    message(red("get_timeseries error: The interval parameter should be a character string"))
    return(NULL)
  }

  # Set default start and end dates if NULL
  if (is.null(start_date)) {
    start_date = format(Sys.Date() - 100, "%Y-%m-%dT%H:%M:%S")
  }
  if (is.null(end_date)) {
    end_date = format(Sys.Date(), "%Y-%m-%dT%H:%M:%S")
  }

  # Build request payload
  payload = list(
    "rics" = rics,
    "fields" = fields,
    "interval" = interval,
    "startdate" = start_date,
    "enddate" = end_date
  )

  # Add optional parameters
  if (!set_integer_property(payload, "count", count)) {
    message(red("get_timeseries error: count must be an integer"))
    return(NULL)
  }
  if (!set_string_property(payload, "calendar", calendar)) {
    message(red("get_timeseries error: calendar must be a string"))
    return(NULL)
  }
  if (!set_string_property(payload, "corax", corax)) {
    message(red("get_timeseries error: corax must be a string"))
    return(NULL)
  }

  # Send request
  json_data = send_json_request(TimeSeries_endpoint, payload, debug = debug)

  # Return raw JSON if requested
  if (raw_output) {

    if(verbose) {message(crayon::blue$bold(paste(emo::ji("package"), "RAW JSON retrieved")))}
    return(json_data)

  } else {

    # Parse response JSON
    data = fromJSON(json_data)

    data = get_formatted_data_frame(data)
    if(verbose) {
    message(crayon::green$bold(paste(emo::ji("chart_with_upwards_trend"),
                                     "Data retrieved correctly for:",
                                     paste(rics, collapse = ", "),
                                     "\n  Start:", start_date,
                                     "\n  End:  ", end_date)))
    }

    return(data)

  }

}
