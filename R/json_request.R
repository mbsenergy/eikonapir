#' Send JSON Request
#'
#' Sends a JSON request to the API using the configured proxy port and application ID.
#'
#' @param entity A character string specifying the entity to query.
#' @param request_data A JSON string or a named list containing the request payload.
#' @param debug A logical value. If `TRUE`, prints the request and response details for debugging.
#'
#' @return A character string containing the response data if the request is successful, otherwise `NULL`.
#'
#' @importFrom httr POST add_headers content
#' @importFrom jsonlite fromJSON toJSON
#' @importFrom crayon blue red green bold
#' @export
send_json_request = function(entity, request_data, debug=FALSE) {

  # Convert JSON string to list if necessary
  if (is.character(request_data)) {
    payload = fromJSON(request_data)
  } else {
    payload = request_data
  }

  # Construct API URL
  url = paste0("http://localhost:", get_proxy_port(), "/api/v1/data")

  # Prepare request payload
  request = list('Entity' = list('E' = entity, 'W' = payload))

  # Send HTTP POST request
  response = POST(
    url,
    add_headers(
      'Content-Type' = 'application/json',
      'x-tr-applicationid' = get_app_id()
    ),
    body = request,
    encode = "json"
  )

  # Extract response content
  response_data = content(response, "text")
  response_status = response$status_code

  # Debugging information
  if (debug) {
    message(bold(blue("Request *************************************")))
    message(blue(toJSON(request, pretty = TRUE)))
    message(bold(green("Response *************************************")))
    message(green(response_data))
    message(bold("Response status *************************************"))
    message(response_status)
  }

  # Handle response
  if (response_status == 200) {
    return(response_data)
  } else {
    message(bold(red("HTTP Error, code =")), red(response_status))
    return(NULL)
  }
}
