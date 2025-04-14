#' Initialize the Library Context
#'
#' This function initializes the library context by creating a global `RequestInfo` object if it does not already exist.
#'
#' @examples
#' library(eikondata)
#' init()
#' @export
init = function() {
  if (!exists("requestInfo", envir = .GlobalEnv)) {
    requestInfo <<- new("RequestInfo")
  }
}

#' Set Application ID
#'
#' Sets the application ID, which must be configured before calling functions to retrieve data.
#'
#' @param appId A character string representing the application ID.
#'
#' @examples
#' library(eikondata)
#' set_app_id("YOUR_APP_ID")
#' @export
set_app_id = function(appId) {
  init()
  requestInfo@application_id <<- appId
}

#' Get Application ID
#'
#' Retrieves the application ID that was previously set using `set_app_id()`.
#'
#' @return A character string containing the application ID.
#'
#' @examples
#' library(eikondata)
#' my_app_id = get_app_id()
#' @export
get_app_id = function() {
  init()
  return(requestInfo@application_id)
}

#' Set Proxy Port
#'
#' Sets the proxy port. By default, the library connects to port 9000 unless overridden.
#'
#' @param port An integer specifying the proxy port.
#'
#' @examples
#' library(eikondata)
#' set_proxy_port(37009L)
#' @export
set_proxy_port = function(port) {
  init()
  requestInfo@proxy_port <<- as.integer(port)
}

#' Get Proxy Port
#'
#' Retrieves the currently configured proxy port.
#'
#' @return An integer representing the proxy port.
#'
#' @examples
#' library(eikondata)
#' prox_port = get_proxy_port()
#' @export
get_proxy_port = function() {
  init()
  return(requestInfo@proxy_port)
}

#' RequestInfo Class
#'
#' A class representing request configuration, including the application ID, proxy port, and service URL.
#'
#' @slot application_id A character string representing the application ID.
#' @slot proxy_port An integer specifying the proxy port (default: `9000L`).
#' @slot url A character string containing the service URL.
#'
#' @export
RequestInfo = setClass(
  "RequestInfo",
  slots = c(
    application_id = "character",
    proxy_port = "integer",
    url = "character"
  ),
  prototype = list(application_id = "", proxy_port = 9000L, url = ""),
  validity = function(object) {
    return(TRUE)
  }
)
