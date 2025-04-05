#' Check if a Value is a String
#'
#' This function checks whether a given value is a character string of length 1.
#'
#' @param value The value to check.
#' @return `TRUE` if `value` is a character string of length 1, otherwise `FALSE`.
is_string = function(value) {
  return(is.character(value) && length(value) == 1)
}

#' Check if a Value is a Named List
#'
#' This function checks whether a given value is a list with named elements.
#'
#' @param value The value to check.
#' @return `TRUE` if `value` is a named list, otherwise `FALSE`.
is_named_list = function(value) {
  return(is.list(value) && !is.null(names(value)))
}

#' Check if a Value is a Non-Named List
#'
#' This function checks whether a given value is a list without names.
#'
#' @param value The value to check.
#' @return `TRUE` if `value` is a list without names, otherwise `FALSE`.
is_non_named_list = function(value) {
  return(is.list(value) && is.null(names(value)))
}





#' Set an Integer Property in a List Object
#'
#' This function sets an integer property in a list object if the provided value is non-null and of type integer.
#'
#' @param list_object A list where the property should be set.
#' @param property A character string representing the property name.
#' @param value An integer value to set.
#'
#' @return A logical value: `TRUE` if the operation was successful, `FALSE` if the value is not an integer.
set_integer_property = function(list_object, property, value) {
  # Validate value
  if (!is.null(value)) {
    if (!is.integer(value)) {
      warning("set_integer_property error: value must be an integer")
      return(FALSE)
    }

    # Set the property in the list
    list_object[[property]] = value
  }

  return(TRUE)
}



#' Set a String Property in a List Object
#'
#' This function sets a string property in a list object if the provided value is non-null and of type character.
#'
#' @param list_object A list where the property should be set.
#' @param property A character string representing the property name.
#' @param value A character string representing the value to set.
#'
#' @return A logical value: `TRUE` if the operation was successful, `FALSE` if the value is not a character.
set_string_property = function(list_object, property, value) {
  if (!is.null(value)) {
    if (is.character(value)) {
      list_object[[property]] = value
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  return(TRUE)
}

#' Convert Data to Formatted Data Frame
#'
#' This function takes raw time series data and returns it in a structured wide-format data frame.
#'
#' @param data A list containing a `timeseriesData` element with structured time series data.
#' @return A data frame where each row represents a time series entry, with an added `Security` column.
#' importFrom data.table setDT
#' @export
get_formatted_data_frame = function(data) {
  input_data_frame = data$timeseriesData
  data_frames = vector("list", nrow(input_data_frame))  # Preallocate list

  for (i in seq_len(nrow(input_data_frame))) {
    current_row = input_data_frame[i, ]
    current_fields = current_row$fields[[1]]$name
    ric_column = rep(current_row$ric, nrow(input_data_frame[i, ]))

    data_frame = as.data.frame(current_row$dataPoints[[1]], stringsAsFactors = FALSE)
    data_frame = cbind(data_frame, Security = ric_column)
    names(data_frame) = current_fields

    data_frames[[i]] = data_frame
  }

  DATAS = do.call(rbind, data_frames)
  setDT(DATAS)
  return(DATAS)
}


#' Print Retrieval Message with Colors
#'
#' @description This function prints a retrieval message with colored output using the `crayon` and `glue` packages. It highlights key elements such as the RIC, start date, and end date.
#'
#' @param rics A character string representing the RIC (Reuters Instrument Code) or a vector of RICs.
#' @param from_date A character string or Date object representing the start date.
#' @param to_date A character string or Date object representing the end date.
#'
#' @return Prints a formatted message to the console but does not return a value.
#'
#' @examples
#' print_retrieval_message("AAPL.O", "2020-01-01", "2023-01-01")
#'
#' @import crayon
#' @importFrom glue glue
#' @export
print_retrieval_message = function(rics, from_date, to_date, nrows = NULL) {
  message = glue::glue("- {crayon::blue('Retrieving:')} {crayon::green(rics)}, {crayon::blue('from')} {crayon::yellow(from_date)}, {crayon::blue('to')} {crayon::yellow(to_date)}, {crayon::blue('with nrows>')} {crayon::yellow(nrows)}")
  cat(message, "\n")
}
