
#获得电脑nodename
get_computer_nodename <- function() {
  return(Sys.info()["nodename"])
}

#' Get the next ID from a primary ID
#'
#' This function extracts the current ID and returns the next ID.
#'
#' @param my_primary A data frame containing primary IDs.
#' @return A list containing the next numeric part of the ID and the prefix part of the ID.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' my_primary <- data.frame(ID = c("g24040412094300123", "g24040412094300124"))
#' result <- get_next_id(my_primary)
#' }
#'
#' @export
get_next_id <- function(my_primary) {
  # Extract the last ID from the data frame
  mystr <- my_primary[nrow(my_primary), "ID"]

  # Extract the prefix and numeric parts of the ID
  id_p <- substr(mystr, 1, 13)
  id_n <- as.numeric(substr(mystr, 14, 18)) + 1

  # Return the next numeric part and prefix part of the ID as a list
  return(list(id_n = id_n, id_p = id_p))
}



#' Generate IDs with specified parameters
#'
#' This function generates a series of IDs based on the specified parameters.
#'
#' @param start_num An integer indicating the starting number of the ID sequence.
#' @param end_num An integer indicating the ending number of the ID sequence.
#' @param char A character placed at the beginning of each ID.
#' @param digit_length An integer specifying the length of the numeric part, controlling the padding.
#' @param include_datetime A logical value indicating whether to include the current year, month, day, hour, minute, and second in the ID.
#'
#' @return A vector containing all the generated IDs between start_num and end_num.
#'
#' @examples
#' \dontrun{
#' start <- 123
#' end <- 135
#' character <- "A"
#' digit_length <- 5
#' result_ids <- generate_id(start, end, character, digit_length, include_datetime = TRUE)
#' for (result_id in result_ids) {
#'   print(paste("Resulting ID:", result_id))
#' }
#' }
#'
#' @export
generate_id <- function(start_num=1, end_num, char="g", digit_length=5, include_datetime = TRUE) {
  # Generate a series of IDs, each consisting of specified parameters

  ids <- c()  # Store the generated IDs

  if (include_datetime) {
    current_time <- format(Sys.time(), "%y%m%d%H%M%S")
  } else {
    current_time <- ""
  }

  # Iterate through the range of numbers to generate IDs
  for (num in start_num:end_num) {
    # Convert the number to a string with the specified number of digits
    num_str <- sprintf(paste0("%0", digit_length, "d"), num)
    # Construct the ID and add it to the vector
    ids <- c(ids, paste0(char, current_time, num_str))
  }

  return(ids)
}

# start <- 123
# end <- 135
# character <- "A"
# digit_length <- 5
# result_ids <- generate_id(start, end, character, digit_length, include_datetime = FALSE)
# for (result_id in result_ids) {
#   print(paste("Resulting ID:", result_id))
# }
#
#

generate_stageid <- function(start_num=1, end_num, char="24GC", digit_length=4) {
  # Generate a series of IDs, each consisting of specified parameters
  ids <- c()  # Store the generated IDs
  # Iterate through the range of numbers to generate IDs
  for (num in start_num:end_num) {
    # Convert the number to a string with the specified number of digits
    num_str <- sprintf(paste0("%0", digit_length, "d"), num)
    # Construct the ID and add it to the vector
    ids <- c(ids, paste0(char, num_str))
  }

  return(ids)
}



