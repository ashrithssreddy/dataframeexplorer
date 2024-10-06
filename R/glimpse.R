#' Generate glimpse of dataset
#'
#' Understanding the dataset through a glimpse of it will come handy while data manipulation coding.
#' This function generates the glimpse of data.frame (similar to str()) using tibble::glimpse and write to a text file.
#' Using same file name for different datasets will append the outputs to a same file.
#'
#' @param dataset A data.frame object
#' @param output_filename Name of the output text file (prefer to end in ".txt", although the backend will append if not)
#' Function's default is "glimpse_<system_time>.txt"
#' @return Does not return any value, writes to disk rather
#' @export
#' @examples
#' \dontrun{
#' glimpse_to_file(dataset = mtcars, output_filename = "glimpse_mtcars.txt")
#' glimpse_to_file(dataset = iris, output_filename = "C/Users/Desktop/glimpse_iris.txt")
#' }

glimpse_to_file <- function(dataset, output_filename = "") {

  # If the user did not pass output_filename parameter, function will generate and use "glimpse_<system_time>.txt"
  if (output_filename == "") {
    output_filename <- gsub(x = paste0("glimpse_", Sys.time(), ".txt"),
                            pattern = " |:|-", replacement = "_")
  }

  # If the output_filename does not end with ".txt", code to append ".txt"
  if (substr(output_filename, nchar(output_filename) - 3, nchar(output_filename)) != ".txt") {
    output_filename <- paste0(output_filename, ".txt")
  }

  # Printing the below message to console, especially handy when user does not pass output_filename parameter
  message("Writing glimpse of the dataset to ", output_filename)
  message("Run closeAllConnections() if outputs are not being printed to console anymore!")

  # Ensures the glimpse output is written to output_filename text file and not to console
  sink(output_filename, type = c("output"), append = TRUE)

  # Generates and print class of the object, although it is expected to be a data.frame
  cat("Class:" , paste0(class(dataset), collapse = ", "), "\n")

  # Generates and writes the output of tibble::glimpse() to output_filename text file
  tibble::glimpse(dataset)

  # Blank line at the end of output
  cat("\n")

  sink() # Unmounts output_filename text file

  invisible() # To return nothing
}
