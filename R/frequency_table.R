#' Generate frequency of each entry in each column of dataframe
#'
#' Real-life data is seldom perfect and fields in a data.frame contains entries not anticipated by the data scientist.
#' This function helps to know your data entries before performing any manipulations on it.
#' This function generates frequency table excel, each column of input dataframe in a separate sheet in output excel file.
#' Warning: An excel sheet can support 2^20 rows of data only (approx. 1 million). If the number of unique entries in a column exceeds that, excel will drop the low frequency entries.
#'
#' @param dataset A data.frame
#' @param output_filename Name of the output text file (should end in ".xlsx")
#' Strongly advised to pass this parameter, else the function's default is "frequency_table_<system_time>.xlsx"
#' @param maximum_entries Maximum unique entries in output.
#' For e.g. setting this parameter to 10000 will return only top 10000 occurring entries in each column
#' @param format_width Boolean input indicating if output excel cells' column width need to be formatted to "auto"
#' @param sl_no_required Boolean input indicating if Sl_No column needs to be present in output excel
#' @param frequency_required Boolean input indicating if Frequency column needs to be present in output excel
#' @param percentage_required Boolean input indicating if Percentage column needs to be present in output excel
#' @param cumulative_percentage_required Boolean input indicating if Cumulative_Percentage column needs to be present in output excel
#' @param string_length_required Boolean input indicating if String_Length column needs to be present in output excel
#' @return Does not return to calling function, writes to file system rather
#' @export
#' @importFrom magrittr %>%
#' @importFrom data.table .N
#' @examples
#' \dontrun{
#' frequency_table(dataset = iris, output_filename = "frequency_table_iris.xlsx")
#' frequency_table(dataset = mtcars, output_filename = "C/Users/Desktop/frequency_table_mtcars.xlsx")
#' }

frequency_table <- function(dataset, output_filename = "", maximum_entries = 2^20, format_width = TRUE,
                            sl_no_required = TRUE,
                            frequency_required = TRUE,
                            percentage_required = TRUE,
                            cumulative_percentage_required = FALSE,
                            string_length_required = TRUE) {

  .data <- frequency <- cumulative_percentage <- percentage <- sl_no <- string_length <- NULL

  if (output_filename == "") {
    output_filename <- gsub(x = paste0("frequency_table_", Sys.time(), ".xlsx"), pattern = " |:|-", replacement = "_")
  }
  if (substr(output_filename, nchar(output_filename) - 4, nchar(output_filename)) != ".xlsx") {
    output_filename <- paste0(output_filename, ".xlsx")
  }
  message("Writing frequency table of dataset to ", output_filename)
  cat("\n")

  message(stringr::str_interp("Understanding the output (${output_filename}):"))

  message("Each worksheet in the output pertains to a column in input dataset.")
  message("sl_no: Serial number, just a unique identifier assigned to each unique entry sequentially.")
  message("<field-name>: Distinct element present in the column of input dataset.")
  message("frequency: Count of occurrence of each distinct element in the column of input dataset.")
  message("percentage: frequency column converted to percentage.")
  message("string_length: Number of characters in string equivalent of an entry. Can be useful while disambiguating missing value (NA) with blank spaces.")
  cat("\n")

  dataset <- dataset %>% dplyr::mutate_if(is.factor, as.character) # Change factor columns to character
  data.table::setDT(dataset)                                      # Changing class of dataset to data.table
  workbook <- openxlsx::createWorkbook(output_filename)            # Creating and mounting an empty excel file

  for (i in seq_len(ncol(dataset))) {                                      # Loop over each column
    sheet_name <- names(dataset)[i]                                # Each column will be stored in a sheet of excel

    # Excel sheet name can have a maximum of 31 characters. If a column-name has more than 31 characters, below line will pick first 15 and last 16 characters.
    sheet_name <- ifelse(nchar(sheet_name) > 31, paste0(substr(sheet_name, 1, 15), substr(sheet_name, nchar(sheet_name) - 15, nchar(sheet_name))), sheet_name)

    openxlsx::addWorksheet(workbook, sheet_name)                  # Add a new empty sheet for this particular column

    frequency_table <- dataset[, i, with = FALSE]                  # Retain current column
    frequency_table <- as.data.frame(frequency_table[, list(frequency = .N), by = c(names(dataset)[i])]) # Aggregate to get frequency

    frequency_table <- frequency_table %>%
      dplyr::arrange(dplyr::desc(frequency)) %>%                              # Sort by decreasing order of frequency
      dplyr::mutate(percentage = 100 * frequency / sum(frequency)) %>%     # Create new column, frequency to percentage
      dplyr::mutate(cumulative_percentage = cumsum(percentage)) %>% # Create new column, Cumulative Percentage
      dplyr::mutate(sl_no = seq_len(length(.data$frequency)))  %>% # Create new column, serial number
      dplyr::mutate(string_length = nchar(.data[[names(dataset)[i]]])) %>% # Create new column, string length = number of characters in the value
      dplyr::select(sl_no, dplyr::everything(), string_length)                  # Reorder columns

    # Excel supports a maximum of 2^20 rows (1048576). Therefore, Any entries below that in frequency table will not be written
    frequency_table <- frequency_table[1:min(nrow(frequency_table), maximum_entries), ]

    if (format_width == TRUE) {
      openxlsx::setColWidths(workbook, sheet = i, cols = 1:5, widths = "auto")
    }
    if (sl_no_required == FALSE) {
      frequency_table <- frequency_table %>% dplyr::select(-sl_no)
    }
    if (frequency_required == FALSE) {
      frequency_table <- frequency_table %>% dplyr::select(-frequency)
    }
    if (percentage_required == FALSE) {
      frequency_table <- frequency_table %>% dplyr::select(-percentage)
    }
    if (cumulative_percentage_required == FALSE) {
      frequency_table <- frequency_table %>% dplyr::select(-cumulative_percentage)
    }
    if (string_length_required == FALSE) {
      frequency_table <- frequency_table %>% dplyr::select(-string_length)
    }

    openxlsx::writeData(workbook, i, frequency_table)                  # Write frequency table to workbook at ith position

    message(paste0("Generated frequency table for column " , i , " - ", names(dataset)[i])) # For status printing
  }

  openxlsx::saveWorkbook(workbook, file = output_filename)             # Write final excel and unmount it from R
  invisible()                                                          # To return nothing
}
