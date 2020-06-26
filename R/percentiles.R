#' Generate percentiles for entire dataframe
#'
#' This function generates percentiles for all numeric columns in the dataframe.
#' This will come handy while understanding the distribution of data and in outlier treatment.
#'
#' @param dataset A data.frame
#' @param output_filename Name of the output excel file (should end in ".xlsx")
#' Strongly advised to pass this parameter, else the function's default is "percentiles_table_<system_time>.xlsx"
#' @param percentiles numeric vector of probabilities with values in [0,100]
#' @param format_width Boolean input indicating if output excel cells' column width need to be formatted to "auto"
#' @param sd_required Boolean input indicating if standard deviation column needs to be present in output excel
#' @param min_required Boolean input indicating if minimum column needs to be present in output excel
#' @param max_required Boolean input indicating if maximum column needs to be present in output excel
#' @param mean_required Boolean input indicating if mean column needs to be present in output excel
#' @param missing_percentage_required Boolean input indicating if missing percentage column needs to be present in output excel
#' @param class_required Boolean input indicating if datatype column should be the last column in output excel
#' @return Does not return to calling function, writes to file system rather
#' @importFrom stats quantile sd setNames
#' @export
#' @examples
#' \dontrun{
#' percentiles_table(mtcars, output_filename = "percentiles_table_mtcars.xlsx")
#' percentiles_table(iris, output_filename = "C/Users/Desktop/percentiles_table_iris.xlsx")
#' }

percentiles_table <- function(dataset, output_filename = "",
                             percentiles = c(0:10, seq(10, 90, 10), seq(25, 75, 25), 91:100),
                             format_width = TRUE,
                             sd_required = TRUE,
                             min_required = TRUE,
                             max_required = TRUE,
                             mean_required = TRUE,
                             missing_percentage_required = TRUE, class_required = TRUE) {

  # Features to add
  # Warning: Non-numeric fields found, ignoring them
  column <- missing_percentage <- NULL

  if (output_filename == "") {
    output_filename <- gsub(x = paste0("percentile_table_", Sys.time(), ".xlsx"), pattern = " |:|-", replacement = "_")
  }
  if (substr(output_filename, nchar(output_filename) - 4, nchar(output_filename)) != ".xlsx") {
    output_filename <- paste0(output_filename, ".xlsx")
  }
  message("Writing percentile table of dataset to ", output_filename)

  dataset <- dataset %>% dplyr::select_if(is.numeric) # Select only numeric columns
  invisible(gc()) # To recover memory be clearing garbage

  percentiles <- percentiles %>% unique %>% sort  # Remove duplicates, if any

  # Code to generate percentiles
  final_output <- dataset %>% lapply(FUN = function(x) {
    setNames(as.data.frame(t(unname(quantile(x, percentiles / 100, na.rm = TRUE)))), paste0("percentile_", percentiles))}) %>%
    plyr::rbind.fill()

  # Additional column summary metrics for reference
  final_output$sd <- dataset %>% sapply(sd, na.rm = TRUE) %>% unname()
  final_output$min <- dataset %>% sapply(min, na.rm = TRUE) %>% unname()
  final_output$max <- dataset %>% sapply(max, na.rm = TRUE) %>% unname()
  final_output$mean <- dataset %>% sapply(mean, na.rm = TRUE) %>% unname()
  final_output$missing_percentage <- dataset %>% sapply(function(x)  100 * sum(is.na(x)) / length(x)) %>% unname()
  final_output$class <- dataset %>% sapply(class) %>% unname()

  # Re-arranging columns
  final_output <- final_output %>%
    dplyr::mutate(column = colnames(dataset)) %>%
    dplyr::select(column, min, dplyr::everything())

  # Dropping columns based on user's inputs (function arguments)
  if (sd_required == FALSE) {
    final_output <- final_output %>% dplyr::select(-sd)
  }
  if (min_required == FALSE) {
    final_output <- final_output %>% dplyr::select(-min)
  }
  if (max_required == FALSE) {
    final_output <- final_output %>% dplyr::select(-max)
  }
  if (mean_required == FALSE) {
    final_output <- final_output %>% dplyr::select(-mean)
  }
  if (missing_percentage_required == FALSE) {
    final_output <- final_output %>% dplyr::select(-missing_percentage)
  }
  if (class_required == FALSE) {
    final_output <- final_output %>% dplyr::select(-class)
  }

  # Writing formatted output to working directory
  if (format_width) {
    final_output %>% openxlsx::write.xlsx(output_filename, row.names = FALSE, colWidths = "auto")
  }else{
    final_output %>% openxlsx::write.xlsx(output_filename, row.names = FALSE)
  }

  invisible() # To return nothing
}
