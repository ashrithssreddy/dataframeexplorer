#' Determine the level / primary key of dataset
#'
#' Knowing the level of dataset is paramount to effectively and efficiently manipulate data, and the level of dataset is unknown oftentimes.
#' This function checks for count of unique records in all possible column combinations to
#' determine the level of dataset.
#' Check for text file generated for column combinations with unique records.
#'
#' @param dataset A data.frame
#' @param output_filename Name of the output text file (should end in ".txt", although the backend will append if not)
#' Function's default is "level_of_dataset_<system_time>.txt"
#' @param verbose Pass TRUE for detailed output
#' @return Does not return to calling function, writes to file system rather
#' @export
#' @examples
#' \dontrun{
#' level_of_data(dataset = iris[,c("mpg", "cyl", "disp", "hp")], output_filename = "level_mtcars.txt")
#' }

level_of_data <- function(dataset, output_filename = "", verbose = TRUE) {

  if (output_filename == "") {
    output_filename <- gsub(x = paste0("level_of_data_", Sys.time(), ".csv"), pattern = " |:|-", replacement = "_")
  }
  if (substr(output_filename, nchar(output_filename) - 3, nchar(output_filename)) != ".csv") {
    output_filename <- paste0(output_filename, ".csv")
  }
  cat("Writing level of data results to ", output_filename)
  data.frame(count_cols = integer(0), column_combination = character(0), duplicates = integer(0), is_level = logical(0)) %>%
    data.table::fwrite(output_filename)

  concatenated <- NULL

  generate_column_combinations <- function(dataset, n) {

    result <- utils::combn(names(dataset), n) %>%
      t %>%
      data.frame()

    result <- result %>%
      stats::setNames(paste0("V", seq_len(length(result)))) %>%
      as.matrix()
  }

  check_for_level <- function(dataset, column_combinations) {

    for (j in seq_len(nrow(column_combinations))) {
      concatenated_combination <- dataset %>%
        dplyr::select(column_combinations[j, ]) %>%
        tidyr::unite(concatenated, sep = ";;;") %>%
        dplyr::pull(concatenated)

      duplicates <- length(concatenated_combination) -  length(unique(concatenated_combination))

      data.frame(count_cols = ncol(column_combinations),
                 column_combination = paste(column_combinations[j, ], collapse = " x "),
                 duplicates = duplicates,
                 is_level = ifelse(duplicates == 0, TRUE, FALSE)) %>%
        data.table::fwrite(output_filename, append = TRUE)

      if (duplicates == 0) {
        message(paste(paste(column_combinations[j, ], collapse = " x "), "is found to be a level")) # , "\n"
      }else{
        if(verbose == TRUE){
          cat(paste(paste(column_combinations[j, ], collapse = " x "), "is not found to be a level", "\n"))
        }
      }
      rm(concatenated_combination, duplicates); invisible(gc());
    }
  }

  data.table::setDF(dataset)
  cat("\n", paste0(ncol(dataset), " columns present in dataframe passed to function: "))
  dataset %>% names %>% paste(collapse = ", ") %>% paste0("\n") %>% cat

  if(verbose == TRUE){
    cat("\n")
    message(stringr::str_interp("Understanding the output (${output_filename}):"))
    message("count_cols: Count of columns in the column-combination checked")
    message("column_combination: Columns-combination that is checked for duplicates")
    message("duplicates: Count of duplicates found in that column combination")
    message("is_level: (Binary) True / False indicating if the combination is the level")
  }

  for (i in seq_len(ncol(dataset))) {
    cat("\n")
    cat(paste0(rep("#", 90), collapse = ""),"\n")
    cat(paste0(i, " COLUMN COMBINATION(S) WILL BE CHECKED FOR LEVEL:", "\n"))
    cat(paste0(rep("#", 90), collapse = ""),"\n")
    column_combinations <- generate_column_combinations(dataset, i)
    check_for_level(dataset, column_combinations)
    cat(stringr::str_interp("Please open a COPY of ${output_filename} for the report generated so far."))
    cat("\n");
  }

  invisible()
}
