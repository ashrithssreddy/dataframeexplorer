#' Detect if any column of a data.frame is a duplicate of another
#'
#' It occasionally happens that 2 (or more) columns in dataframe are exactly identical.
#' This could lead to redundant computational cost and unexpected behavior in Machine Learning methods.
#' This function scans though all column combinations of dataframe to examine if any 2 columns are exactly identical.
#'
#' @param dataset A data.frame
#' @param return_type How to return detected duplicate columns
#' Use "col_names", "col_positions" or "dataset" to return dataset with deleted duplicate columns
#' @param duplicate_col If 2 columns are identical, which of the 2 columns should be treated as duplicate?
#' Use "right" for right column, "left" for left.
#' @return A vector of duplicate column names or column positions or dataset with deleted duplicate columns. Use return_type parameter to specify.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select all_of any_of last_col
#' @examples
#' \dontrun{
#' detect_dupl_cols(dataset = head(mutate(mtcars, mpg_2 =  mpg)), duplicate_col = "right")
#' }

detect_dupl_cols <- function(dataset, return_type = "col_names", duplicate_col = "right"){

  # Check/fix inputs
  if("matrix" %in% class(dataset) == T){
    warning("matrix was provided when data.frame was expected. Attempting to convert to data.frame")
    dataset = dataset %>% as.data.frame
  }
  if("data.frame" %in% class(dataset) == F)
    stop("class of dataset must be a data.frame")
  if(return_type %in% c("col_names", "col_positions", "dataset") == F)
    stop('return_type must be "col_names", "col_positions" or "dataset". Or simply use default value')
  if(duplicate_col %in% c("left", "right") == F)
    stop('duplicate_col must be "left" or "left". Or simply use default value')
  dataset = dataset %>% as.data.frame() # forcing data.frame like objects to data.frame only

  # A vector to collect names of duplicated columns
  duplicate_cols = NULL

  for(col_1 in dataset %>% select(1:last_col(1)) %>% names()){

    # Don't check for duplicate of already detected duplicate columns, since its duplicates would have been detected already
    if(col_1 %in% duplicate_cols){
      next
    }

    # Check col_1 is duplicated with any of the columns to its right
    for(col_2 in dataset %>% select(all_of(col_1):last_col()) %>% names()){

      # Don't check for duplicate of already detected duplicate columns, since its duplicates would have been detected already
      if(col_2 %in% duplicate_cols){
        next
      }

      # Don't treat a column as a duplicate of itself, obviously!
      if(col_1 == col_2){
        next
      }

      # If col_1 and col_2 are exactly identical, collect column name.
      if(all(dataset[,col_1] == dataset[,col_2])){
        if(duplicate_col == "right"){
          duplicate_cols = c(duplicate_cols, col_2)
        }else if(duplicate_col == "left"){
          duplicate_cols = c(duplicate_cols, col_1)
        }
      }
    } # col_2 for loop ends here
  } # col_1 for loop ends here

  if(return_type == "col_names"){
    if(is.null(duplicate_cols)){
      message("No duplicate columns found. Returning NULL.")
      return(NULL)
    }
    return(duplicate_cols)
  }else if(return_type == "col_positions"){
    if(is.null(duplicate_cols)){
      message("No duplicate columns found. Returning NULL.")
      return(NULL)
    }
    return(which(names(dataset) %in% duplicate_cols))
  }else if(return_type == "dataset"){
    if(is.null(duplicate_cols)){
      message("No duplicate columns found. Returning original dataset.")
      return(dataset)
    }
    return(dataset %>% select(-any_of(duplicate_cols)))
  }

} # function ends here
