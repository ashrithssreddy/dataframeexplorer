#' Detect if any column of a data.frame has constant values.
#'
#' It occasionally happens that a column in dataframe contains a single value throughout.
#' This could lead to redundant computational cost and unexpected behavior in Machine Learning methods.
#' This function scans though all columns of dataframe to examine if any column has no variation.
#'
#' @param dataset A data.frame
#' @param return_type How to return detected constant columns
#' Use "col_names", "col_positions" or "dataset" to return dataset with deleted constant columns
#' @param ignore_na Whether NA should be ignored while checking if a column has just 1 unique value
#' @return. A vector of constant column-names or column positions or dataset with deleted constant columns. Use return_type parameter to specify.
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom dplyr select any_of
#' @examples
#' \dontrun{
#' detect_const_cols(dataset = head(mutate(mtcars, mpg_2 =  999)))
#' }

detect_const_cols <- function(dataset, return_type = "col_names", ignore_na = F){

  # Check/fix inputs
  if("matrix" %in% class(dataset) == T){
    warning("matrix was provided when data.frame was expected. Attempting to convert to data.frame")
    dataset = dataset %>% as.data.frame
  }
  if("data.frame" %in% class(dataset) == F)
    stop("class of dataset must be a data.frame")
  if(return_type %in% c("col_names", "col_positions", "dataset") == F)
    stop('return_type must be "col_names", "col_positions" or "dataset". Or simply use default value')
  dataset = dataset %>% as.data.frame() # forcing data.frame like objects to data.frame only

  # A vector to collect names of const columns
  const_colnames = which(sapply(dataset, function(x)
    ifelse(ignore_na, length(unique(na.omit(x)))==1,length(unique(x))==1))) %>% names

  # Return results according to return_type parameter
  if(return_type == "col_names"){
    return(const_colnames)

  }else if(return_type == "col_positions"){
    return(which(names(dataset) %in% const_colnames))

  }else if(return_type == "dataset"){
    return(dataset %>% select(-any_of(const_colnames)))
  }

} # function ends here

# Tests
# dataset = mtcars %>% head %>% mutate(mpg = 999, hp = c(100,NA,100,NA, 100, NA))
# dataset = mtcars %>% head %>% mutate(mpg = 999, hp = 100)
# dataset %>% detect_const_cols(return_type = "col_names")
# dataset %>% detect_const_cols(return_type = "col_positions")
# dataset %>% detect_const_cols(return_type = "dataset")
# dataset %>% detect_const_cols(return_type = "blah-blah")
# dataset %>% detect_const_cols(ignore_na = T)
