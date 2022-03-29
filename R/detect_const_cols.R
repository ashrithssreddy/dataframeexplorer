#' Detect if any column of a data.frame constant values
#'
#' @param dataset A data.frame
#' @return.
#' @export
#' @examples
#' \dontrun{
#' detect_const_cols(dataset = head(mutate(mtcars, mpg_2 =  999)))
#' }

detect_const_cols <- function(dataset, return_type = "col_names"){

  # A vector to collect names of const columns
  const_colnames = which(sapply(dataset, function(x) length(unique(x)))==1) %>% names

  if(return_type == "col_names"){
    return(const_colnames)

  }else if(return_type == "col_positions"){
    return(which(names(dataset) %in% const_colnames))

  }else if(return_type == "dataset"){
    dataset %>% select(-any_of(const_colnames))

  }else{
    error("Invalid value passed for return_type. Use 'col_names', 'col_positions' or 'dataset'")
  }

} # function ends here

# Tests
# dataset = mtcars %>% mutate(mpg = 999, hp = 100) %>% head
# dataset %>% detect_const_cols(return_type = "col_names")
# dataset %>% detect_const_cols(return_type = "col_positions")
# dataset %>% detect_const_cols(return_type = "dataset")

# Next Steps
# Missing values incorporate
# Convert a matrix to a dataframe inside
