#' Detect if any column of a data.frame is a duplicate of another
#'
#' It occasionally happens that 2 (or more) columns in dataframe are exactly identical.
#' This could lead to redundant computational cost and unexpected behavior in Machine Learning methods.
#' This function scans though all column combinations of dataframe to examine if any 2 columns are exactly identical.
#'
#' @param dataset A data.frame
#' @param duplicate_col If 2 columns are identical, which column should be treated as duplicate?
#' Use "right" for right column, "left" for left.
#' @return A vector of duplicate column names.
#' @export
#' @importFrom magrittr %>%
#' @importFrom dplyr select, all_of, last_col
#' @examples
#' \dontrun{
#' detect_dupl_cols(dataset = head(mutate(mtcars, mpg_2 =  mpg)), duplicate_col = "right")
#' }

rm(list=ls())
cat("\014")

detect_dupl_cols <- function(df, duplicate_col = "right"){

  # A vector to collect names of duplicated columns
  duplicate_cols = NULL

  for(col_1 in df %>% select(1:last_col(1)) %>% names()){

    # Check col_1 is duplicated with any of the columns to its right
    for(col_2 in df %>% select(all_of(col_1):last_col()) %>% names()){

      # Don't treat a column as a duplicate of itself, obviously!
      if(col_1 == col_2){
        next
      }

      # If col_1 and col_2 are exactly identical, collect column name.
      if(all(df[,col_1] == df[,col_2])){
        if(duplicate_col == "right"){
          duplicate_cols = c(duplicate_cols, col_2)
        }else if(duplicate_col == "left"){
          duplicate_cols = c(duplicate_cols, col_1)
        }else{
          message("Invalid value passed to duplicate_col parameter (use 'left' or 'right'). Using default value of 'right' for now")
          duplicate_cols = c(duplicate_cols, col_2)
        }
      }
    } # col_2 for loop ends here
  } # col_1 for loop ends here
  return(duplicate_cols)
} # function ends here

df = data.table::fread("99_junk/mtcars_dups.csv") %>%
  head %>%
  setDF()
set.seed(2022)
df = df[,sample(names(df)[1:ncol(df)], ncol(df))]

df %>%
  detect_dupl_cols()

#### Stray Codes ####
# df = mtcars %>%
#   dplyr::mutate(mpg_2 = mpg) %>%
#   head

# print(col_2)
# print(col_1)
# paste(col_1, col_2) %>% print
# print("lol")
# isTRUE(all.equal(df[,..col_1], df[,..col_2]))


# mtcars %>% data.table::fwrite("99_junk/mtcars_dups.csv")


# df = df %>% data.table
# col_combinations = expand.grid(x = names(df), y = names(df)) %>%
#   dplyr::filter(x!=y) %>%
#   arrange(x)

# col_combinations$x
# col_combinations$y


# Next steps
# avoid checking duplicates of already detected duplicate column
# docstrings
# Return positions of columns instead of column names
# comments

