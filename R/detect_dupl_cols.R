rm(list=ls())
cat("\014")

detect_dupl_cols <- function(df){

  duplicate_cols = NULL
  for(col_1 in df %>% select(1:last_col(1)) %>% names()){
    for(col_2 in df %>% select(all_of(col_1):last_col()) %>% names()){

      if(col_1 == col_2){
        next
      }

      if(sum(df[,col_1] != df[,col_2]) ==0){
        duplicate_cols = c(duplicate_cols, col_2)
      }
    }
  }
  return(duplicate_cols)
}

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



