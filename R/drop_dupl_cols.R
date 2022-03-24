rm(list=ls())
cat("\014")

drop_dupl_cols <- function(df, duplicate_cols){
  df %>% 
    dplyr::select(-all_of(duplicate_cols))
}
  
  