# duplicate columns
# function to duplicate the master column of the wide to long format
# spreadsheet to save the user having to write out the same column
# values twice

# df = dataframe with columns that need duplicating
# src_col = the first master column in the wide to long format spreadsheets
#           either person by question level or person by item level
# copy_col = the name of the column which is a duplicate of the src_col

dup_col <- function(
  df,
  src_col,
  copy_col
){
  
  t1 <- df
  
  t2 <- names(t1)
  
  t3 <- 1:length(t1)
  
  # get just the first column
  t4 <- select(
    t1,
    !!sym(src_col)
  )
  
  # select just the first master column
  t5 <- tibble(
    !!sym(copy_col) := t1[[src_col]]
  ) 
  
  # select the columns after the first
  # master column

  t6 <- select(
    t1,
    !!!syms(
      t2[t3>=2]
    )
  )
  
  # put the dataframe back together again
  t7 <- bind_cols(
    t4,
    t5,
    t6
  )
  
  return(t7)
  
}
  





