# main function to organise the output for
# printing to excel

# parameters
#   sc_output = list output of the run_split_classical function 
#   sel_fm_banding_fill = selected formatting options for the dataframe bands when
#                         printed to the excel file.
#   sel_fm_col_name = selected formatting options for the column names when printed
#                     to the excel file
run_sc_output <- function(
  sc_output,
  sel_fm_banding_fill,
  sel_fm_col_name
){
  
  # need to create the list with a string
  fd01 <- organise_split(
    sc_output
  )
  
  # find the rows that each
  # dataframe will be outputted to
  fd02 <- rows_split_classical(
    fd01,
    sel_fm_banding_fill,
    sel_fm_col_name
  )
  
  # fd03 <- elems_split_classical(
  #   fd01,
  #   fd02
  # )
  
  return(
    fd02
  )
  
}