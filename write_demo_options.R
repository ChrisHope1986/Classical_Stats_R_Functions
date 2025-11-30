# function to write the demographic variable options
# as tables to the analysis specification template and
# then save this out as a workbook for the user to edit.

# ld = list of 3 dataframes of assessment data in long format
#       names of the dataframes should be person, person_i and person_q
# demo_vars = demographic variable names to be given as options for the
#             user to select
# wb = character value of the analysis specification template to read in.
# op = character value of analysis specification file to output.

# returns the data written to the analysis template as a list of dataframes.

write_demo_options <- function(
  ld,
  demo_vars,
  wb,
  op
){
  
  # create dataframes to be written as tables
  # to the template analysis specification workbook
  fd01 <- anal_spec_demo_options(
    ld,
    demo_vars
  )
  
  # read in template analysis specification workbook
  fd02 <- loadWorkbook(
   wb
  )
  
  # print dataframes to the template workbook 
  print_asdo(
    fd01,
    fd02
  )
  
  saveWorkbook(
    fd02,
    op,
    overwrite = T
  )
  
  return(
    fd01
  )
  
}