library(tidyverse)
library(haven)
library(openxlsx)
library(rJava)
library(XLConnect)
library(readxl)
library(weights)
# requires weights
# radiant.data
# spatstat

# d01 <- read_sav(
#   "K:/LENT/Research Department/CfS/Scoring File/01 Maths/Arithmetic/ArithmeticScoring_UPDATED23122020.sav"  
# )

# read in the main classical statistics function

source(
  './print_asdo.R'
)

source(
  "./classical main.R"
)

# load in the file formatting functions
source(
  "./file formatter main.R"
)
source(
  "./gatherer main.R"
)
source(
  "./gatherer.R"
)

# load in the analysis functions
source(
  "./overview_stats.R"
)

# item stats
source(
  "./item stats_fast.R"
)

# distractor analysis
source(
  "./distractor analysis.R"
)

# cumulative statistics
source(
  "./cumulative stats.R"
)

# DIF (Classical differential item functioning)
source("./dif.R")


source(
  "./ld_splitter.R"  
)

source(
  "./split_spec_parse.R"
)

source(
  "./organise_split.R"  
)

source(
  "./rows_split_classical.R"  
)

source(
  "./elems_split_classical.R"  
)

source(
  "./output_split_classical.R"  
)

source(
  "./run_split_classical.R"  
)

source(
  "./run_sc_output.R"
)

source(
  './booklet_info.R'
)

source(
  './data_printer.R'
)

source('./run_classical.R')

source('./cronbach_alpha.R')

source('./weighted_var.R')

source('./anal_spec_demo_options.R')

source('./write_demo_options.R')

source('./read_filter_spec.R')

source('./filter table parse.R')

source('./get_output_names.R')
       
source('./rename_output.R')

source('./classical_stats.R')

source(
  "./src_formatting/exp_run.R"  
)

source(
  "./src_formatting/run_fomat_spec.R"
)


# read in formatting functions
source(
  "./src_formatting/df_expander.R"
)

source(
  "./src_formatting/df_exploder.R"
)
  
source(  
  "./src_formatting/row_expander.R"
)

source(
  "./src_formatting/split_fm_df.R"
)

source(
  "./src_formatting/fm_obj_applier.R"
)

source(
  "./dup_col.R"
)


# load in the fomatting data
load(
"./input/formatting/background fill banding/fm_banding_fill.RData"
)
load(
"./input/formatting/column_name/fm_col_name.RData"
)
load(
"./input/formatting/header/fm_header.RData"
)
load(
"./input/formatting/text properties/fm_txt_prop.RData"
)

##  ##### #######

#     Broken out workflow

###   ####  ######

# get the data in in the right format

# format file
d01 <- file_formatter_main("./01 wtl spec.xlsx")
# write the demographic options to the analysis specification

# only need to write these if they are specified by the user 
# otherwise can just use the analysis specification as it is. template analysis specification

d02 <- write_demo_options(
  d01,
  c("gender", "ethnic", "homelanguage"),
  "./analysis spec 2.xlsx",
  "./analysis spec options.xlsx"
)

d03 <- get_output_names(
  "./input/rename output columns.xlsx"
)


d04 <- read_xlsx(
    "./input/rename headers.xlsx"
  )


# # set up the fill colour of the banding
# fm_banding_fill <- tibble(
#   banding = c(1,0),
#   prop_type = "fgFill",
#   prop_val = c(
#     "#ffffff",
#     "#cccccc"
#   )
# )
# 
# # set up the text formatting of the cells
# d04 <- "C:\Work\SOLUTIONS\Garage\02 Meth 25 - classical\2021 April 22nd/input/formatting/text properties/text formatting.xlsx"
# 
# fmt_txt_prop <- list(
#   `Test statistics` = list(
#     rg_size = 6,
#     fmt_df = read_xlsx(
#       d04,
#       "overview_stat"
#     )
#   ),
#   `Item stats` = list(
#     rg_size = 1,
#     fmt_df = read_xlsx(
#       d04,
#       "item_stats"
#     )
#   )
# )


# run the analysis
ls_classical <- classical_stats(
  d01,
  "./analysis spec options.xlsx",
  "./classical stats.xlsm",
  "./output",
  d03,
  theme = "purple",
  d04
)




#### OLD  code

# individual test

# get the analysis specification
d04 <- list(
  var_def = read_xlsx(
    "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/2021 April 16th/analysis spec options.xlsx",
    "variable def"
  ),
  qi_mapping = read_xlsx(
    "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/2021 April 16th/analysis spec options.xlsx",
    "items - question mapping"    
  )
)

d05 <- read_filter_spec(
  "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/2021 April 16th/analysis spec options.xlsx"
)


# lapply over long data repeatedly filtering and analysing.

# need to create a new total score each time witin the new function based on the long format data
# going in for each of the persons.

d06 <- run_classical(
  d01,
  d04,
  d05
)

d07 <- run_sc_output(
  d06
)

# transform the filter spec into a usable list to filter the data


# create the analysis specification


# test <- read.xlsx(
#   "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/10 March 2021/a_spec_options.xlsx",
#   namedRegion = "Table1"
# )

# wb <- XLConnect::loadWorkbook(
#   "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/10 March 2021/a_spec_options.xlsx"
# )
# 
# test <- XLConnect::readTable(
#   wb,
#   sheet = "demo_options_data",
#   table = "Table1"
# )

wb <- loadWorkbook(
  "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/2021 April 16th/analysis spec 2.xlsx"
)

# print the data to a workbook as tables


openXL(wb)




# the user edits the filter option = they come by default as 1
# the user can put a 0 if they want to remove a level and have
# only a selection.

# the application then reads the data back into the workbook and
# reads the selection in as a list of values to do the filtering.

wb <- loadWorkbook(
  "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/10 March 2021/a_spec_options.xlsx"
)

test <- getTables(
  wb,
  sheet = "demo_options_data"
)

names(test) <- test

# create the analysis specification

wb2 <- XLConnect::loadWorkbook(
  "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/10 March 2021/a_spec_options.xlsx"
)

test2 <- lapply(
  test,
  function(x){
    XLConnect::readTable(
      wb2,
      "demo_options_data",
      x
    ) 
  }
)



# read in the analysis specification as a list of dataframes to perform the analysis by.
d01c <- list(
  var_def = read_xlsx(
    "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/10 March 2021/analysis spec 2.xlsx",
    "variable def"
  ),
  qi_mapping = read_xlsx(
    "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/10 March 2021/analysis spec 2.xlsx",
    "items - question mapping"    
  )
)







# saveWorkbook(wb, "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/08 March 2021/output/test.xlsm", overwrite = TRUE)

# save out the workbook










# d06 <- output_split_classical(
#   wb,
#   d05
# )
# 
# openXL(wb)
# 
# # loop over the first element then the second
# # the the third
# 
# 
# writeData(wb, names(d05[1]), d05[[1]]$`gender _ F`$header$data, startCol = 1, startRow = d05[[1]]$`gender _ F`$header$row)
# 
# writeData(wb, names(d05[1]), d05[[1]]$`gender _ F`$data$data, startCol = 1, startRow = d05[[1]]$`gender _ F`$data$row)
# 
# names(d05[1])
# 
# 
# 
# # create function to print out the data to a workbook
# 
# 
# 
# # create list ouput from row_split
# 
# 
# 
# # create  list of element to be outputted
# 
# 
# 
# # if not specified then the function needs to go into the person data and find the levels
# 
# # take the string and the data to be able to find the factor levels
# 
# # loop over to searching for the parentheses
# 
# # test <- c(
# #   "gender",
# #   "FluencyinEnglish",
# #   "homelanguage"  
# # )
# 
# 
# # pass in the ld_person into the function
# # form the master function.
# 
# # ld_person <- d01$person
# # 
# # split_ls  <- split_spec_parse(
# #   "gender|homelanguage(English, Polish)",
# #   ld_person
# # )
# # 
# # split_ls$whole_data = list(
# #   var = NULL,
# #   level = NULL
# # )
#   
# 
# 
# # arithmetic
# # split_ls <- list(
# #   sex_F = list(
# #     var = "sex",
# #     level = "F"
#   ),
#   sex_M = list(
#     var = "sex",
#     level = "M"
#   ),
#   FSMYorN_Y = list(
#     var = "FSMYorN",
#     level = "Y"
#   ),
#   FSMYorN_N = list(
#     var = "FSMYorN",
#     level = "N"
#   ),
#   whole_data = list(
#    var = NULL,
#    level = NULL
#   )
# )
# 
# d02 <- ld_splitter(
#   d01,
#   split_ls[["whole_data"]],
#   d01c
# )

# lapply over each of the split list filter elements
# d03 <- lapply(
#   split_ls,
#   function(x){
#     
#     fd01 <- ld_splitter(
#       d01,
#       x,
#       d01c
#     )
#     
#     return(
#       fd01
#     )
#     
#   }
# )

# add output directories onto the list of dataframes
# op_dir <- "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/02 March 2021/output"
# 
# d04 <- list(
#   sex_F = list(
#     data = d03$sex_F,
#     output = "sex_F"
#   ),
#   sex_M = list(
#     data = d03$sex_M,
#     output = "sex_M"
#   ),
#   FSMYorN_Y = list(
#     data = d03$FSMYorN_Y,
#     output = "FSMYorN_Y"
#   ),
#   FSMYorN_N = list(
#     data = d03$FSMYorN_N,
#     output = "FSMYorN_N"
#   ),
#   whole_data = list(
#     data = d03$whole_data,
#     output = "whole_data"
#   )
# )

# create function to first create the folder specified by the output
# string and then run the classical output to put the output there

# for(
#   i in d04
# ){
#   
#   # create directory
#   dir.create(
#     paste0(op_dir,"/",i$output)
#   )
#   
#   # run classical analysis
#   classical(
#     ld = i$data,
#     a_spec = d01c,
#     wb = wb,
#     op = paste0(op_dir,"/",i$output,"/", i$output,".xlsx")
#   )
#   
# }

# lapply over the data passing in the analysis spec as the only other function needed

# set all facility values to zero if they are NA
# d04 <- lapply(
#   d03,
#   function(x){
#     fd01 <- x
#     fd01$person_i <- mutate(
#       fd01$person_i,
#       item_facility_value = ifelse(
#         is.na(item_facility_value),
#         0,
#         item_facility_value
#       )
#     )
#     
#     return(fd01)
#   }
# )


# d05 <- lapply(
#   d04,
#   function(x){
#     classical(
#       x,
#       d01c
#     )
#   }
# )









# improve item stats so it runs faster

# classical(
#   ld = d01,
#   a_spec = d01c,
#   wb = wb,
#   op = "K:/LENT/Research Department/CfS/FastItems/01 Chris R fastitems/output/test ouput.xlsx"
# )

# create a function to split out the data by the different identifiers in the anlaysis
# by gender and ethnicity and then run all the analysis if there is a NULL vlaue for the variables
# to split by


# gender fsm
# whole sample



# need to create different file formatters
# need to filter the data set each time

# filter each of the long format data frames

# each paper has different items and questions anyway






