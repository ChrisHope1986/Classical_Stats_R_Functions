# funciton to run classical analysis
#     ld = list of three long format dataframes
#           a person dataframe
#           a person by item dataframe
#           and a person by question dataframe

#     anal_spec = character value of a directory to an analysis specification excel file

#     wb = directory to an excel workbook

#   op = character value of a directory to create folders and
#         excel file output to.

# mapping_list = a list of dataframes to names in the output dataframe to new names
#               to make them more human readable.

# theme = character value of the colour theme for formatting, default is "grey", can take the values
#         of ....
# header_names = dataframe of the header names to replace the R names with the final output
#               names for the headers.


classical_stats <- function(
  ld,
  anal_spec,
  wb,
  op,
  mapping_list,
  theme = "grey",
  header_names
){
  
  fd01 <- list(
    var_def = read_xlsx(
      anal_spec,
      "variable def"
    ),
    qi_mapping = read_xlsx(
      anal_spec,
      "items - question mapping"    
    )
  )
  
  fd01b <- read_filter_spec(
    anal_spec
  )
  
  # select the correct colour fomatting from the lists stored as part of the
  # package data
  
  sel_fm_banding_fill <- fm_banding_fill[[theme]]
  sel_fm_col_name <- fm_col_name[[theme]]
  sel_fm_header <- fm_header[[theme]]
  
  # join on the booklets to the questions
  # data
  
  # map the items onto the questions and then map 
  # the booklets onto the mapped items
  # then remove the item data from the question data
  # then arrange the data by child id and then by question
  
  ldq <- ld$person_q
  
  ldq_02 <- inner_join(
    ldq,
    fd01$qi_mapping,
    by = "question"
  )
  
  ldq_03 <- inner_join(
    ldq_02,
    select(
      ld$person_i,
      item,
      booklet
    ) %>% unique(),
    by = c("items" = "item")
  ) %>% arrange(
    !!sym(
      filter(
        fd01$var_def,
        `variable type` == "person uid"
      ) %>% pull(`variable name`)
    ),
    question
  ) %>% select(
    -items
  )
  
  
  ld$person_q <- ldq_03
  
  fd02 <- run_classical(
    ld,
    fd01,
    fd01b
  )
  
  # format the analysis into a format the can be outputted to an
  # excel workbook
  fd03 <- run_sc_output(
    fd02,
    sel_fm_banding_fill,
    sel_fm_col_name
  )
  
  # get the workbook for doing the analysis
  # wb <- loadWorkbook(
  #   "C:/Work/SOLUTIONS/Garage/02 Meth 25 - classical/08 March 2021/classical stats.xlsm"
  # )
  
  # create an evnrionment here that the workbook is loaded into
  # the active bindings are then not associated with a workbook with the same name
  # elsewhere.
  # env <- new.env()
  # env$wb <- loadWorkbook(
  #   wb
  # )
  
  # print to the necessary sheets
  output_split_classical(
    wb,
    fd03,
    op,
    ld,
    mapping_list,
    header_names,
    sel_fm_header
  )
  
  return(fd02)
  
}